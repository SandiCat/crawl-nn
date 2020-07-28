module Lib where

import qualified URI.ByteString as URI
import qualified URI.ByteString.QQ as URI
import URI.ByteString (URIRef, URI)
import qualified Streamly as S
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as S.Fold
import qualified Streamly.Memory.Array as S.Array
import qualified Streamly.FileSystem.Handle as S.Handle
import qualified Streamly.Data.String as S.Str
-- import Control.Lens.TH (makeLenses)
import qualified Network.HTTP.Req as Req
import Network.HTTP.Req ((/~), (/:), (=:))
import qualified Control.Retry as Retry
import qualified Path
import qualified Path.IO as Path
import Path ((</>), (<.>))
import qualified Text.HTML.Scalpel as Scalpel
import Text.HTML.Scalpel ((//), (@:), (@=), (@=~))
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson.Text
import System.IO (openFile)
import qualified Data.ByteString.Char8 as BS
import qualified Control.Exception as Exception
import qualified Control.Monad.Catch as Catch
import qualified Text.StringLike as SL

newtype Year = Year { unYear :: Int } deriving (Eq, Ord, Show)
newtype Page = Page { unPage :: Int } deriving (Eq, Ord, Show)

data SearchPage = SearchPage { year :: Year, page :: Page } deriving (Eq, Ord, Show)

resultsPerPage = 200

baseSearchPage :: URI
baseSearchPage = [URI.uri|https://narodne-novine.nn.hr/search.aspx?sortiraj=4&kategorija=1&rpp=200&qtype=2&pretraga=da|]

searchPageUri :: SearchPage -> URI
searchPageUri (SearchPage (Year year) (Page page)) =
    baseSearchPage { URI.uriQuery = pageAndTime <> baseQuery}
    where
        baseQuery = URI.uriQuery baseSearchPage
        pageAndTime = URI.Query
            [ ("od", [i|01.01.#{year}|])
            , ("do", [i|31.12.#{year}|])
            , ("str", show page)
            ]

reqParamsFromURI :: URI -> Maybe (Req.Url 'Req.Https, Req.Option a)
reqParamsFromURI =
    Req.parseUrlHttps . URI.serializeURIRef'

data OnFail = Retry | DontRetry

download :: OnFail -> URI -> IO ByteString
download onFail uri = 
    case (reqParamsFromURI uri) of
        Just (url, options) ->
            Req.runReq
                (case onFail of
                    Retry ->
                        Req.defaultHttpConfig
                            { Req.httpConfigRetryPolicy = Retry.retryPolicy $ const $ Just 5000 
                            , Req.httpConfigRedirectCount = 50
                            }
                    DontRetry -> Req.defaultHttpConfig
                )
            $  Req.responseBody
            <$> Req.req
                Req.GET
                url
                Req.NoReqBody
                Req.bsResponse
                options
        Nothing ->
            fail "invalid uri to url conversion"

a @. b = a @: [Scalpel.hasClass b]

scrapeResultCount :: ByteString -> Int
scrapeResultCount body = fromMaybe 0 $ Scalpel.scrapeStringLike body $ do -- TODO: error managment
    x <- Scalpel.innerHTML $ "div" @. "total-results" // "span"
    let Just count = parseResCount x
    return count
    where
        parseResCount :: ByteString -> Maybe Int
        parseResCount = readMaybe . (takeWhile Char.isDigit) . decodeUtf8

mapFst f (a, b) = (f a, b)
mapSnd f (a, b) = (a, f b)


dirToSearchPage :: Path.Path a Path.File -> Maybe SearchPage
dirToSearchPage dir = do
    let filename = Path.toFilePath $ Path.filename dir
    year <- readMaybe $ take 4 filename
    page <- readMaybe $ takeWhile Char.isDigit $ drop 5 filename
    return (SearchPage (Year year) (Page page))

downloadAllSearches :: Path.Path a Path.Dir -> [Year] -> IO ()
downloadAllSearches path years = do
    (_, downloadedFiles) <- Path.listDir path
    let alreadyDownloaded :: Set SearchPage = fromList $ catMaybes $ map dirToSearchPage $ downloadedFiles
    print alreadyDownloaded

    S.drain
        $ S.asyncly
        $ S.mapM save
        $ S.maxRate 0.5
        $ S.concatMapWith S.wAsync
            (\(year, body) ->
                S.cons (SearchPage year $ Page 0, body)
                $ S.mapM (\sp -> do
                    body <- download Retry $ searchPageUri sp
                    return (sp, body))
                $ S.filter ((flip Set.notMember) alreadyDownloaded)
                $ S.map (SearchPage year)
                $ resultCountToRestOfPages
                $ scrapeResultCount body
            )
        $ S.mapM (\year -> do
                body <- download Retry $ searchPageUri $ SearchPage year (Page 0)
                return (year, body)
            )
        $ S.fromList years

    where
        resultCountToRestOfPages n =
            S.map Page $ S.fromList [1.. (\up -> up - 1) $ ceiling $ (fromIntegral n) / (fromIntegral resultsPerPage)]
        
        save ((SearchPage (Year year) (Page page)), body) = do
            putStrLn [i|#{year} #{page}|]
            let Just file = Path.parseRelFile [i|#{year}_#{page}.html|]
            writeFileBS (Path.toFilePath $ path </> file) body

scrapeArticleLinks :: ByteString -> [URIRef URI.Relative]
scrapeArticleLinks body =
    ("div" @. "searchListItem" // "div" @. "resultTitle" // "a")
    & Scalpel.attrs "href"
    & Scalpel.scrapeStringLike body
    & fromMaybe []
    & map (URI.parseRelativeRef URI.laxURIParserOptions)
    & \ls -> do
        Right x <- ls
        return x

writeStream :: Path.Path a Path.File -> S.Serial Char -> IO ()
writeStream file stream = do
    fh <- openFile (Path.toFilePath file) WriteMode
    S.fold (S.Handle.write fh) $ S.Str.encodeChar8 stream

instance Aeson.ToJSON (URIRef URI.Relative) where
    toJSON = Aeson.String . decodeUtf8 . URI.serializeURIRef'

instance Aeson.FromJSON (URIRef URI.Relative) where
    parseJSON (Aeson.String str) =
        case URI.parseRelativeRef URI.laxURIParserOptions $ encodeUtf8 str of 
            Left err -> fail $ show err
            Right uri -> return uri
    parseJSON _ =
        fail "not string"

streamEncodeArray :: (S.MonadAsync m, Aeson.ToJSON a) => S.SerialT m a -> S.SerialT m Char
streamEncodeArray =
    S.cons '['
    . (flip (<>)) (S.yield ']')
    . S.concatMap (S.fromList . toString)
    . S.intersperse ","
    . S.map Aeson.Text.encodeToLazyText

allArticleLinks :: Path.Path a Path.Dir -> Path.Path a Path.File -> IO ()
allArticleLinks searchPagesFolder outputFile =
    S.yieldM (Path.listDir searchPagesFolder)
    & S.concatMap (S.fromList . snd)
    & S.mapM (readFileBS . Path.toFilePath)
    & S.concatMap (S.fromList . scrapeArticleLinks)
    & S.parallely
    & streamEncodeArray
    & writeStream outputFile

-- /clanci/sluzbeni/1990_03_13_201.html -> /clanci/sluzbeni/full/1990_03_13_201.html
toFullArticleUri :: URIRef URI.Relative -> Maybe URI
toFullArticleUri uri =
    case BS.split '/' $ URI.rrPath uri of 
        [a, b, c, d] ->
            Just $ URI.toAbsolute (URI.Scheme "https")
            $ uri
                { URI.rrPath = mconcat $ intersperse "/" [a, b, c, "full", d]
                , URI.rrAuthority = Just 
                    $ URI.Authority
                        Nothing
                        (URI.Host "narodne-novine.nn.hr")
                        Nothing
                }
        _ ->
            Nothing

uriToFilename :: URI -> Maybe (Path.Path Path.Rel Path.File)
uriToFilename uri =
    (Path.parseRelFile . decodeUtf8) =<< (viaNonEmpty last $ BS.split '/' $ URI.uriPath uri)

uriRelToFilename :: URIRef URI.Relative -> Maybe (Path.Path Path.Rel Path.File)
uriRelToFilename uri =
    (Path.parseRelFile . decodeUtf8) =<< (viaNonEmpty last $ BS.split '/' $ URI.rrPath uri)

downloadArticles :: Path.Path a Path.File -> Path.Path a Path.Dir -> IO ()
downloadArticles inputJson articlesDir = do
    (_, downloadedFiles) <- Path.listDir articlesDir
    let alreadyDownloaded = fromList $ map Path.filename downloadedFiles :: Set (Path.Path Path.Rel Path.File)
    print alreadyDownloaded

    S.yieldM (readFileLBS $ Path.toFilePath inputJson)
        & S.concatMap (S.fromList . fromMaybe [] . Aeson.decode)
        & S.serially
        & S.filter (maybe False ((flip Set.notMember) alreadyDownloaded)  . uriRelToFilename)
        & S.parallely
        & S.mapM downloadAndSave 
        & S.maxRate 10
        & S.asyncly
        & S.drain
    where
        downloadAndSave :: URIRef URI.Relative -> IO ()
        downloadAndSave loadedUri =
            Catch.handleAll (\e -> putStrLn [i|exception #{e} while downloading #{loadedUri}|]) $ do
                case (toFullArticleUri loadedUri, uriRelToFilename loadedUri) of
                    (Just uri, Just filename) -> do
                        body <- download DontRetry uri
                        let fullPath = articlesDir </> filename
                        print filename
                        writeFileBS (Path.toFilePath fullPath) body
                    _ -> putStrLn $ "error on " <> show loadedUri

extractDoc :: ByteString -> Maybe ByteString
extractDoc body =
    ("div" @. "doc")
    & Scalpel.innerHTML
    & Scalpel.scrapeStringLike body

{--
instance SL.StringLike (S.SerialT Identity Char)

extractDocStream :: S.SerialT Identity Char -> Maybe (S.SerialT Identity Char)
extractDocStream body =
    ("div" @. "doc")
    & Scalpel.innerHTML
    & Scalpel.scrapeStringLike body
--}

stripArticles :: Path.Path a Path.Dir -> Path.Path a Path.Dir -> IO ()
stripArticles fromDir toDir = do 
    (_, cachedFiles) <- Path.listDir toDir
    let cached = fromList $ map Path.filename cachedFiles :: Set (Path.Path Path.Rel Path.File)
    print cached
    
    S.yieldM (Path.listDir fromDir <&> snd)
        & S.maxBuffer 10
        -- & S.maxThreads 4
        & S.concatMap (S.fromList)
        & S.filter ((flip Set.notMember) cached . Path.filename)
        & S.indexed 
        -- & S.serially -- trying to avoid excess concurrency?
        & S.mapM 
            (\(ix, file) -> do 
                docMay <- extractDoc <$> readFileBS (Path.toFilePath file)
                case docMay of
                    Nothing -> putStrLn $ "can't extract " <> show file
                    Just doc -> writeFileBS (Path.toFilePath $ toDir </> Path.filename file) doc
                putStrLn [i|#{ix}: #{file}|]
            )
        -- & S.maxRate 10
        & S.asyncly
        & S.drain
