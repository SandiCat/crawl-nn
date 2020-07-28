import Lib
import Test.HUnit
import qualified Path
import Path ((</>), (<.>))
import qualified URI.ByteString as URI
import qualified URI.ByteString.QQ as URI
import URI.ByteString (URIRef, URI)
import Relude.Unsafe ((!!))

offlineAndOnline :: Path.Path a Path.File -> URI -> (ByteString -> Assertion) -> Test
offlineAndOnline path url test = TestList
    [ TestLabel "offline" $ TestCase $ readFileBS (Path.toFilePath path) >>= test
    , TestLabel "online" $ TestCase $ download url >>= test
    ]

dataFolder = [Path.reldir|test/data|]

main :: IO Counts
main = runTestTT $ TestList 
    [ TestLabel "result count scraping"
        $ offlineAndOnline
            (dataFolder </> [Path.relfile|1990-2014.html|])
            [URI.uri|https://narodne-novine.nn.hr/search.aspx?sortiraj=4&kategorija=1&od=01.01.1990&do=01.01.2014&rpp=200&str=0&qtype=2&pretraga=da|]
        $ \body -> assertEqual "result count" 64561 (scrapeResultCount body)
    , TestLabel "dirToSearchPage" 
        $ TestCase $ assertEqual "2016_11" (Just $ SearchPage (Year 2016) (Page 11)) 
        $ dirToSearchPage [Path.absfile|/path/to/dir/2016_11.html|]
    , TestLabel "article links scraping"
        $ offlineAndOnline
            (dataFolder </> [Path.relfile|2007_19.html|])
            (searchPageUri (SearchPage (Year 2007) (Page 19)))
        $ \body -> do
            let links = scrapeArticleLinks body
            assertEqual "list length" 59 (length $ links)
            assertEqual "10th link" [URI.relativeRef|/clanci/sluzbeni/2007_12_133_3812.html|] (links !! 10)
    , TestLabel "toFullArticle" $ TestCase
        $ assertEqual "" (Just [URI.uri|https://narodne-novine.nn.hr/clanci/sluzbeni/full/1990_03_13_201.html|])
        $ Lib.toFullArticleUri [URI.relativeRef|/clanci/sluzbeni/1990_03_13_201.html|]
    , TestLabel "uriToFilename" $ TestCase
        $ assertEqual "" (Just "1990_03_13_201.html")
        $ Lib.uriToFilename [URI.uri|https://narodne-novine.nn.hr/clanci/sluzbeni/full/1990_03_13_201.html|]
    ]
