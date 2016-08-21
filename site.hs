--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
--------------------------------------------------------------------------------

import           System.FilePath    (takeFileName)

import qualified Data.Map           as M
import           Data.List          (intercalate, isSuffixOf)
import           Data.Time.Clock    (UTCTime (..))
import           Data.Time.Format   (TimeLocale, defaultTimeLocale,
                                     formatTime, parseTimeM)

import           Control.Monad      (msum)

--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
    match ("img/*" .||. "fonts/*") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.markdown", "contact.html"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ metadataRoute routeByDate   `composeRoutes`
                setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post-boilerplate.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx =
                    postCtx `mappend`
                    bodyField "description"

            posts <- fmap (take 10) . recentFirst
                        =<< loadAllSnapshots "posts/*" "content"
            renderAtom feedConfig feedCtx posts

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Welcome"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"        `mappend`
    dateField "timestamp" "%FT%T%QZ"    `mappend`
    defaultContext

routeByDate :: Metadata -> Routes
routeByDate metadata = customRoute $ \id' ->
    let path        = toFilePath id'

        fileName    = intercalate "-" $ drop 3 $ splitAll "-"
                    $ takeFileName path

        locale      = defaultTimeLocale
        datePath    = formatTime locale "%Y/%m/%d"

    in case getItemUTC' locale path metadata of
        Nothing     -> path
        Just date   -> "posts/" ++ datePath date ++ "/" ++ fileName

getItemUTC' :: TimeLocale       -- ^ Output time locale
           -> FilePath
           -> Metadata
           -> Maybe UTCTime     -- ^ Parsed UTCTime
getItemUTC' locale path metadata = do
    let tryField k fmt = M.lookup k metadata >>= parseTime' fmt
        fn             = takeFileName path

    msum $
        [tryField "published" fmt | fmt <- formats] ++
        [tryField "date"      fmt | fmt <- formats] ++
        [parseTime' "%Y-%m-%d" $ intercalate "-" $ take 3 $ splitAll "-" fn]
  where
    parseTime' = parseTimeM True locale
    formats    =
        [ "%a, %d %b %Y %H:%M:%S %Z"
        , "%Y-%m-%dT%H:%M:%S%Z"
        , "%Y-%m-%d %H:%M:%S%Z"
        , "%Y-%m-%d"
        , "%B %e, %Y %l:%M %p"
        , "%B %e, %Y"
        , "%b %d, %Y"
        ]

-- | From: http://www.dancingfrog.co.uk/posts/2013-08-13-cool-blog-uris
cleanHtmlUrls :: Item String -> Compiler (Item String)
cleanHtmlUrls item = return $ fmap (withUrls clean) item
  where
    idx = "index.html"
    html =".html"

    clean :: String -> String
    clean url
      | idx `isSuffixOf` url = take (length url - length idx) url
      | html `isSuffixOf` url = take (length url - length html) url
      | otherwise = url

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle = "nonsingular"
    , feedDescription = "a blog by Saeid Al-Wazzan"
    , feedAuthorName = "Saeid Al-Wazzan"
    , feedAuthorEmail = "saeid.wazzan@nonsingular.net"
    , feedRoot = "http://wwww.nonsingular.net"
    }

