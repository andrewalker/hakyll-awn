--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Time.Clock.POSIX
import Data.Monoid ((<>), mconcat)
import Hakyll
import Portuguese

main :: IO ()
main = hakyllWith config rules

config :: Configuration
config = defaultConfiguration {
    deployCommand = "s3_website push"
}

rules :: Rules ()
rules = do
    deploymentId <- preprocess getDeploymentId

    match "keybase.txt" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "webicons/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- FIXME: how to avoid this? I only want style.css, so the other css files
    -- shouldn't be copied.
    match "css/*" $ do
        route idRoute
        compile $ getResourceBody >>= saveSnapshot "raw"

    create ["style.css"] $ do
        route $ assetsRoute deploymentId
        compile $ do
            items <- loadAllSnapshots "css/*" "raw"
            makeItem $ compressCss $ concat $ map itemBody (items :: [Item String])

    match "pages/*" $ do
        route   pagesRoute
        compile $ do
            let ctx = defaultContext <> constField "deploymentId" (show deploymentId)

            pandocCompiler
                >>= loadAndApplyTemplate "templates/page.html"    ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts com a tag \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = mconcat
                  [ constField "title" title
                  , constField "deploymentId" (show deploymentId)
                  , listField "posts" postCtx (return posts)
                  , defaultContext
                  ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            let postCtxWithTags =
                    tagsField "tags" tags <>
                    constField "deploymentId" (show deploymentId) <>
                    postCtx

            pandocCompiler
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/post.html"    postCtxWithTags
                >>= loadAndApplyTemplate "templates/default.html" postCtxWithTags
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx = mconcat
                  [ listField "posts" postCtx (return posts)
                  , constField "title" "Arquivo"
                  , constField "deploymentId" (show deploymentId)
                  , field "tagcloud" (\_ -> renderTagCloud 100 250 (sortTagsBy caseInsensitiveTags tags))
                  , defaultContext
                  ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = bodyField "description" <> postCtx

            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
            renderAtom feedConfiguration feedCtx posts

    blog <- buildPaginateWith
        (\ids -> return $ paginateEvery 10 $ reverse ids)
        "posts/*"
        (\n -> if n == 1
            then "index.html"
            else fromCapture "posts/page/*.html" (show n))

    paginateRules blog $ \pageNum pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern

            let paginateCtx = paginateContext blog pageNum
            let ctx         = mconcat
                  [ constField "title" "Home"
                  , constField "deploymentId" (show deploymentId)
                  , listField "posts" (postCtx <> paginateCtx) (return posts)
                  , paginateCtx
                  , defaultContext
                  ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

dateFieldLoc :: String -> String -> Context a
dateFieldLoc = dateFieldWith timeLocalePtBr

postCtx :: Context String
postCtx = mconcat
    [ teaserField "teaser" "content"
    , dateFieldLoc "date"       "%e de %B, %Y"
    , dateFieldLoc "date_full"  "%F"
    , dateFieldLoc "date_dmy"   "%d/%m/%Y"
    , dateFieldLoc "date_day"   "%d"
    , dateFieldLoc "date_month" "%b"
    , dateFieldLoc "date_year"  "%Y"
    , defaultContext
    ]

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "andrewalker.net"
    , feedDescription = "Lorem ipsum dolor sit amet"
    , feedAuthorName  = "André Walker"
    , feedAuthorEmail = "andre+website@andrewalker.net"
    , feedRoot        = "https://andrewalker.net"
    }

getDeploymentId :: IO Integer
getDeploymentId = round `fmap` getPOSIXTime

assetsRoute :: Integer -> Routes
assetsRoute id' = customRoute $ ((show id') ++) . toFilePath

pagesRoute :: Routes
pagesRoute =
    gsubRoute "pages/" (const "") `composeRoutes`
    setExtension "html"
