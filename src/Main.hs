--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>), mconcat)
import Hakyll

main :: IO ()
main = hakyllWith config rules

config :: Configuration
config = defaultConfiguration {
    deployCommand = "s3cmd sync -P --delete-removed _site/ s3://andrewalker.net"
}

rules :: Rules ()
rules = do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "webicons/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- FIXME: how to avoid this? I only want style.css, so the other css files
    -- shouldn't be copied.
    match "css/*" $ do
        route idRoute
        compile $ do
            getResourceBody
                >>= saveSnapshot "raw"

    create ["style.css"] $ do
        route idRoute
        compile $ do
            items <- loadAllSnapshots "css/*" "raw"
            makeItem $ compressCss $ concat $ map itemBody (items :: [Item String])

    match "pages/*" $ do
        route   pagesRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html"    defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts com a tag \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = mconcat
                  [ constField "title" title
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
            let postCtxWithTags = tagsField "tags" tags <> postCtx

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
        (\ids -> return $ paginateEvery 2 $ reverse ids)
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
                  , listField "posts" (postCtx <> paginateCtx) (return posts)
                  , paginateCtx
                  , defaultContext
                  ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

postCtx :: Context String
postCtx = mconcat
    [ teaserField "teaser" "content"
    , dateField "date"       "%B %e, %Y"
    , dateField "date_full"  "%F"
    , dateField "date_dmy"   "%d/%m/%Y"
    , dateField "date_day"   "%d"
    , dateField "date_month" "%b"
    , dateField "date_year"  "%Y"
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

pagesRoute :: Routes
pagesRoute =
    gsubRoute "pages/" (const "") `composeRoutes`
    setExtension "html"
