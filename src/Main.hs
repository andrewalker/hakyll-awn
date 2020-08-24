--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import qualified Data.Text as DT
import Data.Time.Clock.POSIX
import Hakyll
import Portuguese
import System.FilePath
import qualified Text.DocTemplates as DocT
import Text.Pandoc
import Text.Pandoc.PDF (makePDF)

main :: IO ()
main = hakyllWith config rules

config :: Configuration
config =
  defaultConfiguration
    { deployCommand = "s3_website push"
    }

rules :: Rules ()
rules = do
  staticId <- preprocess getStaticId
  rulesWithStaticId staticId

rulesWithStaticId :: Integer -> Rules ()
rulesWithStaticId staticId = do
  match "keybase.txt" $ do
    route idRoute
    compile copyFileCompiler

  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "assets/webicons/*" $ do
    route $ assetsRoute' "webicons/"
    compile copyFileCompiler

  match "assets/fonts/*" $ do
    route $ assetsRoute' "fonts/"
    compile copyFileCompiler

  match "assets/js/*" $ do
    route $ assetsRoute' "js/"
    compile copyFileCompiler

  match "assets/css/*" $ do
    -- Uncomment the following line for debugging:
    -- route idRoute
    -- And access at http://localhost:8000/assets/css
    compile $ getResourceBody >>= saveSnapshot "raw"

  create ["style.css"] $ do
    route $ assetsRoute' ""
    compile $ do
      items <- loadAllSnapshots "assets/css/*" "raw"
      makeItem $ compressCss $ concat $ map itemBody (items :: [Item String])

  match "cv.md" $ do
    route $ setExtension "pdf"
    compile pdfCompiler

  match "pages/*" $ do
    route pagesRoute
    compile $ do
      pandocCompiler
        >>= loadAndApplyTemplate "templates/page.html" siteCtx'
        >>= loadAndApplyTemplate "templates/default.html" siteCtx'
        >>= relativizeUrls

  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  tagsRules tags $ \tag pattern -> do
    let title = "Posts com a tag \"" ++ tag ++ "\""
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx =
            mconcat
              [ constField "title" title,
                listField "posts" postCtx' (return posts),
                siteCtx'
              ]

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ do
      let postCtxWithTags = tagsField "tags" tags <> postCtx'

      pandocCompiler
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" postCtxWithTags
        >>= loadAndApplyTemplate "templates/default.html" postCtxWithTags
        >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"

      let archiveCtx =
            mconcat
              [ listField "posts" postCtx' (return posts),
                constField "title" "Arquivo",
                field "tagcloud" (\_ -> renderTagCloud 100 250 (sortTagsBy caseInsensitiveTags tags)),
                siteCtx'
              ]

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = bodyField "description" <> postCtx'

      posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"

      renderAtom feedConfiguration feedCtx posts

  blog <-
    buildPaginateWith
      (\ids -> return $ paginateEvery 10 $ reverse ids)
      "posts/*"
      ( \n ->
          if n == 1
            then "index.html"
            else fromCapture "posts/page/*.html" (show n)
      )

  paginateRules blog $ \pageNum pattern -> do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern

      let paginateCtx = paginateContext blog pageNum
      let ctx =
            mconcat
              [ constField "title" "Home",
                listField "posts" (postCtx' <> paginateCtx) (return posts),
                paginateCtx,
                siteCtx'
              ]

      makeItem ""
        >>= loadAndApplyTemplate "templates/index.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "templates/*" $ compile templateCompiler
  where
    postCtx' = postCtx staticId
    siteCtx' = siteCtx staticId
    assetsRoute' = assetsRoute staticId

pdfCompiler :: Compiler (Item LB.ByteString)
pdfCompiler = do
  content <- readPandoc =<< getResourceBody

  res <- unsafeCompiler $
    runIOorExplode $ do
      templ <- compileDefaultTemplate (DT.pack "latex")
      makePDF
        "pdflatex"
        []
        writeLaTeX
        def
          { writerTemplate = Just templ,
            writerVariables = DocT.Context $ M.fromList [(DT.pack "geometry", DocT.toVal $ DT.pack "margin=2cm")]
          }
        (itemBody content)

  case res of
    Right pdf -> makeItem pdf
    Left err -> error $ show err

dateFieldLoc :: String -> String -> Context a
dateFieldLoc = dateFieldWith timeLocalePtBr

postCtx :: Integer -> Context String
postCtx staticId =
  mconcat
    [ teaserField "teaser" "content",
      dateFieldLoc "date" "%e de %B, %Y",
      dateFieldLoc "date_full" "%F",
      dateFieldLoc "date_dmy" "%d/%m/%Y",
      dateFieldLoc "date_day" "%d",
      dateFieldLoc "date_month" "%b",
      dateFieldLoc "date_year" "%Y",
      siteCtx staticId
    ]

siteCtx :: Integer -> Context String
siteCtx staticId =
  mconcat
    [ constField "staticId" (show staticId),
      defaultContext
    ]

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = "andrewalker.net",
      feedDescription = "Lorem ipsum dolor sit amet",
      feedAuthorName = "André Walker",
      feedAuthorEmail = "andre+website@andrewalker.net",
      feedRoot = "https://andrewalker.net"
    }

getStaticId :: IO Integer
getStaticId = round `fmap` Data.Time.Clock.POSIX.getPOSIXTime

assetsRoute :: Integer -> String -> Routes
assetsRoute staticId folder = customRoute $ prefixPlus . takeFileName . toFilePath
  where
    prefix = (show staticId) ++ "/" ++ folder
    prefixPlus = (prefix ++)

pagesRoute :: Routes
pagesRoute =
  gsubRoute "pages/" (const "")
    `composeRoutes` setExtension "html"
