--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Arrow ((&&&))
import           Control.Monad (forM_)
import qualified Data.Map.Strict as M
import           Data.Maybe (maybeToList, fromMaybe)
import           Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Text as T
import           Text.Pandoc
import           Text.Pandoc.Walk
import           System.FilePath
import           Hakyll
import           Hakyll.Web.Sass (sassCompiler)
import           Hakyll.Images (loadImage
                               , compressJpgCompiler
                               , resizeImageCompiler
                               , scaleImageCompiler)
import           Text.Sass.Options ( SassOptions(..)
                                   , defaultSassOptions
                                   , SassOutputStyle(..))
--------------------------------------------------------------------------------
-- config
config :: Configuration
config = defaultConfiguration
  { providerDirectory = "src" 
  , deployCommand = "rsync -avz _site root@okura.at:/var/www/main"
  , ignoreFile = myIgnoreList
  } where
    myIgnoreList path
      | "."    `isPrefixOf` fileName = True
      | "#"    `isPrefixOf` fileName = True
      | "~"    `isSuffixOf` fileName = True
      | ".swp" `isSuffixOf` fileName = True
      | "_"    `isPrefixOf` fileName = True
      | otherwise                    = False
      where
        fileName = takeFileName path

sassOptions :: Maybe FilePath -> SassOptions
sassOptions distPath = defaultSassOptions
  { sassSourceMapEmbed = True
  , sassOutputStyle = SassStyleCompressed
  , sassIncludePaths = fmap (: []) distPath
  }


main :: IO ()
main = hakyllWith config $ do

--------------------------------------------------------------------------------
-- simple compilatiion

    forM_ [ "images/*"
          , "node/*"
          , "assets/resume.pdf"
          ] $ \f -> match f $ do
        route   idRoute
        compile copyFileCompiler


    match "css/*.scss" $ do
      route $ setExtension "css"
      let compressCssItem = fmap compressCss
      compile (compressCssItem <$> sassCompiler)

    match "templates/*" $ compile templateBodyCompiler

    match "code/**" $ do
        route idRoute
        compile getResourceString

    match "*.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "upload.html" $ do
        route idRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archives"            <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    let orimatches = fmap ("origami/*/*." ++ ) ["jpg", "jpeg"]

    create ["origami.html"] $ do
      route idRoute
      compile $ do
        models <- loadAll "origami/**.md"
        let origamiCtx =
              listField "models" postCtx (return models) <>
              constField "title" "Origami"              <>
              defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/origami.html" origamiCtx
          >>= loadAndApplyTemplate "templates/default.html" origamiCtx
          >>= relativizeUrls

    -- photos
    forM_ (fmap fromGlob orimatches) $ \f -> match f $ do
            route idRoute
            compile $ loadImage
              >>= compressJpgCompiler 50

    --match "posts/*" $ do
    --   route $ composeRoutes (gsubRoute "rss/" (const "posts")) (setExtension "html")
    --   runRoutes gsubRoute "posts/" (const "posts")

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
          snippetMap <- toSnippetMap <$> loadAll "code/**"
          pandocCompilerWithCodeInsertion snippetMap
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "origami/*/*.md" $ do
      route $ (customRoute $ trimModelPth . toFilePath) `composeRoutes` (setExtension "html")
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/model.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls



trimModelPth :: FilePath -> FilePath
trimModelPth path = (T.unpack $ head ws) ++ "/" ++ (T.unpack $ last ws) where
  ws = T.splitOn (T.pack "/") (T.pack path)

--------------------------------------------------------------------------------
--
-- http://blog.tpleyer.de/posts/2019-04-21-external-code-inclusion-with-hakyll.html


toSnippetMap :: [Item String] -> M.Map FilePath String
toSnippetMap is = M.fromList kvs
    where kvs = map ((toFilePath . itemIdentifier) &&& itemBody) is

pandocCompilerWithCodeInsertion :: M.Map FilePath String -> Compiler (Item String)
pandocCompilerWithCodeInsertion snippetMap =
    pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions (codeInclude snippetMap)

codeInclude :: M.Map FilePath String -> Pandoc -> Pandoc
codeInclude  snippetMap = walk $ \block -> case block of
    div@(Div (_,cs,_)_) -> if "code-include" `elem` cs
                            then codeBlockFromDiv snippetMap div
                            else block
    _ -> block

codeBlockFromDiv snippetMap div@(Div (_,_,kvs) _) =
    let classes = maybeToList $ lookup "lexer" kvs
        content = lookup "file" kvs >>= (`M.lookup` snippetMap)
    in maybe Null (CodeBlock ("", classes, [])) content
codeBlockFromDiv _ _ = Null

--------------------------------------------------------------------------------
-- defaults

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext
