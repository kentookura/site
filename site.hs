--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Control.Arrow ((&&&))
import           Control.Monad (forM_)
import qualified Data.Map.Strict as M
import           Data.Maybe (maybeToList, fromMaybe)
import           Text.Pandoc
import           Text.Pandoc.Walk
import           Hakyll
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    forM_ [ "images./*"
          , "node/*"
          , "assets/resume.pdf"
          ] $ \f -> match f $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ compile compressCssCompiler
    create ["style.css"] $ do
      route idRoute
      compile $ do
        csses <- loadAll "css/*.css"
        makeItem $ unlines $ map itemBody csses

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
        
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
          snippetMap <- toSnippetMap <$> loadAll "code/**"
          pandocCompilerWithCodeInsertion snippetMap
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
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

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

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

