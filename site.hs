--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad ((>=>))
import Data.Char (isDigit)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import Data.Ord (comparing)
import Data.Traversable (for)
import System.FilePath (takeBaseName)
import Text.Read (readMaybe)
import Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/**/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md", "contact.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            comments <- compilePostComments
            let ctx = postCtx comments
            pandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" ctx
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "comments/*/*" $ do
        compile $
            pandocCompiler
                >>= loadAndApplyTemplate "templates/comment.html" commentCtx

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postSummaryCtx (return posts) `mappend`
                    constField "title" "Archives"                   `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
    
    create ["atom.xml"] $ do
        route idRoute
        compile $ 
            loadAllSnapshots "posts/*" "content"
                >>= recentFirst
                <&> take 10
                >>= renderAtom feedConfig atomCtx


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postSummaryCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: [Item String] -> Context String
postCtx comments =
    listField "comments" commentCtx (return comments) `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postSummaryCtx :: Context String
postSummaryCtx = 
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

atomCtx :: Context String
atomCtx =
    postSummaryCtx `mappend`
    bodyField "description"

commentCtx :: Context String
commentCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

feedConfig = FeedConfiguration {
    feedTitle = "benjamin.pizza",
    feedDescription = "Benjamin's blog",
    feedAuthorName = "Benjamin Hodgson",
    feedAuthorEmail = "bhodgson@stackoverflow.com",
    feedRoot = "http://www.benjamin.pizza"
}


compilePostComments :: Compiler [Item String]
compilePostComments = do
    filename <- fmap (takeBaseName . toFilePath) getUnderlying
    sortBy (comparing commentNumber) <$> loadAll (fromGlob $ "comments/" ++ filename ++ "/*")

commentNumber :: Item a -> Int
commentNumber =
    fromMaybe maxBound
    . readMaybe
    . takeWhile isDigit
    . takeBaseName
    . toFilePath
    . itemIdentifier

infixl 1 <&>
(<&>) = flip (<$>)