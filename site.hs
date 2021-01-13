--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad ((>=>))
import Data.Char (isSpace, isDigit, toLower)
import Data.List (sortBy)
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import Data.Monoid (mappend)
import Data.Ord (comparing)
import qualified Data.Set as S
import qualified Data.Time as Time
import Data.Traversable (for)
import System.Environment (getArgs)
import System.FilePath (takeBaseName)
import Text.Pandoc.Options
import Text.Read (readMaybe)
import Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = do
    command <- fromMaybe "" <$> listToMaybe <$> getArgs
    let postsPattern = postsPatternForCommand command
    metadataMatcher <- metadataMatcherForCommand command
    
    hakyll $ do
        match "favicon.ico" $ do
            route   idRoute
            compile copyFileCompiler

        match "images/**" $ do
            route   idRoute
            compile copyFileCompiler

        match "css/*" $ do
            compile getResourceBody

        create ["all.css"] $ do
            route   idRoute
            compile $ do
                items <- loadAll "css/*"
                makeItem $ compressCss $ concatMap itemBody items

        match (fromList ["about.md", "contact.md"]) $ do
            route   $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

        matchMetadata postsPattern metadataMatcher $ do
            route $ setExtension "html"
            compile $ do
                comments <- compilePostComments
                let ctx = postCtx comments
                pandocCompiler
                    >>= loadAndApplyTemplate "templates/post.html" ctx
                    >>= saveSnapshot "content"
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls

        version "redirects" $ createRedirects [
            ("posts/2018-03-10-eighty.html", "2018-03-16-eighty.html"),
            ("posts/2019-12-22-building-prolog's-rules-engine.html", "2019-12-22-building-prologs-rules-engine.html")
            ]

        match "comments/*/*" $ do
            compile $
                pandocCompiler
                    >>= loadAndApplyTemplate "templates/comment.html" commentCtx

        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll postsPattern
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
                loadAllSnapshots postsPattern "content"
                    >>= recentFirst
                    <&> take 10
                    >>= renderAtom feedConfig atomCtx


        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll postsPattern
                let indexCtx =
                        listField "posts" postSummaryCtx (return posts) `mappend`
                        defaultContext

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------

postsPatternForCommand "watch" = "posts/*" .&&. hasNoVersion .||. "drafts/*"
postsPatternForCommand _ = "posts/*" .&&. hasNoVersion

metadataMatcherForCommand "watch" = return (const True)
metadataMatcherForCommand _ = do
    time <- Time.getCurrentTime
    return $ \metadata ->
        let Just date = parseDate <$> lookupString "date" metadata
        in date < time

parseDate = Time.parseTimeOrError True Time.defaultTimeLocale "%Y-%m-%d"


postRoute metadata =
    let Just date = lookupString "date" metadata
        Just title = map (\x -> if isSpace x then '-' else x) <$> map toLower <$> lookupString "title" metadata
        route = "posts/" ++ date ++ "-" ++ title
    in constRoute route `composeRoutes` setExtension "html"

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
