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
import Text.Pandoc
import Text.Pandoc.Shared (stringify)
import Text.Read (readMaybe)
import Text.HTML.TagSoup (Tag(TagOpen, TagClose))
import qualified Data.HashMap.Strict as H
import qualified Data.Aeson as Aeson
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
                pandocCompiler
                    >>= loadAndApplyTemplate "templates/post.html" postCtx
                    >>= linkifyHeaders
                    >>= saveSnapshot "content"
                    >>= loadAndApplyTemplate "templates/default.html" postCtx
                    >>= relativizeUrls

        version "redirects" $ createRedirects [
            ("posts/2018-03-10-eighty.html", "2018-03-16-eighty.html"),
            ("posts/2019-12-22-building-prolog's-rules-engine.html", "2019-12-22-building-prologs-rules-engine.html")
            ]

        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll postsPattern
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
            compile $ 
                loadAllSnapshots postsPattern "content"
                    >>= recentFirst
                    >>= renderAtom feedConfig atomCtx


        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll postsPattern
                let indexCtx =
                        listField "posts" postCtx (return posts) `mappend`
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


showTOC :: Compiler Bool
showTOC = do
    meta <- getMetadata =<< getUnderlying
    return $ case H.lookup "toc" meta of
        Just (Aeson.Bool True) -> True
        _ -> False



postCtx :: Context String
postCtx =
    tocFields `mappend`
    dateField "date" "%Y-%m-%d" `mappend`
    dateField "englishDate" "%B %e, %Y" `mappend`
    defaultContext
    where
        tocFields = 
            listField "tocItems" tocCtx (do
                toc <- showTOC
                if toc then do
                    pandoc <- readPandocWith defaultHakyllReaderOptions =<< getResourceBody
                    return $ makeTOC (itemBody pandoc)
                else return []
            )
        tocCtx =
            field "sectionHash" (return . sectionHash . itemBody) `mappend`
            field "sectionTitle" (return . sectionTitle . itemBody)

        

atomCtx :: Context String
atomCtx =
    postCtx `mappend`
    bodyField "description"

feedConfig = FeedConfiguration {
    feedTitle = "benjamin.pizza",
    feedDescription = "Benjamin's blog",
    feedAuthorName = "Benjamin Hodgson",
    feedAuthorEmail = "bhodgson@stackoverflow.com",
    feedRoot = "http://www.benjamin.pizza"
}

data TOCEntry = TOCEntry { sectionHash :: String, sectionTitle :: String }
    deriving (Eq, Ord, Show, Read)

makeTOC :: Pandoc -> [Item TOCEntry]
makeTOC (Pandoc _ blocks) = [
    Item (fromFilePath hash) (TOCEntry hash (stringify content))
        | Header 2 (hash, _, _) content <- blocks
    ]

linkifyHeaders :: Item String -> Compiler (Item String)
linkifyHeaders = return . fmap (withTagList (concatMap linkify))
    where
        linkify t@(TagOpen ('h':[_]) attrs) =
            let href = "#" ++ (fromMaybe "" $ lookup "id" attrs)
            in [t, TagOpen "a" [("href", href)]]
        linkify t@(TagClose ('h':[_])) = [TagClose "a", t]
        linkify t = [t]
