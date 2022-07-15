{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Functor.Identity (Identity(runIdentity))
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Time as Time
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import Data.Traversable (for)
import System.Environment (getArgs)

import Commonmark
import Commonmark.Blocks (BlockParser, BPState(nodeStack), BlockData(..), BlockSpec(blockType))
import Commonmark.Extensions (footnoteSpec, attributesSpec, autoIdentifiersAsciiSpec, implicitHeadingReferencesSpec, strikethroughSpec)
import Hakyll
import Text.Parsec (getState, updateState)
import Debug.Trace


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

        match "Benjamin_Hodgson_CV.pdf" $ do
            route   idRoute
            compile copyFileCompiler

        match "css/*" $ do
            compile getResourceBody
            
        match "js/**" $ do
            route   idRoute
            compile getResourceBody

        create ["all.css"] $ do
            route   idRoute
            compile $ do
                items <- loadAll "css/*"
                makeItem $ compressCss $ concatMap itemBody items

        match "contact.md" $ do
            route   $ setExtension "html"
            compile $ commonmarkCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

        matchMetadata postsPattern metadataMatcher $ do
            route $ setExtension "html"
            compile $
                commonmarkCompiler
                    >>= loadAndApplyTemplate "templates/post.html" postCtx
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
        case parseDate <$> lookupString "date" metadata of
            Just date -> date < time
            _ -> False

parseDate = Time.parseTimeOrError True Time.defaultTimeLocale "%Y-%m-%d"


postCtx :: Context String
postCtx =
    dateField "date" "%Y-%m-%d" `mappend`
    dateField "englishDate" "%B %e, %Y" `mappend`
    defaultContext


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

commonmarkCompiler :: Compiler (Item String)
commonmarkCompiler = cached "Benjamin.Pizza.commonmarkCompiler" $ do
    body <- fmap Text.pack <$> getResourceBody
    path <- getResourceFilePath
    for body $ \md -> case runIdentity $ commonmarkWith benjaminFlavouredMarkdown path md of
        Left err -> fail (show err)
        Right html -> return $ Text.Lazy.unpack (renderHtml html)


benjaminFlavouredMarkdown :: SyntaxSpec Identity (Html ()) (Html ())
benjaminFlavouredMarkdown = 
    strikethroughSpec
    <> attributesSpec
    <> autoIdentifiersAsciiSpec
    <> footnoteSpec
    <> implicitHeadingReferencesSpec
    <> linkifyHeadersSpec
    <> defaultSyntaxSpec

linkifyHeadersSpec :: SyntaxSpec Identity (Html ()) (Html ())
linkifyHeadersSpec = mempty {
        syntaxFinalParsers = [linkifyHeaders]
    }

linkifyHeaders :: BlockParser Identity (Html ()) (Html ()) (Html ())
linkifyHeaders = do
    nodes <- nodeStack <$> getState
    updateState $ \st -> st { nodeStack = [fmap setLink t | t <- nodes] }
    return mempty

    where
        setLink bd
            | blockType (blockSpec bd) `elem` ["ATXHeading", "SetextHeading"] =
                case lookup "id" (blockAttributes bd) of
                    Nothing -> bd
                    Just ident -> traceShowId $ bd { blockLines = addLink (blockLines bd) ident }
            | otherwise = bd

        addLink [line@(_:_)] ident =
            let (start, end) = (tokPos (head line), tokPos (last line))
                linkDest = [Tok (Symbol '(') end "(", Tok (Symbol '#') end "#"] ++ tokenize "" ident ++ [Tok (Symbol ')') end ")"]
                newLine = [Tok (Symbol '[') start "["] ++ line ++ [Tok (Symbol ']') end "]"] ++ linkDest
            in [newLine]
        addLink lines _ = lines
