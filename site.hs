{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (mappend)
import qualified Data.Time as Time
import Data.Text (Text)
import qualified Data.Text as Text
import System.Environment (getArgs)
import Text.Pandoc
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk (walk)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = do
    Options { optCommand = command } <- defaultParser defaultConfiguration
    let debug = case command of { Watch {} -> True; _ -> False }

    let postsPat = postsPattern debug
    metadataMatcher <- metadataMatcher debug
    
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

        create ["all.css"] $ do
            route   idRoute
            compile $ do
                items <- loadAll "css/*"
                makeItem $ (if debug then id else compressCss) $ concatMap itemBody items

        match "contact.md" $ do
            route   $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

        matchMetadata postsPat metadataMatcher $ do
            route $ setExtension "html"
            compile $
                pandocCompilerWithTransform defaultHakyllReaderOptions defaultHakyllWriterOptions linkifyHeaders
                    >>= loadAndApplyTemplate "templates/post.html" (postCtx debug)
                    >>= saveSnapshot "content"
                    >>= loadAndApplyTemplate "templates/default.html" (postCtx debug)
                    >>= relativizeUrls

        version "redirects" $ createRedirects [
            ("posts/2018-03-10-eighty.html", "2018-03-16-eighty.html"),
            ("posts/2019-12-22-building-prolog's-rules-engine.html", "2019-12-22-building-prologs-rules-engine.html")
            ]

        create ["archive.html"] $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll postsPat
                let archiveCtx =
                        boolField "debug" (const debug) `mappend`
                        listField "posts" (postCtx debug) (pure posts) `mappend`
                        constField "title" "Archives" `mappend`
                        defaultContext

                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                    >>= relativizeUrls
        
        create ["atom.xml"] $ do
            route idRoute
            compile $ 
                loadAllSnapshots postsPat "content"
                    >>= recentFirst
                    >>= renderAtom feedConfig (atomCtx debug)


        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll postsPat
                let indexCtx =
                        boolField "debug" (const debug) `mappend`
                        listField "posts" (postCtx debug) (pure posts) `mappend`
                        defaultContext

                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/default.html" indexCtx
                    >>= relativizeUrls

        match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------

postsPattern True = "posts/*" .&&. hasNoVersion .||. "drafts/*"
postsPattern False = "posts/*" .&&. hasNoVersion

metadataMatcher True = return (const True)
metadataMatcher False = do
    time <- Time.getCurrentTime
    return $ \metadata ->
        case parseDate <$> lookupString "date" metadata of
            Just date -> date < time
            _ -> False

parseDate = Time.parseTimeOrError True Time.defaultTimeLocale "%Y-%m-%d"


showTOC :: Compiler Bool
showTOC = do
    meta <- getMetadata =<< getUnderlying
    return $ case Aeson.lookup "toc" meta of
        Just (Aeson.Bool True) -> True
        _ -> False

postCtx :: Bool -> Context String
postCtx debug =
    boolField "debug" (const debug) `mappend`
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
            field "sectionHash" (pure . sectionHash . itemBody) `mappend`
            field "sectionTitle" (pure . sectionTitle . itemBody)

atomCtx :: Bool -> Context String
atomCtx debug =
    postCtx debug `mappend`
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
    Item (fromFilePath hash) (TOCEntry hash (Text.unpack $ stringify content))
        | Header 2 (Text.unpack -> hash, _, _) content <- blocks
    ]

linkifyHeaders :: Pandoc -> Pandoc
linkifyHeaders pandoc = walk linkify pandoc
    where
        linkify (Header lvl attr@(ident, _, _) is) = Header lvl attr [Link nullAttr is ("#" <> ident, "")]
        linkify i = i
