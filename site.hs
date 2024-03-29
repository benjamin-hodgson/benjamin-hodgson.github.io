{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Functor.Identity (Identity(runIdentity))
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Time as Time
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import Data.Foldable (for_)
import Data.Traversable (for)
import System.Environment (getArgs)

import qualified Text.Blaze.Html as Blaze
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import qualified Commonmark as Md
import qualified Commonmark.Blocks as Md
import qualified Commonmark.Extensions as Md
import qualified Skylighting
import Hakyll
import Text.Parsec (getState, updateState)


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

        for_ ["contact.md", "404.md"] $ \pat -> match pat $ do
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
    for body $ \md -> case runIdentity $ Md.commonmarkWith benjaminFlavouredMarkdown path md of
        Left err -> fail (show err)
        Right (Html html) -> return $ Text.Lazy.unpack (Md.renderHtml html)

newtype Html = Html { getHtml :: Md.Html () }
    deriving (
        Show,
        Semigroup,
        Monoid,
        Md.Rangeable,
        Md.HasAttributes,
        Md.IsInline,
        Md.HasQuoted,
        Md.HasStrikethrough,
        Md.ToPlainText
    )

instance Md.HasFootnote Html Html where
  footnote num txt = Html . Md.footnote num txt . getHtml
  footnoteList = Html . Md.footnoteList . map getHtml
  footnoteRef num txt = Html . Md.footnoteRef num txt . getHtml
instance Md.IsBlock Html Html where
  paragraph = Html . Md.paragraph . getHtml
  plain = Html . Md.plain . getHtml
  thematicBreak = Html Md.thematicBreak
  blockQuote = Html . Md.blockQuote . getHtml
  codeBlock = highlightCodeBlock
  heading lvl = Html . Md.heading lvl . getHtml
  rawBlock fmt txt = Html (Md.rawBlock fmt txt)
  referenceLinkDefinition txt info = Html (Md.referenceLinkDefinition txt info)
  list ty spacing = Html . Md.list ty spacing . map getHtml

benjaminFlavouredMarkdown :: Md.SyntaxSpec Identity Html Html
benjaminFlavouredMarkdown = mconcat [
    Md.smartPunctuationSpec,
    Md.strikethroughSpec,
    Md.attributesSpec,
    Md.autoIdentifiersAsciiSpec,
    Md.footnoteSpec,
    Md.implicitHeadingReferencesSpec,
    linkifyHeadersSpec,
    Md.defaultSyntaxSpec
    ]

linkifyHeadersSpec :: Md.SyntaxSpec Identity Html Html
linkifyHeadersSpec = mempty {
        Md.syntaxFinalParsers = [linkifyHeaders]
    }

linkifyHeaders :: Md.BlockParser Identity Html Html Html
linkifyHeaders = do
    nodes <- Md.nodeStack <$> getState
    updateState $ \st -> st { Md.nodeStack = [fmap setLink t | t <- nodes] }
    return mempty

    where
        setLink bd
            | Md.blockType (Md.blockSpec bd) `elem` ["ATXHeading", "SetextHeading"] =
                case lookup "id" (Md.blockAttributes bd) of
                    Nothing -> bd
                    Just ident -> bd { Md.blockLines = addLink (Md.blockLines bd) ident }
            | otherwise = bd

        addLink [line@(_:_)] ident =
            let (start, end) = (Md.tokPos (head line), Md.tokPos (last line))
                linkDest = [sym '(' end, sym '#' end] ++ Md.tokenize "" ident ++ [sym ')' end]
                newLine = [sym '[' start] ++ line ++ [sym ']' end] ++ linkDest
            in [newLine]
        addLink lines _ = lines

        sym s loc = Md.Tok (Md.Symbol s) loc (Text.singleton s)

highlightCodeBlock :: Text -> Text -> Html
highlightCodeBlock lang txt =
    let result = do
            syntax <- Skylighting.lookupSyntax lang Skylighting.defaultSyntaxMap
            lines <- foldMap Just $ Skylighting.tokenize defaultTokeniserConfig syntax txt
            return $ Skylighting.formatHtmlBlock Skylighting.defaultFormatOpts lines
    in case result of
        Just blaze -> Html $ Md.htmlRaw $ Text.Lazy.toStrict $ Blaze.renderHtml blaze
        Nothing -> Html $ Md.codeBlock lang txt 

defaultTokeniserConfig = Skylighting.TokenizerConfig Skylighting.defaultSyntaxMap False
