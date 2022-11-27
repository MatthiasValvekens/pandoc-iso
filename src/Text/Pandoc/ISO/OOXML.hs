{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Some raw OOXML blurbs from Leonard's Lua filter & ISO's template

module Text.Pandoc.ISO.OOXML
  ( ooxmlBlock,
    ooxmlInline,
    ooxmlDirtyToc,
    ooxmlPageBreak,
    expandOOXMLMacro,
    OOXMLMacroError (..),
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Attoparsec.Text as AP
import Data.Char (chr)
import Data.FileEmbed
import Data.Functor ((<&>))
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Text.Pandoc.Definition

ooxmlBlock :: T.Text -> Block
ooxmlBlock = RawBlock (Format "openxml")

ooxmlInline :: T.Text -> Inline
ooxmlInline = RawInline (Format "openxml")

ooxmlDirtyToc :: Block
ooxmlDirtyToc =
  ooxmlBlock
    "<w:sdt>\
    \    <w:sdtContent xmlns:w=\"http://schemas.openxmlformats.org/wordprocessingml/2006/main\">\
    \        <w:p>\
    \            <w:r>\
    \                <w:fldChar w:fldCharType=\"begin\" w:dirty=\"true\" />\
    \                <w:instrText xml:space=\"preserve\">TOC \\o \"1-2\" \\h \\z \\u</w:instrText>\
    \                <w:fldChar w:fldCharType=\"separate\" />\
    \                <w:fldChar w:fldCharType=\"end\" />\
    \            </w:r>\
    \        </w:p>\
    \    </w:sdtContent>\
    \</w:sdt>"

ooxmlPageBreak :: Block
ooxmlPageBreak = ooxmlBlock "<w:p><w:r><w:br w:type=\"page\" /></w:r></w:p>"

ooxmlEmptyPara :: Block
ooxmlEmptyPara = ooxmlBlock "<w:p />"

data Token = Txt T.Text | Var T.Text

subVarsInTemplate :: Monad m => T.Text -> HM.HashMap T.Text T.Text -> ExceptT OOXMLMacroError m Block
subVarsInTemplate template vars = parsedTemplate >>= traverse resolveToken <&> T.concat <&> ooxmlBlock
  where
    doParse = do
      pre <- AP.takeWhile (/= chr 0x24)
      done <- AP.atEnd
      if done
        then return [Txt pre]
        else do
          -- consume the dollar sign first
          void $ AP.char (chr 0x24)
          -- attempt to parse a var name, if that doesn't work, treat as regular text
          nextTok <- AP.option (Txt "$") (Var <$> parseVarName)
          doParse <&> (\toks -> Txt pre : nextTok : toks)
    parseVarName = AP.string "{" *> AP.takeWhile1 (AP.inClass "-a-z") <* AP.string "}"
    resolveToken (Txt txt) = return txt
    resolveToken (Var varName) = case HM.lookup varName vars of
      Just value -> return value
      Nothing -> throwError (UnsetVariable varName)
    parsedTemplate = case AP.parseOnly doParse template of
      Left err -> throwError (SyntaxError err)
      Right result -> return result

ooxmlIsoFrontmatterTemplate :: T.Text
ooxmlIsoFrontmatterTemplate = $(embedStringFile "data/front-page-template.ooxml")

populateFrontmatterVars :: Monad m => ReaderT Meta m (HM.HashMap T.Text T.Text)
populateFrontmatterVars = do
  meta <- asks unMeta
  return $ addMap "iso" meta $ addString "title" meta $ addDateInfo meta HM.empty
  where
    addString name inputMap = case getString name inputMap of
      Just val -> HM.insert name val
      _ -> id
    getString name inputMap = case M.lookup name inputMap of
      Just (MetaString val) -> Just val
      Just (MetaInlines val) -> Just (stringify val)
      _ -> Nothing
    -- TODO validate syntax
    addDateInfo inputMap = case getString "date" inputMap of
      Just val -> HM.insert "draft-year" (T.takeWhile (/= chr 0x2d) val) . HM.insert "date" val
      Nothing -> id
    addMap name inputMap = case M.lookup name inputMap of
      Just (MetaMap subMap) -> foldr (.) id $ do
        key <- M.keys subMap
        return (addString key subMap)
      _ -> id
    stringify (Str x : xs) = x <> stringify xs
    stringify (Space : xs) = " " <> stringify xs
    stringify (_ : xs) = stringify xs
    stringify [] = ""

data OOXMLMacroError = UnsetVariable T.Text | SyntaxError String deriving (Show)

expandOOXMLMacro :: Monad m => T.Text -> ReaderT Meta (ExceptT OOXMLMacroError m) (Maybe Block)
expandOOXMLMacro "\\newpage" = return (Just ooxmlPageBreak)
expandOOXMLMacro "\\toc" = return (Just ooxmlDirtyToc)
expandOOXMLMacro "\\emptypar" = return (Just ooxmlEmptyPara)
expandOOXMLMacro "\\maketitle" = do
  vars <- populateFrontmatterVars
  lift $ Just <$> subVarsInTemplate ooxmlIsoFrontmatterTemplate vars
expandOOXMLMacro _ = return Nothing
