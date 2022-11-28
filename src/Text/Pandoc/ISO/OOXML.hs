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
    SimpleTable (..),
    trySimplifyTable,
    formatSimpleTable,
    populateFrontmatterVars,
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
import Text.Pandoc.ISO.Types

ooxmlBlock :: T.Text -> Block
ooxmlBlock = RawBlock (Format "openxml")

ooxmlInline :: T.Text -> Inline
ooxmlInline = RawInline (Format "openxml")

ooxmlDirtyToc :: Block
ooxmlDirtyToc = ooxmlBlock $(embedStringFile "data/dirty-toc.ooxml")

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

ooxmlIsoFrontPageTemplate :: T.Text
ooxmlIsoFrontPageTemplate = $(embedStringFile "data/front-page-template.ooxml")

ooxmlIsoFrontmatterSectDelimBlock :: Block
ooxmlIsoFrontmatterSectDelimBlock = ooxmlBlock $(embedStringFile "data/frontmatter-sectpr.ooxml")

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

stringify :: [Inline] -> T.Text
stringify (Str x : xs) = x <> stringify xs
stringify (Space : xs) = " " <> stringify xs
stringify (Span _ xs' : xs) = stringify xs' <> stringify xs
stringify (Link _ xs' _ : xs) = stringify xs' <> stringify xs
stringify (Emph xs' : xs) = stringify xs' <> stringify xs
stringify (Underline xs' : xs) = stringify xs' <> stringify xs
stringify (Strong xs' : xs) = stringify xs' <> stringify xs
stringify (Strikeout xs' : xs) = stringify xs' <> stringify xs
stringify (Superscript xs' : xs) = stringify xs' <> stringify xs
stringify (SmallCaps xs' : xs) = stringify xs' <> stringify xs
stringify (Quoted _ xs' : xs) = stringify xs' <> stringify xs
stringify (Cite _ xs' : xs) = stringify xs' <> stringify xs
stringify (Image _ xs' _ : xs) = stringify xs' <> stringify xs
stringify (Code _ x : xs) = x <> stringify xs
stringify (SoftBreak : xs) = "\n" <> stringify xs
stringify (LineBreak : xs) = "\n" <> stringify xs
stringify (_ : xs) = stringify xs
stringify [] = ""

data OOXMLMacroError = UnsetVariable T.Text | SyntaxError String deriving (Show)

expandOOXMLMacro :: Monad m => T.Text -> ReaderT Meta (ExceptT OOXMLMacroError m) (Maybe Block)
expandOOXMLMacro "\\newpage" = return (Just ooxmlPageBreak)
expandOOXMLMacro "\\toc" = return (Just ooxmlDirtyToc)
expandOOXMLMacro "\\emptypar" = return (Just ooxmlEmptyPara)
expandOOXMLMacro "\\mainmatter" = return (Just ooxmlIsoFrontmatterSectDelimBlock)
expandOOXMLMacro "\\doctitle" = do
  meta <- ask
  case docTitle meta of
    [] -> return Nothing
    title -> return $ Just (Div attrs [Para title])
      where attrs = ("", [], [("custom-style", "zzSTDTitle")])
expandOOXMLMacro "\\makebibliography" = return (Just biblio)
  where biblioTitle = Div titleAttrs [Para [Str "Bibliography"]]
        biblioSection = Div sectAttrs []
        biblio = Div ("", [], []) [biblioTitle, biblioSection]
        titleAttrs = ("", [], [("custom-style", "Biblio Title")])
        sectAttrs = ("refs", [], [("custom-style", "Bibliography")])
expandOOXMLMacro "\\maketitle" = do
  vars <- populateFrontmatterVars
  lift $ Just <$> subVarsInTemplate ooxmlIsoFrontPageTemplate vars
expandOOXMLMacro _ = return Nothing

type SimpleCell = [Block]

type SimpleRow = [SimpleCell]

data SimpleTable = SimpleTable
  { simpleCaption :: T.Text,
    colWidths :: [ColWidth],
    headerRow :: SimpleRow,
    bodyRows :: [SimpleRow]
  }
  deriving (Show)

trySimplifyTable :: Block -> Maybe SimpleTable
trySimplifyTable (Table _ capt cols th [tb] tf) = do
  guard (noFooter tf)
  th' <- simpleHeader th
  tb' <- simpleBody tb
  return $ SimpleTable captionText ws th' tb'
  where
    captionText = stringify (inline capt)
    ws = fmap snd cols
    noFooter (TableFoot _ []) = True
    noFooter _ = False
    simpleHeader (TableHead _ [h]) = simpleRow h
    simpleHeader _ = Nothing
    simpleRow (Row _ cells) = traverse simpleCell cells
    simpleCell (Cell _ AlignDefault (RowSpan 1) (ColSpan 1) blks) = Just blks
    simpleCell _ = Nothing
    simpleBody (TableBody _ (RowHeadColumns 0) [] rows) = traverse simpleRow rows
    simpleBody _ = Nothing
trySimplifyTable _ = Nothing

ooxmlTablePropsTemplate :: T.Text
ooxmlTablePropsTemplate = $(embedStringFile "data/table-props-template.ooxml")

-- Same logic as in Pandoc's table layout code, but without the intermediate
-- XML representation
simpleTableLayout :: [ColWidth] -> (T.Text, Int, [Int])
simpleTableLayout cWidths
  | all (== 0) widths = ("auto", 0, proportionToTwips <$> equidistributed)
  | otherwise = ("pct", totalWidthPct, proportionToTwips <$> widths)
  where
    numCols = length cWidths
    equidistributed = replicate numCols $ 1.0 / fromIntegral numCols
    -- ColWidth values are percentages
    widths = fmap getWidth cWidths
      where
        getWidth (ColWidth n) = n
        getWidth _ = 0
    -- in relative pct units, 5000 is a full row
    totalWidthPct = round (5000 * sum widths) :: Int
    -- ...but the width in gridCol is always in twentieths of a point (twips)
    -- We take 7920 as the text width here as in Pandoc, but that might not
    -- be appropriate ==> TODO check template page width!
    proportionToTwips :: Double -> Int
    proportionToTwips w = floor (7920 * w)

formatSimpleCell :: Int -> T.Text -> SimpleCell -> [Block]
formatSimpleCell twipsW extraProps blks = open ++ blks ++ close
  where
    tcPr = "<w:tcPr>" <> tcW <> extraProps <> "</w:tcPr>"
    tcW = "<w:tcW w:type=\"dxa\" w:w=\"" <> T.pack (show twipsW) <> "\"/>"
    open =
      [ ooxmlBlock "<w:tc>",
        ooxmlBlock tcPr
      ]
    close = [ooxmlBlock "</w:tc>"]

formatSimpleRow :: T.Text -> T.Text -> [Int] -> [SimpleCell] -> [Block]
formatSimpleRow rowExtra cellExtraProps ws cs = open ++ cells ++ close
  where
    cells = do
      (w, c) <- zip ws cs
      formatSimpleCell w cellExtraProps c
    open = [ooxmlBlock $ "<w:tr>" <> rowExtra]
    close = [ooxmlBlock "</w:tr>"]

-- | Format a simple table in the ISO template using raw OOXML hacks.
formatSimpleTable :: Monad m => SimpleTable -> ExceptT OOXMLMacroError m [Block]
formatSimpleTable tbl = do
  let (wType, wVal, ws) = simpleTableLayout (colWidths tbl)
  let tablePropsVars =
        HM.fromList
          [ ("short-caption", simpleCaption tbl),
            ("table-w-value", T.pack (show wVal)),
            ("table-w-type", wType)
          ]
  tableProps <- subVarsInTemplate ooxmlTablePropsTemplate tablePropsVars
  let open =
        [ ooxmlBlock "<w:tbl>",
          tableProps,
          ooxmlBlock (fmtGrid ws)
        ]
  let headerMarker = "<w:trPr><w:tblHeader/></w:trPr>"
  let headerCellPropsExtra = $(embedStringFile "data/table-header-cell-props-template.ooxml")
  let hdr = formatSimpleRow headerMarker headerCellPropsExtra ws (headerRow tbl)
  let rows = concatMap (formatSimpleRow "" "" ws) (bodyRows tbl)
  let close = [ooxmlBlock "</w:tbl>"]
  return $ open ++ hdr ++ rows ++ close
  where
    fmtGrid ws = "<w:tblGrid>" <> gridCols <> "</w:tblGrid>"
      where
        gridCol w = "<w:gridCol w:w=\"" <> T.pack (show w) <> "\"/>"
        gridCols = T.concat $ fmap gridCol ws
