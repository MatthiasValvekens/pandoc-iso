{-# LANGUAGE OverloadedStrings #-}

-- Some raw OOXML blurbs from Leonard's Lua filter


module Text.Pandoc.ISO.OOXML 
    ( ooxmlBlock
    , ooxmlInline
    , ooxmlDirtyToc
    , ooxmlPageBreak
    , expandOOXMLMacro
    ) where

import qualified Data.Text as T

import Text.Pandoc.Definition


ooxmlBlock :: T.Text -> Block
ooxmlBlock = RawBlock (Format "openxml")

ooxmlInline :: T.Text -> Inline
ooxmlInline = RawInline (Format "openxml")

ooxmlDirtyToc :: Block
ooxmlDirtyToc = ooxmlBlock $ "<w:sdt>\
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


expandOOXMLMacro :: T.Text -> Maybe Block
expandOOXMLMacro "\\newpage" = Just ooxmlPageBreak
expandOOXMLMacro "\\toc" = Just ooxmlDirtyToc
expandOOXMLMacro _ = Nothing
