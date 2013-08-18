{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Barch.QueryParser where

import Prelude
import Data.Text as T
import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Token

-- Term: a single blob of text, possibly with spaces if enclosed by quotes
type Term = Text


-- Elt: corresponds to a single term in our search language
data Elt = Plain Term |
           Field Text Term |
           Tag Term
           deriving Show

text::GenParser st Text
text =
  do s <- many1 (noneOf " \t\"")
     return $ pack s

spaceText::GenParser st Text
spaceText =
  do s <- sepBy text space
     return $ T.concat s

quotedText::GenParser st Text
quotedText =
  do s <- between (char '\"') (char '\"') $ many (noneOf "\"")
     return $ pack s

term::GenParser st Term
term =
  do val <- (quotedText <|> text)
     return val

plain::GenParser st Elt
plain =
  do t <- term
     return $ Plain t

field::GenParser st Elt
field =
  do key <- try $ do
       res <- many1 (noneOf " \t\":")
       char ':'
       return res
     val <- term
     return $ Field (pack key) val

tag::GenParser st Elt
tag =
  do char '#'
     val <- term
     return $ Tag val

line::GenParser st [Elt]
line =
  do many space
     sepEndBy (tag <|> field <|> plain) spaces
