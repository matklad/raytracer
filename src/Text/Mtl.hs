{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Mtl
    ( Mtl(..)
    , parseMtl
    ) where

import Control.Applicative ((<$>), (<*>), (<*), (*>), pure)
import Data.Char (isAlpha, isAlphaNum, isSpace)
import qualified Data.ByteString.Char8 as S

import Data.Attoparsec.ByteString.Char8 (Parser, takeWhile1, string)

data Mtl = Mtl
    { mtlName :: S.ByteString
    } deriving Show

parseMtl :: Parser Mtl
parseMtl = do
  mtlName <- string "newmtl" *> takeWhile1 isSpace
  return Mtl { .. }
