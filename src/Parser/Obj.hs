{-# LANGUAGE OverloadedStrings #-}

module Parser.Obj
       ( parse
       ) where

import Data.Maybe(catMaybes)

import qualified Data.ByteString.Char8 as B
import qualified Data.Array as A
import Data.ByteString.Lex.Double(readDouble)

type Point = (Double, Double, Double)

parse:: B.ByteString -> [[Point]]
parse s =
    faces
  where
    ls = B.lines s
    startsWith pref str = (B.take (B.length pref) str) == pref

    vLines = filter (startsWith "v ") ls
    fLines = filter (startsWith "f ") ls

    verts = A.array (0, length vLines) (zip [0..] $ map readVert vLines)
    faces = map readFace fLines

    tokens = tail . B.words
    readVert l =
        case catMaybes $ map readDouble (tokens l) of
            [(x, _), (y, _), (z, _)] -> (x, y, z)
            _ -> error "Expected three doubles in vertex!"

    readFaceBlock b = case B.readInt (head $ B.split '/' b) of
        Just (x, _)  -> verts A.! (pred x)
        Nothing -> error "Expected Int in face"

    readFace l = map readFaceBlock (tokens l)
