module Warren.Codec 
  ( decode
  , encode
  ) where

import Prelude

import Data.Array
  ( concatMap
  , mapWithIndex
  , snoc
  )
import Data.Char.Unicode
  ( isUpper
  , toUpper
  )
import Data.Either
  ( Either(..)
  , note
  )
import Data.FoldableWithIndex
  ( foldlWithIndex
  )
import Data.Foldable
  ( foldM
  )
import Data.Int.Bits
  ( (.&.)
  , shl
  , shr
  )
import Data.Map
  ( Map
  , fromFoldable
  , lookup
  , toUnfoldable
  )
import Data.Maybe
  ( maybe
  )
import Data.String
  ( CodePoint
  , Pattern (..)
  , codePointFromChar
  , fromCodePointArray
  , joinWith
  , singleton
  , split
  , toCodePointArray
  , trim
  )
import Data.String.CodeUnits
  ( fromCharArray
  , toCharArray
  )
import Data.String.Regex
  ( Regex
  , regex
  , test
  )
import Data.String.Regex.Flags
  ( noFlags
  )
import Data.Traversable
  ( traverse
  )
import Data.Tuple
  ( Tuple(..)
  , swap
  )

type Result a = Either String a

inputCharSet :: Array CodePoint
inputCharSet = 
  toCodePointArray $
  """abcdefghijklmnopqrstuvwxyz""" <>
  """ABCDEFGHIJKLMNOPQRSTUVWXYZ""" <>
  """0123456789""" <>
  """!#$%&'()"*+,-./:;<=>?@[\]^_`|{}~ """

warrenChars :: Array Char
warrenChars = 
  toCharArray $
  "warren"

warrenRe :: Result Regex
warrenRe =
  flip regex noFlags $
  (_ <> "[!?])?$") $
  ("^(" <> _) $
  joinWith "" $
  (<$>) m $ 
  warrenChars
  where
    m :: Char -> String
    m c = 
      "[" <>
      toStr c <>
      (toStr <<< toUpper) c <>
      "]"
      where
        toStr :: Char -> String
        toStr = singleton <<< codePointFromChar

intToCodePoint :: Map Int CodePoint
intToCodePoint =
  fromFoldable $
  mapWithIndex Tuple $
  inputCharSet

codePointToInt :: Map CodePoint Int 
codePointToInt  = 
  fromFoldable $
  concatMap (pure <<< swap) $
  toUnfoldable $
  intToCodePoint

intToWarren :: Int -> Result String
intToWarren n
  | n < 0 || n > 127 = 
    Left ("unsupported number: " <> show n)
  | otherwise =
    pure $
    fromCharArray $
    flip snoc suffix $
    mapWithIndex m warrenChars
    where
      suffix :: Char
      suffix
        | n `shr` 6 == 0 = '!'
        | otherwise = '?'
      m :: Int -> Char -> Char
      m idx
        | (n `shr` idx) .&. 1 == 0 = identity
        | otherwise = toUpper

warrenToInt :: String -> Result Int
warrenToInt s = do
  re <- warrenRe
  if test re s
    then (pure <<< foldlWithIndex m 0 <<< toCharArray) s
    else Left ("invalid format: " <> show s)
  where
    m :: Int -> Int -> Char -> Int
    m idx acc c
      | c == '!' = acc 
      | c == '?' = acc + 64
      | isUpper c = acc + (1 `shl` idx)
      | otherwise = acc 

decode :: String -> Result String
decode "" = pure ""
decode s = do
  let chunks = (split (Pattern " ") <<< trim) s
  ints <- traverse warrenToInt chunks
  codepoints <- traverse m ints
  (pure <<< fromCodePointArray) codepoints
  where
    m :: Int -> Result CodePoint
    m = 
      maybe (Left "unsupported number") Right <<< 
      flip lookup intToCodePoint

encode :: String -> Result String
encode = 
  (<$>) (joinWith " ") <<<
  foldM m [] <<<
  toCodePointArray <<<
  trim
  where
    m :: Array String -> CodePoint -> Result (Array String)
    m acc c = do
      n <- note ("unsupported character: " <> show c)
                (lookup c codePointToInt)
      chunk <- intToWarren n
      (pure <<< snoc acc) chunk
