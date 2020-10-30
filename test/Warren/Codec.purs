module Test.Main where

import Prelude

import Data.Char.Gen
  ( genAlpha
  , genDigitChar
  )
import Data.Either
  ( fromRight
  )
import Data.NonEmpty
  ( (:|)
  )
import Data.String
  ( trim
  )
import Data.String.Gen
  ( genString
  )
import Data.String.Regex as Re
import Data.String.Regex.Flags
  ( noFlags
  )
import Effect 
  ( Effect
  )
import Test.QuickCheck
  ( class Arbitrary
  , Result
  , (===)
  )
import Test.QuickCheck.Gen
  ( Gen
  , elements
  , oneOf
  )
import Test.Unit
  ( test
  )
import Test.Unit.Main
  ( runTest
  )
import Test.Unit.QuickCheck
  ( quickCheck
  )
import Partial.Unsafe
  ( unsafePartial
  )

import Warren.Codec
  ( decode
  , encode
  )

newtype AsciiString = AsciiString String

instance arbitraryAsciiString :: Arbitrary AsciiString where
  arbitrary :: Gen AsciiString 
  arbitrary = 
    (AsciiString) <$> 
    (genString charGen)
    where
      charGen :: Gen Char
      charGen =
        oneOf $
        (elements (' ' :| [ '!', '?', '.', ',' ])) :| 
        [ genAlpha, 
          genDigitChar ]

encMsgRe :: Re.Regex
encMsgRe = 
  unsafePartial $
  fromRight $
  Re.regex pattern noFlags
  where
    chunkPattern :: String
    chunkPattern = "[wW][aA][rR][rR][eE][nN][!?]"
    pattern :: String
    pattern =
      "^(" <>
      chunkPattern <>
      ")?( " <>
      chunkPattern <>
      ")*$"

main :: Effect Unit
main = 
  runTest do
    test "format" do
       quickCheck formatProp
    test "reversibility" do
      quickCheck reversibilityProp
  where
    reversibilityProp :: AsciiString -> Result
    reversibilityProp (AsciiString s) =
      (pure $ trim $ s) === (encode s >>= decode)
    formatProp :: AsciiString -> Result
    formatProp (AsciiString s) = 
      pure true === (Re.test encMsgRe <$> encode s)

