{-# LANGUAGE NoMonomorphismRestriction #-}
module CPP.Chars where
import Fuzzer.Core
import Fuzzer.Char
import Fuzzer.Combinators
import Control.Monad
import Control.Applicative

import Numeric (showIntAtBase)
import Data.Char (intToDigit)

identifierChars = [
    (0x30,0x39),
    (0x41,0x5A),
    (0x5F,0x5F),
    (0x61,0x7A),
    (0xA8,0xA8),
    (0xAA,0xAA),
    (0xAD,0xAD),
    (0xAF,0xAF),
    (0xB2,0xB5),
    (0xB7,0xBA),
    (0xBC,0xBE),
    (0xC0,0xD6),
    (0xD8,0xF6),
    (0xF8,0xFF),
    (0x100,0x167F),
    (0x1681,0x180D),
    (0x180F,0x1FFF),
    (0x200B,0x200D),
    (0x202A,0x202E),
    (0x203F,0x2040),
    (0x2054,0x2054),
    (0x2060,0x206F),
    (0x2070,0x218F),
    (0x2460,0x24FF),
    (0x2776,0x2793),
    (0x2C00,0x2DFF),
    (0x2E80,0x2FFF),
    (0x3004,0x3007),
    (0x3021,0x302F),
    (0x3031,0x303F),
    (0x3040,0xD7FF),
    (0xF900,0xFD3D),
    (0xFD40,0xFDCF),
    (0xFDF0,0xFE44),
    (0xFE47,0xFFFD),
    (0x10000,0x1FFFD),
    (0x20000,0x2FFFD),
    (0x30000,0x3FFFD),
    (0x40000,0x4FFFD),
    (0x50000,0x5FFFD),
    (0x60000,0x6FFFD),
    (0x70000,0x7FFFD),
    (0x80000,0x8FFFD),
    (0x90000,0x9FFFD),
    (0xA0000,0xAFFFD),
    (0xB0000,0xBFFFD),
    (0xC0000,0xCFFFD),
    (0xD0000,0xDFFFD),
    (0xE0000,0xEFFFD)
  ]

identifierCharsNotFirst = [
    (0x30,0x39),
    (0x300,0x36F),
    (0x1DC0,0x1DFF),
    (0x20D0,0x20FF),
    (0xFE20,0xFE2F)
  ]

punctuation = words ("{ } [ ] # ## ( ) <: :> <% %> %: %:%: ; : ... new delete" ++
    "? :: . .* + - * / % ^ & | ~ ! = < > += -= *= /= %= ^= &= |= " ++
    "<< >> >>= <<= == != <= >= && || ++ -- , ->* -> " ++ 
    "and and_eq bitand bitor compl not not_eq or or_eq xor xor_eq <::")

randomIdentifierChar = oneOf $ ['a'..'z'] ++ ['A'..'Z']

randomIdent :: Fuzzer String
randomIdent = replicateM 10 randomIdentifierChar

randomPunctuation = oneOf punctuation

randomChar = randomStringOrChar '\''
randomString = randomStringOrChar '"'

randomStringOrChar :: Char -> Fuzzer String
randomStringOrChar delim = (++[delim]) <$> ([delim]++) <$> concat <$> replicateM 40 content
    where content = chance 0.1 escapeSeq ((:[]) <$> noneOf (delim:"\\"))
          escapeSeq = ('\\':) <$> choice [
            (:[]) <$> oneOf "abfnrt",
            ('x':) <$> replicateM 2 hexDigit,
            getRandomR (0, 255) >>= \byte -> return $ showIntAtBase 8 intToDigit (byte :: Int) ""
           ]
