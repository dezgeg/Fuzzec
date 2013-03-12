{-# LANGUAGE NoMonomorphismRestriction #-}
module Fuzzer.Char(
    char,
    anyChar,
    space,
    newline,
    tab,
    upper,
    lower,
    octDigit,
    digit,
    hexDigit,
    alphaNum,
    braces,
    parens,
    brackets,
    angles,
    commaSep,
    semiEndBy,
    noneOf
) where

import Fuzzer.Core
import Fuzzer.Combinators
import Control.Monad
import Control.Applicative
import Data.Char (chr)

char :: Char -> Fuzzer String
char c      = return [c]
anyChar     = chance 0.8 alphaNum (chr <$> getRandomR (0, 0x10FFFF))

space :: Fuzzer String
space       = oneOf " \r\t\v\f" >>= char
newline     = char '\n'
tab         = char '\t'
upper       = getRandomR ('A', 'Z')
lower       = getRandomR ('a', 'z')

octDigit    = getRandomR ('0', '7')
digit       = getRandomR ('0', '9')
hexDigit    = oneOf $ "0123456789abcdef"

alphaNum    = join $ oneOf [upper, lower, digit]

braces      = between (char '{') (char '}')
parens      = between (char '(') (char ')')
brackets    = between (char '(') (char ')')
angles      = between (char '<') (char '>')

commaSep    = sepBy (char ',')
semiEndBy   = endBy (char ';')

noneOf cs   = do c <- anyChar
                 if c `elem` cs
                    then noneOf cs
                    else return c
