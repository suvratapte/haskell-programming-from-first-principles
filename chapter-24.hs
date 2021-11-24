 -- I don't want any warnings as exercises will have some warnings.
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}

module Chapter_24 where

import Text.Trifecta

import Text.Parser.Combinators
import Control.Applicative

import Data.Ratio ((%))

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'

-- Exercises: Parsing Practice

-- 1

{-
There’s a combinator that’ll let us mark that we expect an input stream to be
finished at a particular point in our parser. In the parsers library this is
simply called eof (end-of-file) and is in the Text.Parser.Combinators
module. See if you can make the one and oneTwo parsers fail because they didn’t
exhaust the input stream!
-}

oneEof = one <* eof

oneTwoEof = oneTwo <* eof

-- 2
{-
Use string to make a Parser that parses “1”, “12”, and “123” out of the example --
input respectively. Try combining it with stop too. That is, a single parser --
should be able to parse all three of those strings. --
-}

oneTwoThree :: Parser String
oneTwoThree = string "123" <|> string "12" <|> string "1" <|> stop


-- 3
-- Try writing a Parser that does what string does, but using char.

stringParser :: String -> Parser String
stringParser = foldr (\c acc -> (:) <$> char c <*> acc) $ return ""

--

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

main' :: IO ()
main' = do
  let parseFraction' = parseString parseFraction mempty
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction

-- Exercise: Unit of Success

{-
This should not be unfamiliar at this point, even if you do not understand all
the details:

Prelude> parseString integer mempty "123abc"
Success 123
Prelude> parseString (integer >> eof) mempty "123abc"
Failure (interactive):1:4: error: expected: digit,
    end of input
123abc<EOF>
   ^
Prelude> parseString (integer >> eof) mempty "123"

Success ()

You may have already deduced why it returns () as a Success result here; it’s
consumed all the input but there is no result to return from having done so. The
result Success () tells you the parse was successful and consumed the entire
input, so there’s nothing to return.

What we want you to try now is rewriting the final example so it returns the
integer that it parsed instead of Success (). It should return the integer
successfully when it receives an input with an integer followed by an EOF and
fail in all other cases:

Prelude> parseString (yourFuncHere) mempty "123"
Success 123
Prelude> parseString (yourFuncHere) mempty "123abc"
Failure (interactive):1:4: error: expected: digit,
    end of input
123abc<EOF>
-}

s = parseString (integer <* eof) mempty "123"
f = parseString (integer <* eof) mempty "123abc"
