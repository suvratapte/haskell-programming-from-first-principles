 -- I don't want any warnings as exercises will have some warnings.
{-# OPTIONS_GHC -w #-}

module Chapter_24 where

import Text.Trifecta

import Text.Parser.Combinators
import Control.Applicative

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
