 -- I don't want any warnings as exercises will have some warnings.
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Chapter_24 where

import Text.Trifecta

import Text.Parser.Combinators
import Control.Applicative

import Data.Ratio ((%))

import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta

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

-- Exercise: Try Try

{-
Make a parser, using the existing fraction parser plus a new decimal parser,
that can parse either decimals or fractions. You’ll want to use <|> from
Alternative to combine the...alternative parsers. If you find this too
difficult, write a parser that parses straightforward integers or
fractions. Make a datatype that contains either an integer or a rational and use
that datatype as the result of the parser. Or use Either. Run free, grasshopper.

Hint: we’ve not explained it yet, but you may want to try try.
-}

parseDecimalOrFraction :: Parser (Either Rational Integer)
parseDecimalOrFraction = try (Left <$> parseFraction) <|> (Right <$> decimal)

--

headerEx :: ByteString
headerEx = "[blah]"

-- "[blah]" -> Section "blah"
newtype Header = Header String
  deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

--

assignmentEx :: ByteString
assignmentEx = "woot=1"

type Name = String
type Value = String
type Assignments = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  _ <- char '='
  val <- some (noneOf "\n")
  skipEOL
  return (name, val)

skipEOL :: Parser ()
skipEOL = skipMany $ oneOf "\n"

--

commentEx :: ByteString
commentEx =
  "; last modified 1 April\
  \ 2001 by John Doe"

commentEx' :: ByteString
commentEx' = "; blah\n; woot\n  \n;hah"

skipComments :: Parser ()
skipComments =
  skipMany (do _ <- char ';' <|> char '#'
               skipMany (noneOf "\n")
               skipEOL)

--

sectionEx :: ByteString
sectionEx =
  "; ignore me\n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' = [r|
; ignore me
[states]
Chris=Texas
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw

[whatisit]
red=intoothandclaw
|]

data Section =
  Section Header Assignments
  deriving (Eq, Show)

newtype Config =
  Config (Map Header Assignments)
  deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace = skipMany $ char ' ' <|> char '\n'

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assingments <- some parseAssignment
  return . Section h $ M.fromList assingments

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) = M.insert h a

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections =
        foldr rollup M.empty sections
  return $ Config mapOfSections

--

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main'' :: IO ()
main'' = hspec $ do
  describe "Assignment Parsing" $
    it "can parse a simple assingment" $ do
      let m = parseByteString
                parseAssignment
                mempty
                assignmentEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just ("woot", "1")

  describe "Header Parsing" $
    it "can parse a simple header" $ do
      let m = parseByteString
                parseHeader
                mempty
                headerEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Header "blah")

  describe "Comment parsing" $
    it "Skips comment before header" $ do
      let p = skipComments >> parseHeader
          i = "; woot\n[blah]"
          m = parseByteString p mempty i
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Header "blah")

  describe "Section parsing" $
    it "can parse a simple section" $ do
      let m = parseByteString parseSection mempty sectionEx
          r' = maybeSuccess m
          states = M.fromList [("Chris", "Texas")]
          expected' =Just (Section (Header "states") states)
      print m
      r' `shouldBe` expected'

  describe "INI parsing" $
    it "Can parse multiple sections" $ do
      let m = parseByteString parseIni mempty sectionEx''
          r' = maybeSuccess m
          sectionValues = M.fromList [ ("alias", "claw")
                                     , ("host", "wikipedia.org")]
          whatisitValues = M.fromList [("red", "intoothandclaw")]
          expected' = Just (Config (M.fromList
                                     [ (Header "section" , sectionValues)
                                     , (Header "whatisit" , whatisitValues)]))
      print m
      r' `shouldBe` expected'
