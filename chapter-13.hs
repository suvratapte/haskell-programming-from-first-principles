-- Chapter 13 exercises

-- Needed for chapter exercises
import Data.Char
import System.Exit
import Control.Monad

-- Intermission: Check your understanding

{-
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.MVar as MV
import qualified Data.ByteString.Char8 as B
import qualified Data.Locator as DL
import qualified Data.Time.Clock.POSIX as PSX
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FPC
import qualified Network.Info as NI
import qualified Safe
import Control.Exception (mask, try)
import Control.Monad (forever, when)
import Data.Bits
import Data.Bits.Bitwise (fromListBE)
import Data.List.Split (chunksOf)
import Database.Blacktip.Types
import System.IO.Unsafe (unsafePerformIO)

For our purposes right now, it does not matter whether you are familiar with the
modules referenced in the import list. Look at the declarations and answer the
questions below:
-}

-- 1 - What functions are being imported from Control.Monad?
-- forever, when

-- 2 - Which imports are both unqualified and imported in their en-tirety?
-- Data.Bits, Database.Blacktip.Types

-- 3 - From the name, what do you suppose importing blacktip’s Types module
-- brings in?
-- Types related to Blacktip Database

-- 4 - Now let’s compare a small part of blacktip’s code to the above import
-- list:

{-
writeTimestamp :: MV.MVar ServerState -> FPC.FilePath -> IO CC.ThreadId
writeTimestamp s path = do
  CC.forkIO go
  where go = forever $ do
          ss <- MV.readMVar s mask $ \_ -> do
            FS.writeFile path
            (B.pack (show (ssTime ss))) -- sleep for 1 second CC.threadDelay 1000000
-}

-- a - The type signature refers to three aliased imports. What modules are
-- named in those aliases?
-- Control.Concurrent.MVar, Filesystem.Path.CurrentOS, Control.Concurrent

-- b - Which import does FS.writeFile refer to?
-- Filesystem

-- c - Which import did forever come from?
-- Control.Monad

-- Check the `hangman` directory (sibling of this file).

-- Chapter exercises

-- Modifying code

-- 1
{-
Ciphers: Open your Ciphers module and modify it so that the Caesar and Vigenère
ciphers work with user input.
-}

-- Copying the cipher code here to avoid indirection.

cipherChar :: Int -> Char -> Char
cipherChar n c =
  let baseIndex = if isUpper c then ord 'A' else ord 'a'
      index = ord c - baseIndex
      newIndex = mod (index + n) 26
  in chr $ newIndex + baseIndex

cipher n = map $ cipherChar n

ceasarCipherUserInput :: IO ()
ceasarCipherUserInput = do
  putStrLn "Caesar Cipher"
  putStrLn "-------------"
  putStrLn "Enter the index: "
  indexStr <- getLine
  let index = (read indexStr :: Int)
  putStrLn "Enter the word to be ciphered: "
  word <- getLine
  putStrLn $ "The ciphered word is: " ++ cipher index word

vigenere :: String -> String -> String
vigenere key str = go str 0
  where
    go [] index = []
    go (' ':xs) index = ' ' : go xs index
    go (x:xs) index =
      let
        char = (key !! index)
        baseIndex = if isUpper char then ord 'A' else ord 'a'
        offset = ord char - baseIndex
      in
        cipherChar offset x : go xs (mod (index + 1) (length key))

vigenereCipherUserInput :: IO ()
vigenereCipherUserInput = do
  putStrLn "Vigenere Cipher"
  putStrLn "-------------"
  putStrLn "Enter the seed word for Vigenere cipher: "
  seed <- getLine
  putStrLn "Enter the word to be ciphered: "
  word <- getLine
  putStrLn $ "The ciphered word is: " ++ vigenere seed word

-- 2

{-
Here is a very simple, short block of code. Notice it has a forever that will
make it keep running, over and over again. Load it into your REPL and test it
out. Then refer back to the chapter and modify it to exit successfully after a
False result.
-}

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess

-- 3
{-
If you tried using palindrome on a sentence such as “Madam I’m Adam,” you may
have noticed that palindrome checker doesn’t work on that. Modifying the above
so that it works on sentences, too, involves several steps. You may need to
refer back to previous examples in the chapter to get ideas for proper ordering
and nesting. You may wish to import Data.Char to use the function toLower. Have
fun.
-}

palindromeSentence :: String -> Bool
palindromeSentence str =
  let stripped = filter (`elem` ['a'..'z']) (map toLower str)
  in stripped == reverse stripped

-- 4
type Name = String

type Age = Integer

data Person =
  Person Name Age
  deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

{-
Your job is to write the following function without modifying the code above.

gimmePerson :: IO ()
gimmePerson = undefined

Since IO () is about the least informative type imaginable, we’ll tell what it
should do.

a) It should prompt the user for a name and age input.

b) It should attempt to construct a Person value using the name and age the user
entered. You’ll need the read function for Age because it’s an Integer rather
than a String.

c) If it constructed a successful person, it should print ”Yay! Successfully got
a person:” followed by the Person value.

d) If it got an error value, report that an error occurred and print the error.
-}

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Enter the name of the person: "
  name <- getLine
  putStrLn "Enter the age of the person: "
  ageStr <- getLine
  if all (`elem` ['0'..'9']) ageStr
  then
    do
      let age = (read ageStr :: Integer)
      case mkPerson name age of
        Left error -> print error
        Right person -> putStrLn $ "Yay! Successfully got a person: " ++ (show person)
  else
    do
      putStrLn "Age should be a number."
      exitFailure
