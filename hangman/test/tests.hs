module Tests where

-- import Test.QuickCheck
import Test.Hspec
import Main

mainTest = hspec $ do
  let word = "hello"
      puzzle = freshPuzzle word
  describe "fillInCharacter" $ do
    it "Guess correct character" $ do
      let
          newPuzzle = fillInCharacter puzzle 'h'
          expectedPuzzle =
            Puzzle word (Just 'h' : map (const Nothing) (tail word)) ['h']
      newPuzzle `shouldBe` expectedPuzzle

    it "Guess incorrect character" $ do
      let newPuzzle = fillInCharacter puzzle 'z'
          expectedPuzzle =
            Puzzle word (map (const Nothing) word) ['z']
      newPuzzle `shouldBe` expectedPuzzle

    it "Use all the alphabets" $ do
      let newPuzzle = foldl fillInCharacter puzzle word
          expectedPuzzle = Puzzle word (map Just word) (reverse word)
      newPuzzle `shouldBe` expectedPuzzle

    it "Use all the alphabets" $ do
      let newPuzzle = foldl fillInCharacter puzzle ['a'..'z']
          expectedPuzzle = Puzzle word (map Just word) (reverse ['a'..'z'])
      newPuzzle `shouldBe` expectedPuzzle

  describe "handleGuess" $ do
    it "Correct guess" $ do
      newPuzzle <- handleGuess puzzle 'h'
      let expectedPuzzle = Puzzle word (Just 'h' : map (const Nothing) (tail word)) ['h']
      newPuzzle `shouldBe` expectedPuzzle
    it "Incorrect guess" $ do
      newPuzzle <- handleGuess puzzle 'x'
      let expectedPuzzle = Puzzle word (map (const Nothing) word) ['x']
      newPuzzle `shouldBe` expectedPuzzle
    it "Repeated guess should not change the puzzle" $ do
      newPuzzle <- handleGuess puzzle 'h'
      newPuzzle' <- handleGuess puzzle 'h'
      let expectedPuzzle = Puzzle word (Just 'h' : map (const Nothing) (tail word)) ['h']
      newPuzzle `shouldBe` expectedPuzzle
      newPuzzle `shouldBe` newPuzzle'
    it "Special characters should be accepted as valid" $ do
      newPuzzle <- handleGuess puzzle '!'
      let expectedPuzzle = Puzzle word (map (const Nothing) word) ['!']
      newPuzzle `shouldBe` expectedPuzzle
