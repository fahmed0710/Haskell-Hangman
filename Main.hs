module Main where

import Control.Concurrent (threadDelay)
import Data.List (intersperse, transpose)
import Data.Set (Set)
import qualified Data.Set as S
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Hangman!"
  putStrLn $ "*********************"
  playHangman (createHS "FUNCTIONAL")

playHangman :: HangmanState -> IO ()
playHangman hs
  | winGame hs = do
      putStr $ (formatFigure hs)
      putStrLn $ ("      " <> formatWord hs)
      putStrLn $ ""
      putStrLn $ ("\n" <> show (incorrectGuesses hs) <> "/6 incorrect guesses used")
      putStrLn "\nYOU WIN!"
  | loseGame hs = do
      putStr $ (formatFigure hs)
      putStrLn $ ("      " <> formatWord hs)
      putStrLn $ ("\n" <> show (incorrectGuesses hs) <> "/6 incorrect guesses used")
      putStrLn "\nYOU HAVE DIED."
  | otherwise = do
      putStr $ (formatFigure hs)
      putStrLn $ ("      " <> formatWord hs)
      putStrLn $ ("\n" <> show (incorrectGuesses hs) <> "/6 incorrect guesses used")
      putStrLn $ (show (6 - (incorrectGuesses hs)) <> " incorrect guesses left")
      putStrLn $ "Previously guessed letters: " <> formatGuesses hs

      putStrLn $ "*********************"

      putStr $ "Guess a letter: "
      inp <- getLine
      let g = head inp
      playHangman (fst (guessLetter g hs))

data HangmanState = HS
  { word :: String,
    guessedLetters :: Set Char,
    allowedIncorrectGuesses :: Int,
    incorrectGuesses :: Int
  }
  deriving (Show)

createHS :: String -> HangmanState
createHS word = HS word S.empty 6 0

incIncorrectGuesses :: HangmanState -> HangmanState
incIncorrectGuesses hs = hs {incorrectGuesses = incorrectGuesses hs + 1}

guessLetter :: Char -> HangmanState -> (HangmanState, Bool)
guessLetter c hs
  | c `elem` word hs = (updatedState, True)
  | c `notElem` guessedLetters hs = (incIncorrectGuesses updatedState, False)
  | otherwise = (updatedState, False)
  where
    updatedState = hs {guessedLetters = S.insert c (guessedLetters hs)}

winGame :: HangmanState -> Bool
winGame hs = all (`S.member` guessedLetters hs) (word hs)

loseGame :: HangmanState -> Bool
loseGame hs = not (winGame hs) && (incorrectGuesses hs == allowedIncorrectGuesses hs)

formatWord :: HangmanState -> String
formatWord hs = intersperse ' ' . map hideUnguessedLetters . word $ hs
  where
    hideUnguessedLetters c
      | S.member c (guessedLetters hs) = c
      | otherwise = '_'

formatGuesses :: HangmanState -> String
formatGuesses = intersperse ' ' . S.toList . guessedLetters

formatFigure :: HangmanState -> String
formatFigure hs
  | incorrectGuesses hs == 1 = " o "
  | incorrectGuesses hs == 2 = " o\n/"
  | incorrectGuesses hs == 3 = " o\n/|"
  | incorrectGuesses hs == 4 = " o\n/|\\"
  | incorrectGuesses hs == 5 = " o\n/|\\\n/"
  | incorrectGuesses hs == 6 = " o\n/|\\\n/ \\"
  | otherwise = ""