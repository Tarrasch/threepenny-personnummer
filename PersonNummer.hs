{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (void, guard)
import Data.Maybe
import Text.Printf
import Safe          (readMay)
import qualified Data.Foldable

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = startGUI defaultConfig { tpPort = 10000 } setup

data Gender = Male | Female
  deriving (Eq)

charToInt :: Char -> Int
charToInt = read . (:[])

-- | Check the Luhn control number and gender contorl number.
-- It doesn't check birth date, birth city etc.
validate :: String -- ^ Personnummer
         -> Gender -- ^ Gender
         -> Bool   -- ^ Is valid?
validate numString gender = Data.Foldable.or $ do
    guard $ length numString == 10
    (number :: Int) <- readMay numString
    let digits = map charToInt numString
    guard $ genderFunction $ digits !! 8
    return $ luhn (init digits) == last digits
  where
    genderFunction | gender == Male = odd :: Int -> Bool
                   | otherwise      = even

luhn :: [Int] -> Int
luhn = (10-)
     . (`mod` 10)
     . sum
     . map charToInt
     . concatMap show
     . zipWith (*) (cycle [2,1])

setup :: Window -> UI ()
setup window = void $ do
    return window # set title "Currency Converter"

    personNummer <- UI.input
    genderBox <- UI.input # set UI.type_ "checkbox"
    nextBtn   <- UI.button #+ [string "Next"]

    getBody window #+ [
            column
              [ grid
                [ [ row [string "PersonNummer:", element personNummer]]
                , [ row [string "Male?", element genderBox]]
                ]
              , element nextBtn
              ]
            ]

    (bPersonNummer :: Behavior String) <- stepper "0" (UI.valueChange personNummer)
    (bBoxChecked :: Behavior Bool) <- stepper False (UI.checkedChange genderBox)
    let bGender = (\x -> if x then Male else Female) <$> bBoxChecked
        bEnabled = validate <$> bPersonNummer <*> bGender

    element nextBtn # sink UI.enabled bEnabled
