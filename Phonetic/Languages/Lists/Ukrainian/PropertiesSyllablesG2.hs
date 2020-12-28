-- |
-- Module      :  Phonetic.Languages.Lists.Ukrainian.PropertiesSyllablesG2
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Generalization and extension of the functionality of the DobutokO.Poetry.Norms
-- and DobutokO.Poetry.Norms.Extended modules
-- from the @dobutokO-poetry@ package. Uses syllables information.

{-# LANGUAGE CPP, BangPatterns #-}

module Phonetic.Languages.Lists.Ukrainian.PropertiesSyllablesG2 (
    -- * Newtype to work with
  CoeffTwo(..)
  , Coeffs2
  , isEmpty
  , isPair
  , fstCF
  , sndCF
  , readCF
  -- * Rhythmicity metrices (semi-empirical)
  -- ** Simple one
  , rhythmicity0
  -- ** With weight coefficients
  , rhythmicityK
  -- * Rhythmicity metrices from generated with r-glpk-phonetic-languages-ukrainian-durations package (since 0.2.0.0 version)
  -- ** Simple one
  , rhythmicity02
  -- ** With weight coefficients
  , rhythmicityK2
  -- * NEW Rhythmicity metrices from generated with r-glpk-phonetic-languages-ukrainian-durations package (since 0.2.0.0 version)
  -- ** Simple one
  , rhythmicity03
  -- ** With weight coefficients
  , rhythmicityK3
  -- * General
  , rhythmicity
) where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import qualified Data.Vector as VB
import Languages.Rhythmicity
import Languages.Phonetic.Ukrainian.Syllable.Double
import Languages.Phonetic.Ukrainian.Syllable
import Data.Maybe (isNothing,fromMaybe)
import Text.Read (readMaybe)

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

data CoeffTwo a = CF0 | CF2 (Maybe a) (Maybe a) deriving (Eq)

isEmpty :: CoeffTwo a -> Bool
isEmpty CF0 = True
isEmpty _ = False

isPair :: CoeffTwo a -> Bool
isPair CF0 = False
isPair _ = True

fstCF :: CoeffTwo a -> Maybe a
fstCF (CF2 x _) = x
fstCF _ = Nothing

sndCF :: CoeffTwo a -> Maybe a
sndCF (CF2 _ y) = y
sndCF _ = Nothing

readCF :: String -> Coeffs2
readCF xs
  | any (== '_') xs = let (!ys,!zs) = (\(ks,ts) -> (readMaybe ks::Maybe Double,readMaybe (drop 1 ts)::Maybe Double)) . break (== '_') $ xs in
     if (isNothing ys && isNothing zs) then CF0 else CF2 ys zs
  | otherwise = CF0

type Coeffs2 = CoeffTwo Double

--------------------------------------------------------------------------------------------

eval23 = evalRhythmicity23 . mconcat
{-# INLINE eval23 #-}

eval23K k2 k3 = evalRhythmicity23K k2 k3 . mconcat
{-# INLINE eval23K #-}

rhythmicity0 :: String -> Double
rhythmicity0 xs
 | null xs = 0.0
 | otherwise = eval23 . syllableDurationsD . createSyllablesUkrS $ xs

rhythmicityK :: Double -> Double -> String -> Double
rhythmicityK k2 k3 xs
 | null xs = 0.0
 | otherwise = eval23K k2 k3 . syllableDurationsD . createSyllablesUkrS $ xs

-------------------------------------------------------

rhythmicity02 :: String -> Double
rhythmicity02 xs
 | null xs = 0.0
 | otherwise = eval23 . syllableDurationsD2 . createSyllablesUkrS $ xs

rhythmicityK2 :: Double -> Double -> String -> Double
rhythmicityK2 k2 k3 xs
 | null xs = 0.0
 | otherwise = eval23K k2 k3 . syllableDurationsD2 . createSyllablesUkrS $ xs

-------------------------------------------------------

rhythmicity03 :: String -> Double
rhythmicity03 xs
 | null xs = 0.0
 | otherwise = eval23 . syllableDurationsD3 . createSyllablesUkrS $ xs

rhythmicityK3 :: Double -> Double -> String -> Double
rhythmicityK3 k2 k3 xs
 | null xs = 0.0
 | otherwise = eval23K k2 k3 . syllableDurationsD3 . createSyllablesUkrS $ xs

------------------------------------------------------------------

rhythmicity :: String -> Coeffs2 -> String -> Double
rhythmicity choice CF0
  | choice == "0y" = rhythmicity0
  | choice == "02y" = rhythmicity02
  | otherwise = rhythmicity03
rhythmicity choice (CF2 x y)
  | choice == "0y" = rhythmicityK (fromMaybe 1.0 x) (fromMaybe 1.0 y)
  | choice == "02y" = rhythmicityK2 (fromMaybe 1.0 x) (fromMaybe 1.0 y)
  | otherwise = rhythmicityK3 (fromMaybe 1.0 x) (fromMaybe 1.0 y)
