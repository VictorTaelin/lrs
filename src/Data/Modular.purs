module Data.Modular where

import Data.BigInt (BigInt, fromInt, toBase, modPow, negative)
import Data.Foldable (foldr)
import Data.String (joinWith)
import Prelude

mMod :: BigInt -> BigInt -> BigInt
mMod a b = if negative ab then (ab + b) `mod` b else ab
  where ab = a `mod` b

mPow :: BigInt -> BigInt -> BigInt -> BigInt
mPow p a b 
  | negative b = mInv p (modPow a (-b) p)
  | otherwise  = modPow a b p

mMul :: BigInt -> BigInt -> BigInt -> BigInt
mMul p a b = (a * b) `mMod` p

mInv :: BigInt -> BigInt -> BigInt
mInv p i = mPow p i (p - big2)

mProd :: BigInt -> Array BigInt -> BigInt
mProd p a = foldr (mMul p) big1 a

mJoin :: Array BigInt -> String
mJoin ints = joinWith "" (map (toBase 16) ints)

big1 :: BigInt
big1 = fromInt 1

big2 :: BigInt
big2 = fromInt 2
