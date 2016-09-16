-- | Linkable Ring Signatures as defined on http://link.springer.com/chapter/10.1007%2F11424826_65
-- |
-- | This module exports only pure functions, i.e., `gen` and `sign`
-- | are exported as makePublicKey and pureSign, both of which need
-- | entropy sources as inputs. Moreover, functions exported here
-- | are parameterized by `(LGroup generator order prime)`, which
-- | represents a multiplicative group of prime order `prime`, such
-- | that the underlying discrete logarithm problem (DLP) is hard.
-- | If you want acceptable defaults, just use Data.LRS.SimpleAPI.

module Crypto.LRS 
  ( makePublicKey
  , Party(..)
  , PrivateKey
  , PublicKey
  , Signature(..)
  , Ring
  , pureLink
  , pureSign
  , pureVerify
  , publicKey
  , privateKey
  ) where

import Crypto.Sha512 (hmacSha512)
import Data.Array (index, zipWith, updateAt, findIndex)
import Data.BigInt (BigInt, fromBase, fromInt)
import Data.Foldable (sum, foldr)
import Data.LGroup (LGroup(LGroup))
import Data.Maybe (fromMaybe)
import Data.Modular (mJoin, mPow, mMul, mProd, mMod)
import Prelude

import Debug.Trace

type PrivateKey = BigInt
type PublicKey = BigInt
type Ring = Array PublicKey
data Party = Party PublicKey PrivateKey
data Signature = Signature BigInt BigInt (Array BigInt)

instance showSignature :: Show Signature where
  show (Signature a b c) = "(Signature "<>show a<>" "<>show b<>" "<>show c<>")"

-- Primitive algorithms

makePublicKey :: LGroup -> PrivateKey -> PublicKey 
makePublicKey (LGroup g q p) priv = mPow p g priv

pureSign :: Array BigInt -> LGroup -> Ring -> Party -> String -> Signature
pureSign rands lGroup@(LGroup g q p) y (Party pub pvt) m = sig where
  e   = map (_ `mMod` q) rands
  n   = fromMaybe 0 (findIndex (pub == _) y)
  r   = fromMaybe b0 (index e n)
  c   = fromMaybe e (updateAt n b0 e)
  l   = mJoin y
  h   = hash2 lGroup l
  y0  = mPow p h pvt
  sc  = sum c
  a   = mMul p (mPow p g r) (mProd p (zipWith (mPow p) y c))
  b   = mMul p (mPow p h r) (mPow p y0 sc)
  h1  = hash1 lGroup (l <> mJoin[y0] <> m <> mJoin[a,b])
  ci  = (h1 - sc) `mMod` q
  s   = (r - ci*pvt) `mMod` q
  sig = Signature y0 s (fromMaybe c (updateAt n ci c))
  -- Leaving this here for debug purposes
  -- This should hold: aa == ab, ac == ad
  -- c_ = fromMaybe c (updateAt n ci c)
  -- aa = spy a
  -- ab = spy (mMul p (mPow p g s) (mProd p (zipWith (mPow p) y c_)))
  -- ac = spy b
  -- ad = spy (mMul p (mPow p h s) (mPow p y0 (sum c_)))

pureVerify :: LGroup -> Ring -> Signature -> String -> Boolean
pureVerify lGroup@(LGroup g q p) y (Signature y0 s c) m = sum c `mod` q == h1 where
  l  = mJoin y
  h  = hash2 lGroup l
  a  = mMul p (mPow p g s) (mProd p (zipWith (mPow p) y c))
  b  = mMul p (mPow p h s) (mPow p y0 (sum c))
  h1 = hash1 lGroup (l <> mJoin[y0] <> m <> mJoin[a, b])

pureLink :: Signature -> Signature -> Boolean
pureLink (Signature y0a _ _) (Signature y0b _ _) = y0a == y0b

-- Hash functions

-- FIXME: max group order = 2560-128 bits
hash :: LGroup -> String -> String -> BigInt
hash (LGroup g q p) salt bits = mMod bigInt q where
  bigInt         = fromMaybe b0 (fromBase 16 hashed2560bits)
  distinctHashes = map (\x -> hmacSha512 x (salt<>bits)) (map (salt<>_) ["a","b","c","d","e"])
  hashed2560bits = foldr (<>) "" distinctHashes

hash1 :: LGroup -> String -> BigInt
hash1 lg@(LGroup _ q _) bits = hash lg "salt1" bits

hash2 :: LGroup -> String -> BigInt
hash2 lg@(LGroup g q p) bits = (h*h) `mMod` (b2*q + b1)
  where h = hash lg "salt3" bits `mMod` (q - b2) + b2

-- Helpers

publicKey :: Party -> PublicKey
publicKey (Party pub _) = pub

privateKey :: Party -> PublicKey
privateKey (Party _ pvt) = pvt

b0 :: BigInt
b0 = fromInt 0

b1 :: BigInt
b1 = fromInt 1

b2 :: BigInt
b2 = fromInt 2
