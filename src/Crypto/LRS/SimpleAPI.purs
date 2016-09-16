-- | Linkable Ring Signatures as defined on http://link.springer.com/chapter/10.1007%2F11424826_65
-- |
-- | The functions exported here use, by default, the group 6.2 given in RFC 2409, 
-- | available at https://tools.ietf.org/html/rfc2409#section-6.2, and, as the source 
-- | of entropy, the `secure-random` package on npm. If you want different parameters,
-- | use Data.PureLRS instead.

module Crypto.LRS.SimpleAPI
  ( module Crypto.LRS
  , gen
  , sign
  , link
  , verify
  , module Data.BigInt.Random
  ) where

import Control.Monad.Eff (Eff)
import Crypto.LRS (Ring, Party(..), Signature(..), makePublicKey, pureSign, pureVerify, pureLink, publicKey, privateKey)
import Data.Array (length)
import Data.BigInt (BigInt)
import Data.BigInt.Random (RANDOM, random)
import Data.LGroup (LGroup(..), lgMedium)
import Data.Modular (mMod)
import Data.Unfoldable (replicateA)
import Prelude (bind, pure, map)

-- The default multiplicative group used
defaultLGroup :: LGroup
defaultLGroup = lgMedium -- could be lgEasy/lgHard, or your own group

-- Creates a new party (public/private key pairs).
gen :: forall e . Eff (random :: RANDOM | e) Party
gen = do
  (LGroup _ q _) <- pure defaultLGroup
  priv :: BigInt <- map (_ `mMod` q) (random 256)
  pure (Party (makePublicKey defaultLGroup priv) priv)

-- Signs a message in name of a ring (an array of parties).
sign :: Ring -> Party -> String -> forall e . Eff (random :: RANDOM | e) Signature
sign ring party message = do
  rands :: Array BigInt <- replicateA (length ring) (random 256)
  pure (pureSign rands defaultLGroup ring party message)

-- Asserts that one of the members of a ring signed a message.
verify :: Ring -> Signature -> String -> Boolean
verify = pureVerify defaultLGroup

-- Verifies if two signatures were signed by the same party.
link :: Signature -> Signature -> Boolean
link = pureLink
