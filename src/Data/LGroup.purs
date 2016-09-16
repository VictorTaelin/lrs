module Data.LGroup where

import Data.Array (range)
import Data.BigInt (BigInt, fromInt, modPow, fromBase)
import Data.Maybe (fromMaybe)
import Data.Modular (mPow)
import Data.Set (Set, fromFoldable, toUnfoldable)
import Prelude

data LGroup = LGroup BigInt BigInt BigInt

big :: String -> BigInt
big hex = fromMaybe (fromInt 0) (fromBase 16 hex)

lGroupFromHex :: String -> String -> String -> LGroup
lGroupFromHex g q p = LGroup (big g) (big q) (big p)

validLGroup :: LGroup -> Boolean
validLGroup (LGroup g q p)
  =  g >= fromInt 2
  && g <= p - fromInt 1
  && modPow g q p == fromInt 1

-- From https://tools.ietf.org/html/rfc2409#section-6.2
lgHard :: LGroup
lgHard = LGroup (fromInt 2) (order prime) prime
  where prime = big "FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE65381FFFFFFFFFFFFFFFF"

-- From the same link
lgMedium :: LGroup 
lgMedium = LGroup (fromInt 2) (order prime) prime
  where prime = big "FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A63A3620FFFFFFFFFFFFFFFF"

-- Demo group for testing
lgEasy :: LGroup 
lgEasy = LGroup (fromInt 2) (fromInt 1559) (fromInt 3119)

-- Order of subgroup given prime p
order :: BigInt -> BigInt
order p = (p - (fromInt 1)) / (fromInt 2)

-- Checks if n is a member of the group
member :: LGroup -> BigInt -> Boolean
member (LGroup g q p) n = fromInt 1 <= n && n <= p && mPow p n q == fromInt 1

-- Finds 10000 members of the group
members :: LGroup -> Array BigInt
members (LGroup g q p) = remDups (map (mPow p g <<< fromInt) (range 0 10000))

-- Removes duplicate numbers from an array
remDups :: Array BigInt -> Array BigInt
remDups a = toUnfoldable (fromFoldable a :: Set BigInt)
