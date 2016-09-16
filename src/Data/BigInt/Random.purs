module Data.BigInt.Random where

import Prelude

import Control.Monad.Eff (Eff)
import Data.BigInt (BigInt)

foreign import data RANDOM :: !

foreign import random :: Int -> forall e. Eff (random :: RANDOM | e) BigInt
