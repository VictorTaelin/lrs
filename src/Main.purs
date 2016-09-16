module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Crypto.LRS.SimpleAPI (gen, verify, sign, link, publicKey, RANDOM)
import Prelude
import Test.Assert (ASSERT, assert)

main :: forall e. Eff (assert :: ASSERT, random :: RANDOM, console :: CONSOLE | e) Unit
main = do

  -- 3 parties generate their public/private key pairs
  alice <- gen 
  bob   <- gen
  eve   <- gen

  -- The list of public key is known and distributed
  let group = map publicKey [alice, bob, eve]

  -- Alice signs a message in behalf of one of the 3
  signed <- sign group alice "The body is buried on the backyard."

  -- Anyone is able to verify *some* of them signed that message
  assert $ verify group signed "The body is buried on the backyard."
  
  -- If that same person signs another message...
  signed2 <- sign group alice "Just kidding, he is alive."

  -- We are able to tell the signature came from the same person
  assert $ link signed signed2

  -- We are still not able to tell who it was.
