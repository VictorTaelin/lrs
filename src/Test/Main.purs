module Test.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Crypto.LRS.SimpleAPI (Signature(..), Party(..), gen, sign, link, verify, publicKey, privateKey)
import Data.Array (tail, range, index, length)
import Data.BigInt (toBase, fromInt)
import Data.BigInt.Random (RANDOM)
import Data.Maybe (fromMaybe)
import Data.String (joinWith)
import Data.Unfoldable (replicateA)
import Prelude
import Test.Assert (ASSERT, assert)

formatParty :: Party -> String
formatParty (Party pub pvt) = toBase 16 pub<>"/"<>toBase 16 pvt

formatSignature :: Signature -> String
formatSignature (Signature a b cs) 
  =  toBase 16 a <> "|"
  <> toBase 16 b <> "|" <>
  joinWith ":" (map (toBase 16) cs)

main :: forall e. Eff (assert :: ASSERT, random :: RANDOM, console :: CONSOLE | e) Unit
main = do

  log "\n:: Generating ring with 16 members..."
  ring <- replicateA 16 gen
  let pvts = map privateKey ring
  let pubs = map publicKey ring
  let party n = fromMaybe (Party (fromInt 0) (fromInt 0)) (index ring n)

  -- Gen
  log $ "\n:: Generated PUB/PVT key pairs: "
  log $ joinWith "\n" $ map
    (\i -> "MEMBER "<>show i<>": "<> formatParty (party i))
    (range 0 (length ring-1))

  log "\n:: Generating test signatures..."
  sig0 <- sign pubs (party 0) "testMsg0"
  sig1 <- sign pubs (party 1) "testMsg1"
  sig2 <- sign pubs (party 2) "testMsg2"
  sig3 <- sign pubs (party 0) "testMsg3"

  log "\n:: Generated signatures:"
  log $ "MSG: 'testMsg0' (from member 0/16); SIG: " <> formatSignature sig0
  log $ "MSG: 'testMsg1' (from member 1/16); SIG: " <> formatSignature sig1
  log $ "MSG: 'testMsg2' (from member 2/16); SIG: " <> formatSignature sig2
  log $ "MSG: 'testMsg3' (from member 0/16); SIG: " <> formatSignature sig3

  let testSig conf sig i msg = do
        let fn   = if conf then id else not
        let verb = if conf then "proves" else "doesn't prove"
        assert $ fn (verify pubs sig msg)
        log ("signature "<>show i<>" "<>verb<>" someone signed '"<>msg<>"'")

  log "\n:: Testing if signatures verify their messages."
  testSig true sig0 0 "testMsg0"
  testSig true sig1 1 "testMsg1"
  testSig true sig2 2 "testMsg2"
  testSig true sig3 3 "testMsg3"

  log "\n:: Testing if signatures fail to verify false messages."
  testSig false sig3 0 "arbitraryMsg"
  testSig false sig3 1 "arbitraryMsg"
  testSig false sig3 2 "arbitraryMsg"
  testSig false sig3 3 "arbitraryMsg"

  log "\n:: Testing if signatures fail to verify another signature's messages."
  testSig false sig0 0 "testMsg1"
  testSig false sig0 0 "testMsg2"
  testSig false sig0 0 "testMsg3"
  testSig false sig1 1 "testMsg0"
  testSig false sig1 1 "testMsg2"
  testSig false sig1 1 "testMsg3"
  testSig false sig2 2 "testMsg0"
  testSig false sig2 2 "testMsg1"
  testSig false sig2 2 "testMsg3"
  testSig false sig3 3 "testMsg0"
  testSig false sig3 3 "testMsg1"
  testSig false sig3 3 "testMsg2"

  log "\n:: Testing if signatures fail to verify their messages, but with different rings."
  let subpubs = fromMaybe [] (tail pubs)
  assert $ not (verify subpubs sig0 "testMsg0")
  assert $ not (verify subpubs sig1 "testMsg1")
  assert $ not (verify subpubs sig2 "testMsg2")
  assert $ not (verify subpubs sig3 "testMsg3")
  log "all signatures failed to verify their messages for different rings"

  log "\n:: Testing if the linked signature is accused."
  assert $ link sig0 sig3
  log "signatures 0 and 3 were signed by the same person"

  log "\n::Testing if unlinked signatures aren't accused."
  assert $ not (link sig0 sig1)
  assert $ not (link sig0 sig2)
  assert $ not (link sig1 sig2)
  assert $ not (link sig1 sig3)
  assert $ not (link sig2 sig3)
  log "no other pair of signatures was signed by the same person"
  log ""
