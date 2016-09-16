#### Installing

    bower install purescript-lrs

#### API

```haskell
-- Creates a new party (public/private key pairs).
gen :: forall e . Eff (random :: RANDOM | e) Party

-- Signs a message in name of a ring (an array of parties).
sign :: Ring -> Party -> String -> forall e . Eff (random :: RANDOM | e) Signature

-- Asserts that one of the members of a ring signed a message.
verify :: Ring -> Signature -> String -> Boolean

-- Verifies if two signatures were signed by the same party.
link :: Signature -> Signature -> Boolean
```

#### Example

```haskell
import Crypto.LRS.SimpleAPI
...

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
```

Check [Main.purs](https://github.com/MaiaVictor/purescript-lrs/blob/master/src/Main.purs) for the compiling exmaple.
