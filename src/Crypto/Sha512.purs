module Crypto.Sha512 where

foreign import sha512 :: String -> String
foreign import hmacSha512 :: String -> String -> String

