module Main where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Crypto.Subtle.Hash as Hash
import Data.ArrayBuffer.Typed as ArrayBuffer.Typed
import Data.ArrayBuffer.Types (ArrayView, Uint8)
import Data.Foldable as Foldable
import Data.Int as Int
import Data.String.Base64 as Base64
import Data.String.CodeUnits as CodeUnits
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class as Effect.Class
import Effect.Class.Console as Console
import Web.Encoding.TextEncoder as TextEncoder

main :: Effect Unit
main = Aff.launchAff_ do
  let input = "hello world"

  hashed <- sha256 input
  Console.logShow (Base64.btoa hashed)

  hashedFromJS <- Promise.toAff (_sha256 input)
  Console.logShow (Base64.btoa hashedFromJS)

foreign import _sha256 :: String -> Promise String

-- | Ported from example at https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/digest
sha256 :: String -> Aff String
sha256 input = do
  textEncoder <- Effect.Class.liftEffect TextEncoder.new

  -- encode as UTF-8
  let msgUint8 = TextEncoder.encode input textEncoder

  -- hash the message
  hashBuffer <- Hash.digest Hash.sha256 (ArrayBuffer.Typed.buffer msgUint8)

  -- convert ArrayBuffer to Array
  hashArray <- Effect.Class.liftEffect do
    hashArrayView :: ArrayView Uint8 <- ArrayBuffer.Typed.whole hashBuffer
    ArrayBuffer.Typed.toArray hashArrayView

  -- convert bytes to hex string
  let
    hashHex = hashArray
      # Foldable.foldMap (UInt.toInt >>> Int.toStringAs Int.hexadecimal >>> ("00" <> _) >>> CodeUnits.takeRight 2)

  pure hashHex

