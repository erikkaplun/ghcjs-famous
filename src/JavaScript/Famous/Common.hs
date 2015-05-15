{-# LANGUAGE UnicodeSyntax #-}
module JavaScript.Famous.Common where

import           JavaScript.Famous.Types

import           GHCJS.Foreign           (jsUndefined)
import           GHCJS.Marshal           (ToJSRef, toJSRef)
import           Unsafe.Coerce           (unsafeCoerce)

instance ToJSRef Size where
  toJSRef (XY x y) = unsafeCoerce $ toJSRef (x          , y          )
  toJSRef (X  x  ) = unsafeCoerce $ toJSRef (x          , jsUndefined)
  toJSRef ( Y   y) = unsafeCoerce $ toJSRef (jsUndefined, y          )
  toJSRef Auto     = unsafeCoerce $ toJSRef (jsUndefined, jsUndefined)
