{-# LANGUAGE UnicodeSyntax #-}
module JavaScript.Famous.Transform (
  translate,
) where

import           JavaScript.Famous.Highlevel (transformTranslate)
import           JavaScript.Famous.Types

import           GHCJS.Marshal
import           GHCJS.Types
import           Unsafe.Coerce

translate ∷ Int → Int → Int → IO Transform
translate x y z = do
  x' ← toJSRef x
  y' ← toJSRef y
  z' ← toJSRef z
  transformTranslate (unsafeCoerce x') (unsafeCoerce y') (unsafeCoerce z')
