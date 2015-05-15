{-# LANGUAGE JavaScriptFFI    #-}
{-# LANGUAGE UnicodeSyntax    #-}

module JavaScript.Famous.Internal where

import           JavaScript.Famous.Types

import           GHCJS.DOM.HTMLElement
import           GHCJS.Types

foreign import javascript unsafe "console.debug($1)"
  jsDebug ∷ JSRef a → IO ()

foreign import javascript unsafe "famous.core.Engine"
  engine ∷ Engine

foreign import javascript safe "($2).createContext($1)"
  fms_Engine_createContext1 ∷ JSRef HTMLElement → Engine → IO Context

foreign import javascript safe "($1).createContext()"
  fms_Engine_createContext0 ∷                     Engine → IO Context

foreign import javascript safe "$2.add($1)"
  fms_Context_add ∷ JSRef a → Context → IO RenderNode

foreign import javascript safe "$2.add($1)"
  fms_RenderNode_add ∷ JSRef a → RenderNode → IO RenderNode

foreign import javascript safe "new famous.core.Surface($1)"
  fms_Surface_new ∷ JSRef (SurfaceProps' a) → IO Surface

foreign import javascript safe "$2.setContent($1)"
  fms_Surface_setContent ∷ JSRef a → Surface → IO ()

foreign import javascript safe "famous.core.Transform.translate($1, $2, $3)"
  fms_Transform_translate ∷ JSNumber → JSNumber → JSNumber → IO Transform

foreign import javascript safe "new famous.modifiers.StateModifier($1)"
  fms_StateModifier_new ∷ JSRef StateModifierProps → IO StateModifier
