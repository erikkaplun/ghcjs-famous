{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module JavaScript.Famous.Highlevel (
  jsDebug,
  engine,
  createContext,
  newSurface,
  addToContext,
) where

import           JavaScript.Famous.Internal
import           JavaScript.Famous.Types

import           GHCJS.DOM.HTMLElement
import           GHCJS.Types

createContext ∷ Maybe (JSRef HTMLElement) → IO Context
createContext (Just el) = fms_Engine_createContext1 el engine
createContext Nothing   = fms_Engine_createContext0    engine

addToContext ∷ Renderable (JSRef r) ⇒ JSRef r → Context → IO RenderNode
addToContext = fms_Context_add

newSurface ∷ JSRef (SurfaceProps' a) → IO Surface
newSurface = fms_Surface_new
