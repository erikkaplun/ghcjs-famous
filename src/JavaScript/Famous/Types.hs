{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImplicitParams            #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE JavaScriptFFI             #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnicodeSyntax             #-}

module JavaScript.Famous.Types (
  Attributes,
  CSSClass,
  Context,
  Engine,
  IsContent,
  JSCreate (..),
  Properties,
  RenderNode,
  Renderable,
  Size (..),
  Surface,
  SurfaceProp (..),
  SurfaceProps'(..),
  SurfaceProps,
  Transform,
  StateModifier,
  StateModifierProps (..),
  StateModifierProp (..),
) where

import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import           GHCJS.DOM.Types (Node)
import           GHCJS.Foreign
import           GHCJS.Marshal   (ToJSRef, toJSRef)
import           GHCJS.Types

-- TODO: instances for JSCreate should be made derivable
class JSCreate a where
  type JSCreateType a ∷ *

  doCreateJS ∷ JSRef a → a → IO (JSCreateType a)

  createJS ∷ a → IO (JSCreateType a)
  createJS a = do
    -- prevent the result of newObj accidentally being cast to the
    -- final result type in doCreateJS and returned, causing a wrong
    -- type to be returned at runtime:
    o ← newObj :: IO (JSRef a)

    doCreateJS o a

data Engine_        ; type Engine        = JSRef Engine_
data Context_       ; type Context       = JSRef Context_
data RenderNode_    ; type RenderNode    = JSRef RenderNode_
data Surface_       ; type Surface       = JSRef Surface_
data Transform_     ; type Transform     = JSRef Transform_
data StateModifier_ ; type StateModifier = JSRef StateModifier_

data Size       = XY Int Int | X Int | Y Int | Auto

-- Surface

type CSSClass   = String
type Properties = M.Map T.Text T.Text
type Attributes = M.Map T.Text T.Text

data SurfaceProps' content = SurfaceProps' { sfSize       :: Size
                                           , sfCssClasses :: [CSSClass]
                                           , sfProperties :: Properties
                                           , sfAttributes :: Attributes
                                           , sfContent    :: content }
type SurfaceProps = SurfaceProps' JSString

data SurfaceProp content = SfSize Size
                         | SfCSSClasses [CSSClass]
                         | SfProperties Properties
                         | SfAttributes Attributes
                         | SfContent content

class ToJSRef a ⇒ Renderable a
instance Renderable Surface

class ToJSRef a ⇒ IsContent a
instance IsContent String
instance IsContent T.Text
instance IsContent JSString
instance IsContent Node

-- StateModifier

data StateModifierProps = StateModifierProps { stModTransform :: Maybe Transform }

data StateModifierProp = StModTransform Transform
