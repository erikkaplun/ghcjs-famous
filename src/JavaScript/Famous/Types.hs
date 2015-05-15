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
) where

import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import           GHCJS.DOM.Types (Node)
import           GHCJS.Foreign
import           GHCJS.Marshal   (ToJSRef, toJSRef)
import           GHCJS.Types

data Engine_    ; type Engine     = JSRef Engine_
data Context_   ; type Context    = JSRef Context_
data RenderNode_; type RenderNode = JSRef RenderNode_
data Surface_   ; type Surface    = JSRef Surface_

data Size       = XY Int Int | X Int | Y Int | Auto
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

class JSCreate a where
  type JSCreateType a ∷ *
  createJS ∷ a → IO (JSCreateType a)
