{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnicodeSyntax             #-}

module JavaScript.Famous.Surface (surface) where

import           JavaScript.Famous.Common
import           JavaScript.Famous.Generic
import           JavaScript.Famous.Highlevel
import           JavaScript.Famous.Types

import           Data.Default                (Default, def)
import qualified Data.Map.Strict             as M
import qualified Data.Text                   as T
import           GHCJS.DOM                   (currentDocument)
import           GHCJS.DOM.Document          (documentCreateTextNode)
import           GHCJS.DOM.HTMLElement
import           GHCJS.DOM.Types             (Node (Node))
import           GHCJS.Foreign               (newObj, setProp)
import           GHCJS.Marshal               (ToJSRef, toJSRef)
import           GHCJS.Types
import           System.IO.Unsafe            (unsafePerformIO)
import           Unsafe.Coerce               (unsafeCoerce)

surface ∷ (IsContent a, Default (SurfaceProps' a)) ⇒ [SurfaceProp a] → IO Surface
surface props = createJS $ foldr step def props
  where step p s = case p of SfSize       x → s { sfSize       = x }
                             SfCSSClasses x → s { sfCssClasses = x }
                             SfProperties x → s { sfProperties = x }
                             SfAttributes x → s { sfAttributes = x }
                             SfContent    x → s { sfContent    = x }

instance IsContent a ⇒ JSCreate (SurfaceProps' a) where
  type JSCreateType (SurfaceProps' a) = Surface
  createJS SurfaceProps' { .. } = do
    o ← newObj
    let (.=) ∷ JSString → JSRef a → IO ()
        k .= v = setProp k v o
    ("size"       .=) =<< toJSRef sfSize
    ("cssClasses" .=) =<< toJSRef sfCssClasses
    ("properties" .=) =<< toJSRef sfProperties
    ("attributes" .=) =<< toJSRef sfAttributes
    ("content"    .=) =<< toJSRef sfContent
    newSurface o

instance Default (SurfaceProps' String) where
  def = SurfaceProps' { sfSize       = Auto
                      , sfCssClasses = []
                      , sfProperties = M.empty
                      , sfAttributes = M.empty
                      , sfContent    = "" }

instance Default (SurfaceProps' T.Text) where
  def = (def :: SurfaceProps' String) { sfContent = "" }

instance Default (SurfaceProps' JSString) where
  def = (def :: SurfaceProps' String) { sfContent = "" }

-- XXX: not sure if this is a good idea considering all this unsafe stuff
instance Default (SurfaceProps' Node) where
  def = (def ∷ SurfaceProps) { sfContent = emptyTextNode }
    where
      emptyTextNode = unsafePerformIO $ do
        Just doc  ← currentDocument
        Just node ← documentCreateTextNode doc ("" ∷ JSString)
        return $ unsafeCoerce node
