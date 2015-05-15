{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module JavaScript.Famous.Examples where

import           JavaScript.Famous

import           Control.Applicative ((<$>))
import           Control.Monad.Maybe (MaybeT (MaybeT), runMaybeT)
import           Control.Monad.Trans (lift)
import           Data.Map.Strict     as M
import           GHCJS.DOM           (currentDocument)
import           GHCJS.DOM.Document  (documentImportNode, documentQuerySelector)
import           GHCJS.DOM.Types     (Element, Node (..), unElement)
import           GHCJS.Foreign       (ToJSString, getProp)
import           GHCJS.Types         (JSString)

main = do
  ctx ← createContext Nothing

  Just content ← getTpl ("#main" ∷ JSString)
  sf ← surface [ SfSize       $ XY 200 200
               , SfContent    $ content
               , SfProperties $ M.fromList [ ("backgroundColor", "rgb(240, 238, 233)")
                                           , ("textAlign"      , "center") ] ]

  sf `addToContext` ctx

getTpl ∷ ToJSString sel ⇒ sel → IO (Maybe Node)
getTpl sel = runMaybeT $ do
  doc        ← MaybeT $ currentDocument
  tpl        ← MaybeT $ documentQuerySelector doc sel
  tplContent ← lift   $ templateContent tpl
  ret        ← MaybeT $ documentImportNode doc (Just tplContent) True
  return ret
    where
      templateContent tpl =
        Node <$> getProp ("content" ∷ JSString) (unElement tpl)
