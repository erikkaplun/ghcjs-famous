{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module JavaScript.Famous.Examples where

import           JavaScript.Famous

import           Control.Applicative ((<$>))
import           Control.Concurrent  (threadDelay)
import           Control.Monad       (forM_)
import           Control.Monad.Maybe (MaybeT (MaybeT), runMaybeT)
import           Control.Monad.Trans (lift)
import           Data.Map.Strict     as M
import           Data.String         (fromString)
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

  trans ← translate 150 100 0
  mod ← stateModifier [StModTransform $ trans]
  node ← mod `modify` ctx
  sf `addToRenderNode` node

  forM_ [1..] $ \i -> do
    threadDelay 1000000
    surfaceSetContent (newContent i) sf
      where newContent i = fromString $ "hello " ++ show i ∷ JSString

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
