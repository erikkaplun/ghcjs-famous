{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module JavaScript.Famous.Modifier (
  stateModifier,
  modify
) where

import           JavaScript.Famous.Types
import           JavaScript.Famous.Highlevel (modify, newStateModifier)

import           Data.Default
import           GHCJS.Foreign           (newObj, setProp)
import           GHCJS.Marshal           (toJSRef)
import           GHCJS.Types

stateModifier ∷ [StateModifierProp] → IO StateModifier
stateModifier props = createJS $ foldr step def props
  where step p s = case p of StModTransform x → s { stModTransform = Just x }

instance Default StateModifierProps where
  def = StateModifierProps { stModTransform = Nothing }

instance JSCreate StateModifierProps where
  type JSCreateType StateModifierProps = StateModifier
  doCreateJS o StateModifierProps { .. } = do
    let (.=) ∷ JSString → JSRef a → IO ()
        k .= v = setProp k v o
    ("transform" .=) =<< toJSRef stModTransform
    newStateModifier o
