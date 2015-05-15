{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE UnicodeSyntax #-}

module JavaScript.Famous.Generic where
  
import           Control.Monad   (forM_)
import qualified Data.Map.Strict as M
import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Types

instance (ToJSString k, ToJSRef v) ⇒ ToJSRef (M.Map k v) where
  toJSRef m = do
    o ← newObj
    forM_ (M.toList m) $ \case (k, v) → do v' ← toJSRef v
                                           setProp (toJSString k) v' o
    return o

