{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.Symbols.Internal.Classes (
    module ObjC.Symbols.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- NSSymbolEffectOptionsRepeatBehavior ----------

-- | The behavior of repetition to use when a symbol effect is animating.
-- 
-- Phantom type for @NSSymbolEffectOptionsRepeatBehavior@.
data NSSymbolEffectOptionsRepeatBehavior

instance IsObjCObject (Id NSSymbolEffectOptionsRepeatBehavior) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSymbolEffectOptionsRepeatBehavior"

class IsNSObject a => IsNSSymbolEffectOptionsRepeatBehavior a where
  toNSSymbolEffectOptionsRepeatBehavior :: a -> Id NSSymbolEffectOptionsRepeatBehavior

instance IsNSSymbolEffectOptionsRepeatBehavior (Id NSSymbolEffectOptionsRepeatBehavior) where
  toNSSymbolEffectOptionsRepeatBehavior = unsafeCastId

instance IsNSObject (Id NSSymbolEffectOptionsRepeatBehavior) where
  toNSObject = unsafeCastId
