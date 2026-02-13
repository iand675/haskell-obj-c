{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that scales symbol images.
--
-- Generated bindings for @NSSymbolScaleEffect@.
module ObjC.Symbols.NSSymbolScaleEffect
  ( NSSymbolScaleEffect
  , IsNSSymbolScaleEffect(..)
  , effect
  , scaleUpEffect
  , scaleDownEffect
  , effectWithByLayer
  , effectWithWholeSymbol
  , effectSelector
  , effectWithByLayerSelector
  , effectWithWholeSymbolSelector
  , scaleDownEffectSelector
  , scaleUpEffectSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Symbols.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The default scaling effect, determined by the system.
--
-- ObjC selector: @+ effect@
effect :: IO (Id NSSymbolScaleEffect)
effect  =
  do
    cls' <- getRequiredClass "NSSymbolScaleEffect"
    sendClassMessage cls' effectSelector

-- | Convenience initializer to create a scale effect with a scale up level.
--
-- ObjC selector: @+ scaleUpEffect@
scaleUpEffect :: IO (Id NSSymbolScaleEffect)
scaleUpEffect  =
  do
    cls' <- getRequiredClass "NSSymbolScaleEffect"
    sendClassMessage cls' scaleUpEffectSelector

-- | Convenience initializer to create a scale effect with a scale down level.
--
-- ObjC selector: @+ scaleDownEffect@
scaleDownEffect :: IO (Id NSSymbolScaleEffect)
scaleDownEffect  =
  do
    cls' <- getRequiredClass "NSSymbolScaleEffect"
    sendClassMessage cls' scaleDownEffectSelector

-- | Returns a copy of the effect that animates incrementally, by layer.
--
-- ObjC selector: @- effectWithByLayer@
effectWithByLayer :: IsNSSymbolScaleEffect nsSymbolScaleEffect => nsSymbolScaleEffect -> IO (Id NSSymbolScaleEffect)
effectWithByLayer nsSymbolScaleEffect =
  sendMessage nsSymbolScaleEffect effectWithByLayerSelector

-- | Returns a copy of the effect that animates all layers of the symbol simultaneously.
--
-- ObjC selector: @- effectWithWholeSymbol@
effectWithWholeSymbol :: IsNSSymbolScaleEffect nsSymbolScaleEffect => nsSymbolScaleEffect -> IO (Id NSSymbolScaleEffect)
effectWithWholeSymbol nsSymbolScaleEffect =
  sendMessage nsSymbolScaleEffect effectWithWholeSymbolSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effect@
effectSelector :: Selector '[] (Id NSSymbolScaleEffect)
effectSelector = mkSelector "effect"

-- | @Selector@ for @scaleUpEffect@
scaleUpEffectSelector :: Selector '[] (Id NSSymbolScaleEffect)
scaleUpEffectSelector = mkSelector "scaleUpEffect"

-- | @Selector@ for @scaleDownEffect@
scaleDownEffectSelector :: Selector '[] (Id NSSymbolScaleEffect)
scaleDownEffectSelector = mkSelector "scaleDownEffect"

-- | @Selector@ for @effectWithByLayer@
effectWithByLayerSelector :: Selector '[] (Id NSSymbolScaleEffect)
effectWithByLayerSelector = mkSelector "effectWithByLayer"

-- | @Selector@ for @effectWithWholeSymbol@
effectWithWholeSymbolSelector :: Selector '[] (Id NSSymbolScaleEffect)
effectWithWholeSymbolSelector = mkSelector "effectWithWholeSymbol"

