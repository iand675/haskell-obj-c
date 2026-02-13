{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that applies the Bounce animation to symbol images.
--
-- The Bounce animation applies a transitory scaling effect to the symbol.
--
-- Generated bindings for @NSSymbolBounceEffect@.
module ObjC.Symbols.NSSymbolBounceEffect
  ( NSSymbolBounceEffect
  , IsNSSymbolBounceEffect(..)
  , effect
  , bounceUpEffect
  , bounceDownEffect
  , effectWithByLayer
  , effectWithWholeSymbol
  , bounceDownEffectSelector
  , bounceUpEffectSelector
  , effectSelector
  , effectWithByLayerSelector
  , effectWithWholeSymbolSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Symbols.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The default bounce effect, determined by the system.
--
-- ObjC selector: @+ effect@
effect :: IO (Id NSSymbolBounceEffect)
effect  =
  do
    cls' <- getRequiredClass "NSSymbolBounceEffect"
    sendClassMessage cls' effectSelector

-- | Convenience initializer for a bounce effect that bounces up.
--
-- ObjC selector: @+ bounceUpEffect@
bounceUpEffect :: IO (Id NSSymbolBounceEffect)
bounceUpEffect  =
  do
    cls' <- getRequiredClass "NSSymbolBounceEffect"
    sendClassMessage cls' bounceUpEffectSelector

-- | Convenience initializer for a bounce effect that bounces down.
--
-- ObjC selector: @+ bounceDownEffect@
bounceDownEffect :: IO (Id NSSymbolBounceEffect)
bounceDownEffect  =
  do
    cls' <- getRequiredClass "NSSymbolBounceEffect"
    sendClassMessage cls' bounceDownEffectSelector

-- | Returns a copy of the effect that animates incrementally, by layer.
--
-- ObjC selector: @- effectWithByLayer@
effectWithByLayer :: IsNSSymbolBounceEffect nsSymbolBounceEffect => nsSymbolBounceEffect -> IO (Id NSSymbolBounceEffect)
effectWithByLayer nsSymbolBounceEffect =
  sendMessage nsSymbolBounceEffect effectWithByLayerSelector

-- | Returns a copy of the effect that animates all layers of the symbol simultaneously.
--
-- ObjC selector: @- effectWithWholeSymbol@
effectWithWholeSymbol :: IsNSSymbolBounceEffect nsSymbolBounceEffect => nsSymbolBounceEffect -> IO (Id NSSymbolBounceEffect)
effectWithWholeSymbol nsSymbolBounceEffect =
  sendMessage nsSymbolBounceEffect effectWithWholeSymbolSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effect@
effectSelector :: Selector '[] (Id NSSymbolBounceEffect)
effectSelector = mkSelector "effect"

-- | @Selector@ for @bounceUpEffect@
bounceUpEffectSelector :: Selector '[] (Id NSSymbolBounceEffect)
bounceUpEffectSelector = mkSelector "bounceUpEffect"

-- | @Selector@ for @bounceDownEffect@
bounceDownEffectSelector :: Selector '[] (Id NSSymbolBounceEffect)
bounceDownEffectSelector = mkSelector "bounceDownEffect"

-- | @Selector@ for @effectWithByLayer@
effectWithByLayerSelector :: Selector '[] (Id NSSymbolBounceEffect)
effectWithByLayerSelector = mkSelector "effectWithByLayer"

-- | @Selector@ for @effectWithWholeSymbol@
effectWithWholeSymbolSelector :: Selector '[] (Id NSSymbolBounceEffect)
effectWithWholeSymbolSelector = mkSelector "effectWithWholeSymbol"

