{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that applies the Breathe animation to symbol images.
--
-- The Breathe animation smoothly scales a symbol up and down.
--
-- Generated bindings for @NSSymbolBreatheEffect@.
module ObjC.Symbols.NSSymbolBreatheEffect
  ( NSSymbolBreatheEffect
  , IsNSSymbolBreatheEffect(..)
  , effect
  , breathePulseEffect
  , breathePlainEffect
  , effectWithByLayer
  , effectWithWholeSymbol
  , breathePlainEffectSelector
  , breathePulseEffectSelector
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

-- | The default breathe effect, determined by the system.
--
-- ObjC selector: @+ effect@
effect :: IO (Id NSSymbolBreatheEffect)
effect  =
  do
    cls' <- getRequiredClass "NSSymbolBreatheEffect"
    sendClassMessage cls' effectSelector

-- | Convenience initializer for a breathe effect that pulses layers as they breathe.
--
-- ObjC selector: @+ breathePulseEffect@
breathePulseEffect :: IO (Id NSSymbolBreatheEffect)
breathePulseEffect  =
  do
    cls' <- getRequiredClass "NSSymbolBreatheEffect"
    sendClassMessage cls' breathePulseEffectSelector

-- | Convenience initializer for a breathe effect that makes the symbol breathe with no other styling.
--
-- ObjC selector: @+ breathePlainEffect@
breathePlainEffect :: IO (Id NSSymbolBreatheEffect)
breathePlainEffect  =
  do
    cls' <- getRequiredClass "NSSymbolBreatheEffect"
    sendClassMessage cls' breathePlainEffectSelector

-- | Returns a copy of the effect that animates incrementally, by layer.
--
-- ObjC selector: @- effectWithByLayer@
effectWithByLayer :: IsNSSymbolBreatheEffect nsSymbolBreatheEffect => nsSymbolBreatheEffect -> IO (Id NSSymbolBreatheEffect)
effectWithByLayer nsSymbolBreatheEffect =
  sendMessage nsSymbolBreatheEffect effectWithByLayerSelector

-- | Returns a copy of the effect that animates all layers of the symbol simultaneously.
--
-- ObjC selector: @- effectWithWholeSymbol@
effectWithWholeSymbol :: IsNSSymbolBreatheEffect nsSymbolBreatheEffect => nsSymbolBreatheEffect -> IO (Id NSSymbolBreatheEffect)
effectWithWholeSymbol nsSymbolBreatheEffect =
  sendMessage nsSymbolBreatheEffect effectWithWholeSymbolSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effect@
effectSelector :: Selector '[] (Id NSSymbolBreatheEffect)
effectSelector = mkSelector "effect"

-- | @Selector@ for @breathePulseEffect@
breathePulseEffectSelector :: Selector '[] (Id NSSymbolBreatheEffect)
breathePulseEffectSelector = mkSelector "breathePulseEffect"

-- | @Selector@ for @breathePlainEffect@
breathePlainEffectSelector :: Selector '[] (Id NSSymbolBreatheEffect)
breathePlainEffectSelector = mkSelector "breathePlainEffect"

-- | @Selector@ for @effectWithByLayer@
effectWithByLayerSelector :: Selector '[] (Id NSSymbolBreatheEffect)
effectWithByLayerSelector = mkSelector "effectWithByLayer"

-- | @Selector@ for @effectWithWholeSymbol@
effectWithWholeSymbolSelector :: Selector '[] (Id NSSymbolBreatheEffect)
effectWithWholeSymbolSelector = mkSelector "effectWithWholeSymbol"

