{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that applies the Pulse animation to symbol images.
--
-- The Pulse animation fades the opacity of either all layers in the symbol, or of a subset of the layers in the symbol.
--
-- Generated bindings for @NSSymbolPulseEffect@.
module ObjC.Symbols.NSSymbolPulseEffect
  ( NSSymbolPulseEffect
  , IsNSSymbolPulseEffect(..)
  , effect
  , effectWithByLayer
  , effectWithWholeSymbol
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

-- | The default pulse effect, determined by the system.
--
-- ObjC selector: @+ effect@
effect :: IO (Id NSSymbolPulseEffect)
effect  =
  do
    cls' <- getRequiredClass "NSSymbolPulseEffect"
    sendClassMessage cls' effectSelector

-- | Returns a copy of the effect that only animates annotated pulse layers.
--
-- ObjC selector: @- effectWithByLayer@
effectWithByLayer :: IsNSSymbolPulseEffect nsSymbolPulseEffect => nsSymbolPulseEffect -> IO (Id NSSymbolPulseEffect)
effectWithByLayer nsSymbolPulseEffect =
  sendMessage nsSymbolPulseEffect effectWithByLayerSelector

-- | Returns a copy of the effect that animates all layers of the symbol simultaneously.
--
-- ObjC selector: @- effectWithWholeSymbol@
effectWithWholeSymbol :: IsNSSymbolPulseEffect nsSymbolPulseEffect => nsSymbolPulseEffect -> IO (Id NSSymbolPulseEffect)
effectWithWholeSymbol nsSymbolPulseEffect =
  sendMessage nsSymbolPulseEffect effectWithWholeSymbolSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effect@
effectSelector :: Selector '[] (Id NSSymbolPulseEffect)
effectSelector = mkSelector "effect"

-- | @Selector@ for @effectWithByLayer@
effectWithByLayerSelector :: Selector '[] (Id NSSymbolPulseEffect)
effectWithByLayerSelector = mkSelector "effectWithByLayer"

-- | @Selector@ for @effectWithWholeSymbol@
effectWithWholeSymbolSelector :: Selector '[] (Id NSSymbolPulseEffect)
effectWithWholeSymbolSelector = mkSelector "effectWithWholeSymbol"

