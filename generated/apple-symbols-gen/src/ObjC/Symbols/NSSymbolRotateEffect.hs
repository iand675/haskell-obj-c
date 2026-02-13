{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that applies the Rotate animation to symbol images.
--
-- The Rotate animation rotates parts of a symbol around a symbol-provided anchor point.
--
-- Generated bindings for @NSSymbolRotateEffect@.
module ObjC.Symbols.NSSymbolRotateEffect
  ( NSSymbolRotateEffect
  , IsNSSymbolRotateEffect(..)
  , effect
  , rotateClockwiseEffect
  , rotateCounterClockwiseEffect
  , effectWithByLayer
  , effectWithWholeSymbol
  , effectSelector
  , effectWithByLayerSelector
  , effectWithWholeSymbolSelector
  , rotateClockwiseEffectSelector
  , rotateCounterClockwiseEffectSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Symbols.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The default rotate effect, determined by the system.
--
-- ObjC selector: @+ effect@
effect :: IO (Id NSSymbolRotateEffect)
effect  =
  do
    cls' <- getRequiredClass "NSSymbolRotateEffect"
    sendClassMessage cls' effectSelector

-- | Convenience initializer for a rotate effect that rotates clockwise.
--
-- ObjC selector: @+ rotateClockwiseEffect@
rotateClockwiseEffect :: IO (Id NSSymbolRotateEffect)
rotateClockwiseEffect  =
  do
    cls' <- getRequiredClass "NSSymbolRotateEffect"
    sendClassMessage cls' rotateClockwiseEffectSelector

-- | Convenience initializer for a rotate effect that rotates counter-clockwise.
--
-- ObjC selector: @+ rotateCounterClockwiseEffect@
rotateCounterClockwiseEffect :: IO (Id NSSymbolRotateEffect)
rotateCounterClockwiseEffect  =
  do
    cls' <- getRequiredClass "NSSymbolRotateEffect"
    sendClassMessage cls' rotateCounterClockwiseEffectSelector

-- | Returns a copy of the effect that animates incrementally, by layer.
--
-- ObjC selector: @- effectWithByLayer@
effectWithByLayer :: IsNSSymbolRotateEffect nsSymbolRotateEffect => nsSymbolRotateEffect -> IO (Id NSSymbolRotateEffect)
effectWithByLayer nsSymbolRotateEffect =
  sendMessage nsSymbolRotateEffect effectWithByLayerSelector

-- | Returns a copy of the effect that animates all layers of the symbol simultaneously.
--
-- ObjC selector: @- effectWithWholeSymbol@
effectWithWholeSymbol :: IsNSSymbolRotateEffect nsSymbolRotateEffect => nsSymbolRotateEffect -> IO (Id NSSymbolRotateEffect)
effectWithWholeSymbol nsSymbolRotateEffect =
  sendMessage nsSymbolRotateEffect effectWithWholeSymbolSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effect@
effectSelector :: Selector '[] (Id NSSymbolRotateEffect)
effectSelector = mkSelector "effect"

-- | @Selector@ for @rotateClockwiseEffect@
rotateClockwiseEffectSelector :: Selector '[] (Id NSSymbolRotateEffect)
rotateClockwiseEffectSelector = mkSelector "rotateClockwiseEffect"

-- | @Selector@ for @rotateCounterClockwiseEffect@
rotateCounterClockwiseEffectSelector :: Selector '[] (Id NSSymbolRotateEffect)
rotateCounterClockwiseEffectSelector = mkSelector "rotateCounterClockwiseEffect"

-- | @Selector@ for @effectWithByLayer@
effectWithByLayerSelector :: Selector '[] (Id NSSymbolRotateEffect)
effectWithByLayerSelector = mkSelector "effectWithByLayer"

-- | @Selector@ for @effectWithWholeSymbol@
effectWithWholeSymbolSelector :: Selector '[] (Id NSSymbolRotateEffect)
effectWithWholeSymbolSelector = mkSelector "effectWithWholeSymbol"

