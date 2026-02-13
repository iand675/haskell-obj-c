{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that applies the Wiggle animation to symbol images.
--
-- The Wiggle animation applies a transitory translation or rotation effect to the symbol.
--
-- Generated bindings for @NSSymbolWiggleEffect@.
module ObjC.Symbols.NSSymbolWiggleEffect
  ( NSSymbolWiggleEffect
  , IsNSSymbolWiggleEffect(..)
  , effect
  , wiggleClockwiseEffect
  , wiggleCounterClockwiseEffect
  , wiggleLeftEffect
  , wiggleRightEffect
  , wiggleUpEffect
  , wiggleDownEffect
  , wiggleForwardEffect
  , wiggleBackwardEffect
  , wiggleCustomAngleEffect
  , effectWithByLayer
  , effectWithWholeSymbol
  , effectSelector
  , effectWithByLayerSelector
  , effectWithWholeSymbolSelector
  , wiggleBackwardEffectSelector
  , wiggleClockwiseEffectSelector
  , wiggleCounterClockwiseEffectSelector
  , wiggleCustomAngleEffectSelector
  , wiggleDownEffectSelector
  , wiggleForwardEffectSelector
  , wiggleLeftEffectSelector
  , wiggleRightEffectSelector
  , wiggleUpEffectSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Symbols.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The default wiggle effect, determined by the system.
--
-- ObjC selector: @+ effect@
effect :: IO (Id NSSymbolWiggleEffect)
effect  =
  do
    cls' <- getRequiredClass "NSSymbolWiggleEffect"
    sendClassMessage cls' effectSelector

-- | Convenience initializer for a wiggle effect that rotates back and forth, starting by rotating clockwise.
--
-- ObjC selector: @+ wiggleClockwiseEffect@
wiggleClockwiseEffect :: IO (Id NSSymbolWiggleEffect)
wiggleClockwiseEffect  =
  do
    cls' <- getRequiredClass "NSSymbolWiggleEffect"
    sendClassMessage cls' wiggleClockwiseEffectSelector

-- | Convenience initializer for a wiggle effect that rotates back and forth, starting by rotating counter-clockwise.
--
-- ObjC selector: @+ wiggleCounterClockwiseEffect@
wiggleCounterClockwiseEffect :: IO (Id NSSymbolWiggleEffect)
wiggleCounterClockwiseEffect  =
  do
    cls' <- getRequiredClass "NSSymbolWiggleEffect"
    sendClassMessage cls' wiggleCounterClockwiseEffectSelector

-- | Convenience initializer for a wiggle effect that moves back and forth horizontally, starting by moving left.
--
-- ObjC selector: @+ wiggleLeftEffect@
wiggleLeftEffect :: IO (Id NSSymbolWiggleEffect)
wiggleLeftEffect  =
  do
    cls' <- getRequiredClass "NSSymbolWiggleEffect"
    sendClassMessage cls' wiggleLeftEffectSelector

-- | Convenience initializer for a wiggle effect that moves back and forth horizontally, starting by moving right.
--
-- ObjC selector: @+ wiggleRightEffect@
wiggleRightEffect :: IO (Id NSSymbolWiggleEffect)
wiggleRightEffect  =
  do
    cls' <- getRequiredClass "NSSymbolWiggleEffect"
    sendClassMessage cls' wiggleRightEffectSelector

-- | Convenience initializer for a wiggle effect that moves back and forth vertically, starting by moving up.
--
-- ObjC selector: @+ wiggleUpEffect@
wiggleUpEffect :: IO (Id NSSymbolWiggleEffect)
wiggleUpEffect  =
  do
    cls' <- getRequiredClass "NSSymbolWiggleEffect"
    sendClassMessage cls' wiggleUpEffectSelector

-- | Convenience initializer for a wiggle effect that moves back and forth vertically, starting by moving down.
--
-- ObjC selector: @+ wiggleDownEffect@
wiggleDownEffect :: IO (Id NSSymbolWiggleEffect)
wiggleDownEffect  =
  do
    cls' <- getRequiredClass "NSSymbolWiggleEffect"
    sendClassMessage cls' wiggleDownEffectSelector

-- | Convenience initializer for a wiggle effect that moves back and forth horizontally based on the current locale, starting by moving forward.
--
-- ObjC selector: @+ wiggleForwardEffect@
wiggleForwardEffect :: IO (Id NSSymbolWiggleEffect)
wiggleForwardEffect  =
  do
    cls' <- getRequiredClass "NSSymbolWiggleEffect"
    sendClassMessage cls' wiggleForwardEffectSelector

-- | Convenience initializer for a wiggle effect that moves back and forth horizontally based on the current locale, starting by moving backward.
--
-- ObjC selector: @+ wiggleBackwardEffect@
wiggleBackwardEffect :: IO (Id NSSymbolWiggleEffect)
wiggleBackwardEffect  =
  do
    cls' <- getRequiredClass "NSSymbolWiggleEffect"
    sendClassMessage cls' wiggleBackwardEffectSelector

-- | Convenience initializer for a wiggle effect that moves back and forth along an axis, starting by moving toward a custom angle.
--
-- The angle is in degrees moving clockwise from the positive x-axis.
--
-- ObjC selector: @+ wiggleCustomAngleEffect:@
wiggleCustomAngleEffect :: CDouble -> IO (Id NSSymbolWiggleEffect)
wiggleCustomAngleEffect angle =
  do
    cls' <- getRequiredClass "NSSymbolWiggleEffect"
    sendClassMessage cls' wiggleCustomAngleEffectSelector angle

-- | Returns a copy of the effect that animates incrementally, by layer.
--
-- ObjC selector: @- effectWithByLayer@
effectWithByLayer :: IsNSSymbolWiggleEffect nsSymbolWiggleEffect => nsSymbolWiggleEffect -> IO (Id NSSymbolWiggleEffect)
effectWithByLayer nsSymbolWiggleEffect =
  sendMessage nsSymbolWiggleEffect effectWithByLayerSelector

-- | Returns a copy of the effect that animates all layers of the symbol simultaneously.
--
-- ObjC selector: @- effectWithWholeSymbol@
effectWithWholeSymbol :: IsNSSymbolWiggleEffect nsSymbolWiggleEffect => nsSymbolWiggleEffect -> IO (Id NSSymbolWiggleEffect)
effectWithWholeSymbol nsSymbolWiggleEffect =
  sendMessage nsSymbolWiggleEffect effectWithWholeSymbolSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effect@
effectSelector :: Selector '[] (Id NSSymbolWiggleEffect)
effectSelector = mkSelector "effect"

-- | @Selector@ for @wiggleClockwiseEffect@
wiggleClockwiseEffectSelector :: Selector '[] (Id NSSymbolWiggleEffect)
wiggleClockwiseEffectSelector = mkSelector "wiggleClockwiseEffect"

-- | @Selector@ for @wiggleCounterClockwiseEffect@
wiggleCounterClockwiseEffectSelector :: Selector '[] (Id NSSymbolWiggleEffect)
wiggleCounterClockwiseEffectSelector = mkSelector "wiggleCounterClockwiseEffect"

-- | @Selector@ for @wiggleLeftEffect@
wiggleLeftEffectSelector :: Selector '[] (Id NSSymbolWiggleEffect)
wiggleLeftEffectSelector = mkSelector "wiggleLeftEffect"

-- | @Selector@ for @wiggleRightEffect@
wiggleRightEffectSelector :: Selector '[] (Id NSSymbolWiggleEffect)
wiggleRightEffectSelector = mkSelector "wiggleRightEffect"

-- | @Selector@ for @wiggleUpEffect@
wiggleUpEffectSelector :: Selector '[] (Id NSSymbolWiggleEffect)
wiggleUpEffectSelector = mkSelector "wiggleUpEffect"

-- | @Selector@ for @wiggleDownEffect@
wiggleDownEffectSelector :: Selector '[] (Id NSSymbolWiggleEffect)
wiggleDownEffectSelector = mkSelector "wiggleDownEffect"

-- | @Selector@ for @wiggleForwardEffect@
wiggleForwardEffectSelector :: Selector '[] (Id NSSymbolWiggleEffect)
wiggleForwardEffectSelector = mkSelector "wiggleForwardEffect"

-- | @Selector@ for @wiggleBackwardEffect@
wiggleBackwardEffectSelector :: Selector '[] (Id NSSymbolWiggleEffect)
wiggleBackwardEffectSelector = mkSelector "wiggleBackwardEffect"

-- | @Selector@ for @wiggleCustomAngleEffect:@
wiggleCustomAngleEffectSelector :: Selector '[CDouble] (Id NSSymbolWiggleEffect)
wiggleCustomAngleEffectSelector = mkSelector "wiggleCustomAngleEffect:"

-- | @Selector@ for @effectWithByLayer@
effectWithByLayerSelector :: Selector '[] (Id NSSymbolWiggleEffect)
effectWithByLayerSelector = mkSelector "effectWithByLayer"

-- | @Selector@ for @effectWithWholeSymbol@
effectWithWholeSymbolSelector :: Selector '[] (Id NSSymbolWiggleEffect)
effectWithWholeSymbolSelector = mkSelector "effectWithWholeSymbol"

