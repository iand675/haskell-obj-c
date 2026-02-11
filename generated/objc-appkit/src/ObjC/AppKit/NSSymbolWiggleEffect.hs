{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that applies the Wiggle animation to symbol images.
--
-- The Wiggle animation applies a transitory translation or rotation effect to the symbol.
--
-- Generated bindings for @NSSymbolWiggleEffect@.
module ObjC.AppKit.NSSymbolWiggleEffect
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
  , wiggleClockwiseEffectSelector
  , wiggleCounterClockwiseEffectSelector
  , wiggleLeftEffectSelector
  , wiggleRightEffectSelector
  , wiggleUpEffectSelector
  , wiggleDownEffectSelector
  , wiggleForwardEffectSelector
  , wiggleBackwardEffectSelector
  , wiggleCustomAngleEffectSelector
  , effectWithByLayerSelector
  , effectWithWholeSymbolSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The default wiggle effect, determined by the system.
--
-- ObjC selector: @+ effect@
effect :: IO (Id NSSymbolWiggleEffect)
effect  =
  do
    cls' <- getRequiredClass "NSSymbolWiggleEffect"
    sendClassMsg cls' (mkSelector "effect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for a wiggle effect that rotates back and forth, starting by rotating clockwise.
--
-- ObjC selector: @+ wiggleClockwiseEffect@
wiggleClockwiseEffect :: IO (Id NSSymbolWiggleEffect)
wiggleClockwiseEffect  =
  do
    cls' <- getRequiredClass "NSSymbolWiggleEffect"
    sendClassMsg cls' (mkSelector "wiggleClockwiseEffect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for a wiggle effect that rotates back and forth, starting by rotating counter-clockwise.
--
-- ObjC selector: @+ wiggleCounterClockwiseEffect@
wiggleCounterClockwiseEffect :: IO (Id NSSymbolWiggleEffect)
wiggleCounterClockwiseEffect  =
  do
    cls' <- getRequiredClass "NSSymbolWiggleEffect"
    sendClassMsg cls' (mkSelector "wiggleCounterClockwiseEffect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for a wiggle effect that moves back and forth horizontally, starting by moving left.
--
-- ObjC selector: @+ wiggleLeftEffect@
wiggleLeftEffect :: IO (Id NSSymbolWiggleEffect)
wiggleLeftEffect  =
  do
    cls' <- getRequiredClass "NSSymbolWiggleEffect"
    sendClassMsg cls' (mkSelector "wiggleLeftEffect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for a wiggle effect that moves back and forth horizontally, starting by moving right.
--
-- ObjC selector: @+ wiggleRightEffect@
wiggleRightEffect :: IO (Id NSSymbolWiggleEffect)
wiggleRightEffect  =
  do
    cls' <- getRequiredClass "NSSymbolWiggleEffect"
    sendClassMsg cls' (mkSelector "wiggleRightEffect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for a wiggle effect that moves back and forth vertically, starting by moving up.
--
-- ObjC selector: @+ wiggleUpEffect@
wiggleUpEffect :: IO (Id NSSymbolWiggleEffect)
wiggleUpEffect  =
  do
    cls' <- getRequiredClass "NSSymbolWiggleEffect"
    sendClassMsg cls' (mkSelector "wiggleUpEffect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for a wiggle effect that moves back and forth vertically, starting by moving down.
--
-- ObjC selector: @+ wiggleDownEffect@
wiggleDownEffect :: IO (Id NSSymbolWiggleEffect)
wiggleDownEffect  =
  do
    cls' <- getRequiredClass "NSSymbolWiggleEffect"
    sendClassMsg cls' (mkSelector "wiggleDownEffect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for a wiggle effect that moves back and forth horizontally based on the current locale, starting by moving forward.
--
-- ObjC selector: @+ wiggleForwardEffect@
wiggleForwardEffect :: IO (Id NSSymbolWiggleEffect)
wiggleForwardEffect  =
  do
    cls' <- getRequiredClass "NSSymbolWiggleEffect"
    sendClassMsg cls' (mkSelector "wiggleForwardEffect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for a wiggle effect that moves back and forth horizontally based on the current locale, starting by moving backward.
--
-- ObjC selector: @+ wiggleBackwardEffect@
wiggleBackwardEffect :: IO (Id NSSymbolWiggleEffect)
wiggleBackwardEffect  =
  do
    cls' <- getRequiredClass "NSSymbolWiggleEffect"
    sendClassMsg cls' (mkSelector "wiggleBackwardEffect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for a wiggle effect that moves back and forth along an axis, starting by moving toward a custom angle.
--
-- The angle is in degrees moving clockwise from the positive x-axis.
--
-- ObjC selector: @+ wiggleCustomAngleEffect:@
wiggleCustomAngleEffect :: CDouble -> IO (Id NSSymbolWiggleEffect)
wiggleCustomAngleEffect angle =
  do
    cls' <- getRequiredClass "NSSymbolWiggleEffect"
    sendClassMsg cls' (mkSelector "wiggleCustomAngleEffect:") (retPtr retVoid) [argCDouble (fromIntegral angle)] >>= retainedObject . castPtr

-- | Returns a copy of the effect that animates incrementally, by layer.
--
-- ObjC selector: @- effectWithByLayer@
effectWithByLayer :: IsNSSymbolWiggleEffect nsSymbolWiggleEffect => nsSymbolWiggleEffect -> IO (Id NSSymbolWiggleEffect)
effectWithByLayer nsSymbolWiggleEffect  =
  sendMsg nsSymbolWiggleEffect (mkSelector "effectWithByLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that animates all layers of the symbol simultaneously.
--
-- ObjC selector: @- effectWithWholeSymbol@
effectWithWholeSymbol :: IsNSSymbolWiggleEffect nsSymbolWiggleEffect => nsSymbolWiggleEffect -> IO (Id NSSymbolWiggleEffect)
effectWithWholeSymbol nsSymbolWiggleEffect  =
  sendMsg nsSymbolWiggleEffect (mkSelector "effectWithWholeSymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effect@
effectSelector :: Selector
effectSelector = mkSelector "effect"

-- | @Selector@ for @wiggleClockwiseEffect@
wiggleClockwiseEffectSelector :: Selector
wiggleClockwiseEffectSelector = mkSelector "wiggleClockwiseEffect"

-- | @Selector@ for @wiggleCounterClockwiseEffect@
wiggleCounterClockwiseEffectSelector :: Selector
wiggleCounterClockwiseEffectSelector = mkSelector "wiggleCounterClockwiseEffect"

-- | @Selector@ for @wiggleLeftEffect@
wiggleLeftEffectSelector :: Selector
wiggleLeftEffectSelector = mkSelector "wiggleLeftEffect"

-- | @Selector@ for @wiggleRightEffect@
wiggleRightEffectSelector :: Selector
wiggleRightEffectSelector = mkSelector "wiggleRightEffect"

-- | @Selector@ for @wiggleUpEffect@
wiggleUpEffectSelector :: Selector
wiggleUpEffectSelector = mkSelector "wiggleUpEffect"

-- | @Selector@ for @wiggleDownEffect@
wiggleDownEffectSelector :: Selector
wiggleDownEffectSelector = mkSelector "wiggleDownEffect"

-- | @Selector@ for @wiggleForwardEffect@
wiggleForwardEffectSelector :: Selector
wiggleForwardEffectSelector = mkSelector "wiggleForwardEffect"

-- | @Selector@ for @wiggleBackwardEffect@
wiggleBackwardEffectSelector :: Selector
wiggleBackwardEffectSelector = mkSelector "wiggleBackwardEffect"

-- | @Selector@ for @wiggleCustomAngleEffect:@
wiggleCustomAngleEffectSelector :: Selector
wiggleCustomAngleEffectSelector = mkSelector "wiggleCustomAngleEffect:"

-- | @Selector@ for @effectWithByLayer@
effectWithByLayerSelector :: Selector
effectWithByLayerSelector = mkSelector "effectWithByLayer"

-- | @Selector@ for @effectWithWholeSymbol@
effectWithWholeSymbolSelector :: Selector
effectWithWholeSymbolSelector = mkSelector "effectWithWholeSymbol"

