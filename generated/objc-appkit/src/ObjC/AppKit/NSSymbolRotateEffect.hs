{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that applies the Rotate animation to symbol images.
--
-- The Rotate animation rotates parts of a symbol around a symbol-provided anchor point.
--
-- Generated bindings for @NSSymbolRotateEffect@.
module ObjC.AppKit.NSSymbolRotateEffect
  ( NSSymbolRotateEffect
  , IsNSSymbolRotateEffect(..)
  , effect
  , rotateClockwiseEffect
  , rotateCounterClockwiseEffect
  , effectWithByLayer
  , effectWithWholeSymbol
  , effectSelector
  , rotateClockwiseEffectSelector
  , rotateCounterClockwiseEffectSelector
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

-- | The default rotate effect, determined by the system.
--
-- ObjC selector: @+ effect@
effect :: IO (Id NSSymbolRotateEffect)
effect  =
  do
    cls' <- getRequiredClass "NSSymbolRotateEffect"
    sendClassMsg cls' (mkSelector "effect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for a rotate effect that rotates clockwise.
--
-- ObjC selector: @+ rotateClockwiseEffect@
rotateClockwiseEffect :: IO (Id NSSymbolRotateEffect)
rotateClockwiseEffect  =
  do
    cls' <- getRequiredClass "NSSymbolRotateEffect"
    sendClassMsg cls' (mkSelector "rotateClockwiseEffect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for a rotate effect that rotates counter-clockwise.
--
-- ObjC selector: @+ rotateCounterClockwiseEffect@
rotateCounterClockwiseEffect :: IO (Id NSSymbolRotateEffect)
rotateCounterClockwiseEffect  =
  do
    cls' <- getRequiredClass "NSSymbolRotateEffect"
    sendClassMsg cls' (mkSelector "rotateCounterClockwiseEffect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that animates incrementally, by layer.
--
-- ObjC selector: @- effectWithByLayer@
effectWithByLayer :: IsNSSymbolRotateEffect nsSymbolRotateEffect => nsSymbolRotateEffect -> IO (Id NSSymbolRotateEffect)
effectWithByLayer nsSymbolRotateEffect  =
  sendMsg nsSymbolRotateEffect (mkSelector "effectWithByLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that animates all layers of the symbol simultaneously.
--
-- ObjC selector: @- effectWithWholeSymbol@
effectWithWholeSymbol :: IsNSSymbolRotateEffect nsSymbolRotateEffect => nsSymbolRotateEffect -> IO (Id NSSymbolRotateEffect)
effectWithWholeSymbol nsSymbolRotateEffect  =
  sendMsg nsSymbolRotateEffect (mkSelector "effectWithWholeSymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effect@
effectSelector :: Selector
effectSelector = mkSelector "effect"

-- | @Selector@ for @rotateClockwiseEffect@
rotateClockwiseEffectSelector :: Selector
rotateClockwiseEffectSelector = mkSelector "rotateClockwiseEffect"

-- | @Selector@ for @rotateCounterClockwiseEffect@
rotateCounterClockwiseEffectSelector :: Selector
rotateCounterClockwiseEffectSelector = mkSelector "rotateCounterClockwiseEffect"

-- | @Selector@ for @effectWithByLayer@
effectWithByLayerSelector :: Selector
effectWithByLayerSelector = mkSelector "effectWithByLayer"

-- | @Selector@ for @effectWithWholeSymbol@
effectWithWholeSymbolSelector :: Selector
effectWithWholeSymbolSelector = mkSelector "effectWithWholeSymbol"

