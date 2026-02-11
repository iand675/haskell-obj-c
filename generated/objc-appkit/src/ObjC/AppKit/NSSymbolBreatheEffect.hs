{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that applies the Breathe animation to symbol images.
--
-- The Breathe animation smoothly scales a symbol up and down.
--
-- Generated bindings for @NSSymbolBreatheEffect@.
module ObjC.AppKit.NSSymbolBreatheEffect
  ( NSSymbolBreatheEffect
  , IsNSSymbolBreatheEffect(..)
  , effect
  , breathePulseEffect
  , breathePlainEffect
  , effectWithByLayer
  , effectWithWholeSymbol
  , effectSelector
  , breathePulseEffectSelector
  , breathePlainEffectSelector
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

-- | The default breathe effect, determined by the system.
--
-- ObjC selector: @+ effect@
effect :: IO (Id NSSymbolBreatheEffect)
effect  =
  do
    cls' <- getRequiredClass "NSSymbolBreatheEffect"
    sendClassMsg cls' (mkSelector "effect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for a breathe effect that pulses layers as they breathe.
--
-- ObjC selector: @+ breathePulseEffect@
breathePulseEffect :: IO (Id NSSymbolBreatheEffect)
breathePulseEffect  =
  do
    cls' <- getRequiredClass "NSSymbolBreatheEffect"
    sendClassMsg cls' (mkSelector "breathePulseEffect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for a breathe effect that makes the symbol breathe with no other styling.
--
-- ObjC selector: @+ breathePlainEffect@
breathePlainEffect :: IO (Id NSSymbolBreatheEffect)
breathePlainEffect  =
  do
    cls' <- getRequiredClass "NSSymbolBreatheEffect"
    sendClassMsg cls' (mkSelector "breathePlainEffect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that animates incrementally, by layer.
--
-- ObjC selector: @- effectWithByLayer@
effectWithByLayer :: IsNSSymbolBreatheEffect nsSymbolBreatheEffect => nsSymbolBreatheEffect -> IO (Id NSSymbolBreatheEffect)
effectWithByLayer nsSymbolBreatheEffect  =
  sendMsg nsSymbolBreatheEffect (mkSelector "effectWithByLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that animates all layers of the symbol simultaneously.
--
-- ObjC selector: @- effectWithWholeSymbol@
effectWithWholeSymbol :: IsNSSymbolBreatheEffect nsSymbolBreatheEffect => nsSymbolBreatheEffect -> IO (Id NSSymbolBreatheEffect)
effectWithWholeSymbol nsSymbolBreatheEffect  =
  sendMsg nsSymbolBreatheEffect (mkSelector "effectWithWholeSymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effect@
effectSelector :: Selector
effectSelector = mkSelector "effect"

-- | @Selector@ for @breathePulseEffect@
breathePulseEffectSelector :: Selector
breathePulseEffectSelector = mkSelector "breathePulseEffect"

-- | @Selector@ for @breathePlainEffect@
breathePlainEffectSelector :: Selector
breathePlainEffectSelector = mkSelector "breathePlainEffect"

-- | @Selector@ for @effectWithByLayer@
effectWithByLayerSelector :: Selector
effectWithByLayerSelector = mkSelector "effectWithByLayer"

-- | @Selector@ for @effectWithWholeSymbol@
effectWithWholeSymbolSelector :: Selector
effectWithWholeSymbolSelector = mkSelector "effectWithWholeSymbol"

