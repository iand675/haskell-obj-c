{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that applies the Pulse animation to symbol images.
--
-- The Pulse animation fades the opacity of either all layers in the symbol, or of a subset of the layers in the symbol.
--
-- Generated bindings for @NSSymbolPulseEffect@.
module ObjC.AppKit.NSSymbolPulseEffect
  ( NSSymbolPulseEffect
  , IsNSSymbolPulseEffect(..)
  , effect
  , effectWithByLayer
  , effectWithWholeSymbol
  , effectSelector
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

-- | The default pulse effect, determined by the system.
--
-- ObjC selector: @+ effect@
effect :: IO (Id NSSymbolPulseEffect)
effect  =
  do
    cls' <- getRequiredClass "NSSymbolPulseEffect"
    sendClassMsg cls' (mkSelector "effect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that only animates annotated pulse layers.
--
-- ObjC selector: @- effectWithByLayer@
effectWithByLayer :: IsNSSymbolPulseEffect nsSymbolPulseEffect => nsSymbolPulseEffect -> IO (Id NSSymbolPulseEffect)
effectWithByLayer nsSymbolPulseEffect  =
  sendMsg nsSymbolPulseEffect (mkSelector "effectWithByLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that animates all layers of the symbol simultaneously.
--
-- ObjC selector: @- effectWithWholeSymbol@
effectWithWholeSymbol :: IsNSSymbolPulseEffect nsSymbolPulseEffect => nsSymbolPulseEffect -> IO (Id NSSymbolPulseEffect)
effectWithWholeSymbol nsSymbolPulseEffect  =
  sendMsg nsSymbolPulseEffect (mkSelector "effectWithWholeSymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effect@
effectSelector :: Selector
effectSelector = mkSelector "effect"

-- | @Selector@ for @effectWithByLayer@
effectWithByLayerSelector :: Selector
effectWithByLayerSelector = mkSelector "effectWithByLayer"

-- | @Selector@ for @effectWithWholeSymbol@
effectWithWholeSymbolSelector :: Selector
effectWithWholeSymbolSelector = mkSelector "effectWithWholeSymbol"

