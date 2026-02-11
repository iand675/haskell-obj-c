{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that applies the Bounce animation to symbol images.
--
-- The Bounce animation applies a transitory scaling effect to the symbol.
--
-- Generated bindings for @NSSymbolBounceEffect@.
module ObjC.AppKit.NSSymbolBounceEffect
  ( NSSymbolBounceEffect
  , IsNSSymbolBounceEffect(..)
  , effect
  , bounceUpEffect
  , bounceDownEffect
  , effectWithByLayer
  , effectWithWholeSymbol
  , effectSelector
  , bounceUpEffectSelector
  , bounceDownEffectSelector
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

-- | The default bounce effect, determined by the system.
--
-- ObjC selector: @+ effect@
effect :: IO (Id NSSymbolBounceEffect)
effect  =
  do
    cls' <- getRequiredClass "NSSymbolBounceEffect"
    sendClassMsg cls' (mkSelector "effect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for a bounce effect that bounces up.
--
-- ObjC selector: @+ bounceUpEffect@
bounceUpEffect :: IO (Id NSSymbolBounceEffect)
bounceUpEffect  =
  do
    cls' <- getRequiredClass "NSSymbolBounceEffect"
    sendClassMsg cls' (mkSelector "bounceUpEffect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for a bounce effect that bounces down.
--
-- ObjC selector: @+ bounceDownEffect@
bounceDownEffect :: IO (Id NSSymbolBounceEffect)
bounceDownEffect  =
  do
    cls' <- getRequiredClass "NSSymbolBounceEffect"
    sendClassMsg cls' (mkSelector "bounceDownEffect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that animates incrementally, by layer.
--
-- ObjC selector: @- effectWithByLayer@
effectWithByLayer :: IsNSSymbolBounceEffect nsSymbolBounceEffect => nsSymbolBounceEffect -> IO (Id NSSymbolBounceEffect)
effectWithByLayer nsSymbolBounceEffect  =
  sendMsg nsSymbolBounceEffect (mkSelector "effectWithByLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that animates all layers of the symbol simultaneously.
--
-- ObjC selector: @- effectWithWholeSymbol@
effectWithWholeSymbol :: IsNSSymbolBounceEffect nsSymbolBounceEffect => nsSymbolBounceEffect -> IO (Id NSSymbolBounceEffect)
effectWithWholeSymbol nsSymbolBounceEffect  =
  sendMsg nsSymbolBounceEffect (mkSelector "effectWithWholeSymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effect@
effectSelector :: Selector
effectSelector = mkSelector "effect"

-- | @Selector@ for @bounceUpEffect@
bounceUpEffectSelector :: Selector
bounceUpEffectSelector = mkSelector "bounceUpEffect"

-- | @Selector@ for @bounceDownEffect@
bounceDownEffectSelector :: Selector
bounceDownEffectSelector = mkSelector "bounceDownEffect"

-- | @Selector@ for @effectWithByLayer@
effectWithByLayerSelector :: Selector
effectWithByLayerSelector = mkSelector "effectWithByLayer"

-- | @Selector@ for @effectWithWholeSymbol@
effectWithWholeSymbolSelector :: Selector
effectWithWholeSymbolSelector = mkSelector "effectWithWholeSymbol"

