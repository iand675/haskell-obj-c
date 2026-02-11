{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that scales symbol images.
--
-- Generated bindings for @NSSymbolScaleEffect@.
module ObjC.AppKit.NSSymbolScaleEffect
  ( NSSymbolScaleEffect
  , IsNSSymbolScaleEffect(..)
  , effect
  , scaleUpEffect
  , scaleDownEffect
  , effectWithByLayer
  , effectWithWholeSymbol
  , effectSelector
  , scaleUpEffectSelector
  , scaleDownEffectSelector
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

-- | The default scaling effect, determined by the system.
--
-- ObjC selector: @+ effect@
effect :: IO (Id NSSymbolScaleEffect)
effect  =
  do
    cls' <- getRequiredClass "NSSymbolScaleEffect"
    sendClassMsg cls' (mkSelector "effect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer to create a scale effect with a scale up level.
--
-- ObjC selector: @+ scaleUpEffect@
scaleUpEffect :: IO (Id NSSymbolScaleEffect)
scaleUpEffect  =
  do
    cls' <- getRequiredClass "NSSymbolScaleEffect"
    sendClassMsg cls' (mkSelector "scaleUpEffect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer to create a scale effect with a scale down level.
--
-- ObjC selector: @+ scaleDownEffect@
scaleDownEffect :: IO (Id NSSymbolScaleEffect)
scaleDownEffect  =
  do
    cls' <- getRequiredClass "NSSymbolScaleEffect"
    sendClassMsg cls' (mkSelector "scaleDownEffect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that animates incrementally, by layer.
--
-- ObjC selector: @- effectWithByLayer@
effectWithByLayer :: IsNSSymbolScaleEffect nsSymbolScaleEffect => nsSymbolScaleEffect -> IO (Id NSSymbolScaleEffect)
effectWithByLayer nsSymbolScaleEffect  =
  sendMsg nsSymbolScaleEffect (mkSelector "effectWithByLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that animates all layers of the symbol simultaneously.
--
-- ObjC selector: @- effectWithWholeSymbol@
effectWithWholeSymbol :: IsNSSymbolScaleEffect nsSymbolScaleEffect => nsSymbolScaleEffect -> IO (Id NSSymbolScaleEffect)
effectWithWholeSymbol nsSymbolScaleEffect  =
  sendMsg nsSymbolScaleEffect (mkSelector "effectWithWholeSymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effect@
effectSelector :: Selector
effectSelector = mkSelector "effect"

-- | @Selector@ for @scaleUpEffect@
scaleUpEffectSelector :: Selector
scaleUpEffectSelector = mkSelector "scaleUpEffect"

-- | @Selector@ for @scaleDownEffect@
scaleDownEffectSelector :: Selector
scaleDownEffectSelector = mkSelector "scaleDownEffect"

-- | @Selector@ for @effectWithByLayer@
effectWithByLayerSelector :: Selector
effectWithByLayerSelector = mkSelector "effectWithByLayer"

-- | @Selector@ for @effectWithWholeSymbol@
effectWithWholeSymbolSelector :: Selector
effectWithWholeSymbolSelector = mkSelector "effectWithWholeSymbol"

