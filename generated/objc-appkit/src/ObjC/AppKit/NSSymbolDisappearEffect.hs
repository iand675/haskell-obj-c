{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that applies the Disappear animation to symbol images.
--
-- The Disappear animation makes the symbol visible either as a whole, or one motion group at a time.
--
-- Generated bindings for @NSSymbolDisappearEffect@.
module ObjC.AppKit.NSSymbolDisappearEffect
  ( NSSymbolDisappearEffect
  , IsNSSymbolDisappearEffect(..)
  , effect
  , disappearUpEffect
  , disappearDownEffect
  , effectWithByLayer
  , effectWithWholeSymbol
  , effectSelector
  , disappearUpEffectSelector
  , disappearDownEffectSelector
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

-- | The default disappear effect, determined by the system.
--
-- ObjC selector: @+ effect@
effect :: IO (Id NSSymbolDisappearEffect)
effect  =
  do
    cls' <- getRequiredClass "NSSymbolDisappearEffect"
    sendClassMsg cls' (mkSelector "effect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for a disappear effect that disappears scaling up.
--
-- ObjC selector: @+ disappearUpEffect@
disappearUpEffect :: IO (Id NSSymbolDisappearEffect)
disappearUpEffect  =
  do
    cls' <- getRequiredClass "NSSymbolDisappearEffect"
    sendClassMsg cls' (mkSelector "disappearUpEffect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for a disappear effect that disappears scaling down.
--
-- ObjC selector: @+ disappearDownEffect@
disappearDownEffect :: IO (Id NSSymbolDisappearEffect)
disappearDownEffect  =
  do
    cls' <- getRequiredClass "NSSymbolDisappearEffect"
    sendClassMsg cls' (mkSelector "disappearDownEffect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that animates incrementally, by layer.
--
-- ObjC selector: @- effectWithByLayer@
effectWithByLayer :: IsNSSymbolDisappearEffect nsSymbolDisappearEffect => nsSymbolDisappearEffect -> IO (Id NSSymbolDisappearEffect)
effectWithByLayer nsSymbolDisappearEffect  =
  sendMsg nsSymbolDisappearEffect (mkSelector "effectWithByLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that animates all layers of the symbol simultaneously.
--
-- ObjC selector: @- effectWithWholeSymbol@
effectWithWholeSymbol :: IsNSSymbolDisappearEffect nsSymbolDisappearEffect => nsSymbolDisappearEffect -> IO (Id NSSymbolDisappearEffect)
effectWithWholeSymbol nsSymbolDisappearEffect  =
  sendMsg nsSymbolDisappearEffect (mkSelector "effectWithWholeSymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effect@
effectSelector :: Selector
effectSelector = mkSelector "effect"

-- | @Selector@ for @disappearUpEffect@
disappearUpEffectSelector :: Selector
disappearUpEffectSelector = mkSelector "disappearUpEffect"

-- | @Selector@ for @disappearDownEffect@
disappearDownEffectSelector :: Selector
disappearDownEffectSelector = mkSelector "disappearDownEffect"

-- | @Selector@ for @effectWithByLayer@
effectWithByLayerSelector :: Selector
effectWithByLayerSelector = mkSelector "effectWithByLayer"

-- | @Selector@ for @effectWithWholeSymbol@
effectWithWholeSymbolSelector :: Selector
effectWithWholeSymbolSelector = mkSelector "effectWithWholeSymbol"

