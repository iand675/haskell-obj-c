{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that applies the Appear animation to symbol images.
--
-- The Appear animation makes the symbol visible either as a whole, or one motion group at a time.
--
-- Generated bindings for @NSSymbolAppearEffect@.
module ObjC.AppKit.NSSymbolAppearEffect
  ( NSSymbolAppearEffect
  , IsNSSymbolAppearEffect(..)
  , effect
  , appearUpEffect
  , appearDownEffect
  , effectWithByLayer
  , effectWithWholeSymbol
  , effectSelector
  , appearUpEffectSelector
  , appearDownEffectSelector
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

-- | The default appear effect, determined by the system.
--
-- ObjC selector: @+ effect@
effect :: IO (Id NSSymbolAppearEffect)
effect  =
  do
    cls' <- getRequiredClass "NSSymbolAppearEffect"
    sendClassMsg cls' (mkSelector "effect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for an appear effect that appears scaling up.
--
-- ObjC selector: @+ appearUpEffect@
appearUpEffect :: IO (Id NSSymbolAppearEffect)
appearUpEffect  =
  do
    cls' <- getRequiredClass "NSSymbolAppearEffect"
    sendClassMsg cls' (mkSelector "appearUpEffect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convenience initializer for an appear effect that appears scaling down.
--
-- ObjC selector: @+ appearDownEffect@
appearDownEffect :: IO (Id NSSymbolAppearEffect)
appearDownEffect  =
  do
    cls' <- getRequiredClass "NSSymbolAppearEffect"
    sendClassMsg cls' (mkSelector "appearDownEffect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that animates incrementally, by layer.
--
-- ObjC selector: @- effectWithByLayer@
effectWithByLayer :: IsNSSymbolAppearEffect nsSymbolAppearEffect => nsSymbolAppearEffect -> IO (Id NSSymbolAppearEffect)
effectWithByLayer nsSymbolAppearEffect  =
  sendMsg nsSymbolAppearEffect (mkSelector "effectWithByLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that animates all layers of the symbol simultaneously.
--
-- ObjC selector: @- effectWithWholeSymbol@
effectWithWholeSymbol :: IsNSSymbolAppearEffect nsSymbolAppearEffect => nsSymbolAppearEffect -> IO (Id NSSymbolAppearEffect)
effectWithWholeSymbol nsSymbolAppearEffect  =
  sendMsg nsSymbolAppearEffect (mkSelector "effectWithWholeSymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effect@
effectSelector :: Selector
effectSelector = mkSelector "effect"

-- | @Selector@ for @appearUpEffect@
appearUpEffectSelector :: Selector
appearUpEffectSelector = mkSelector "appearUpEffect"

-- | @Selector@ for @appearDownEffect@
appearDownEffectSelector :: Selector
appearDownEffectSelector = mkSelector "appearDownEffect"

-- | @Selector@ for @effectWithByLayer@
effectWithByLayerSelector :: Selector
effectWithByLayerSelector = mkSelector "effectWithByLayer"

-- | @Selector@ for @effectWithWholeSymbol@
effectWithWholeSymbolSelector :: Selector
effectWithWholeSymbolSelector = mkSelector "effectWithWholeSymbol"

