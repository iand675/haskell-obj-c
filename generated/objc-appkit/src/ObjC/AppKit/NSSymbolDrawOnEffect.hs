{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that applies the DrawOn animation to symbol images.
--
-- The DrawOn animation makes the symbol visible either as a whole, or one motion group at a time, animating parts of the symbol with draw data.
--
-- Generated bindings for @NSSymbolDrawOnEffect@.
module ObjC.AppKit.NSSymbolDrawOnEffect
  ( NSSymbolDrawOnEffect
  , IsNSSymbolDrawOnEffect(..)
  , effect
  , effectWithByLayer
  , effectWithWholeSymbol
  , effectWithIndividually
  , effectSelector
  , effectWithByLayerSelector
  , effectWithWholeSymbolSelector
  , effectWithIndividuallySelector


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

-- | The default draw on effect, determined by the system.
--
-- ObjC selector: @+ effect@
effect :: IO (Id NSSymbolDrawOnEffect)
effect  =
  do
    cls' <- getRequiredClass "NSSymbolDrawOnEffect"
    sendClassMsg cls' (mkSelector "effect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect requesting an animation that applies separately to each motion group.
--
-- ObjC selector: @- effectWithByLayer@
effectWithByLayer :: IsNSSymbolDrawOnEffect nsSymbolDrawOnEffect => nsSymbolDrawOnEffect -> IO (Id NSSymbolDrawOnEffect)
effectWithByLayer nsSymbolDrawOnEffect  =
  sendMsg nsSymbolDrawOnEffect (mkSelector "effectWithByLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect requesting an animation that applies to all motion groups simultaneously.
--
-- ObjC selector: @- effectWithWholeSymbol@
effectWithWholeSymbol :: IsNSSymbolDrawOnEffect nsSymbolDrawOnEffect => nsSymbolDrawOnEffect -> IO (Id NSSymbolDrawOnEffect)
effectWithWholeSymbol nsSymbolDrawOnEffect  =
  sendMsg nsSymbolDrawOnEffect (mkSelector "effectWithWholeSymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect requesting an animation that applies separately to each motion group, where only one motion group is active at a time.
--
-- ObjC selector: @- effectWithIndividually@
effectWithIndividually :: IsNSSymbolDrawOnEffect nsSymbolDrawOnEffect => nsSymbolDrawOnEffect -> IO (Id NSSymbolDrawOnEffect)
effectWithIndividually nsSymbolDrawOnEffect  =
  sendMsg nsSymbolDrawOnEffect (mkSelector "effectWithIndividually") (retPtr retVoid) [] >>= retainedObject . castPtr

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

-- | @Selector@ for @effectWithIndividually@
effectWithIndividuallySelector :: Selector
effectWithIndividuallySelector = mkSelector "effectWithIndividually"

