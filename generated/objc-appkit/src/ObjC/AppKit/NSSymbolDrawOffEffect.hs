{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that applies the DrawOff animation to symbol images.
--
-- The DrawOff animation makes the symbol hidden either as a whole, or one motion group at a time, animating parts of the symbol with draw data.
--
-- Generated bindings for @NSSymbolDrawOffEffect@.
module ObjC.AppKit.NSSymbolDrawOffEffect
  ( NSSymbolDrawOffEffect
  , IsNSSymbolDrawOffEffect(..)
  , effect
  , effectWithByLayer
  , effectWithWholeSymbol
  , effectWithIndividually
  , effectWithReversed
  , effectWithNonReversed
  , effectSelector
  , effectWithByLayerSelector
  , effectWithWholeSymbolSelector
  , effectWithIndividuallySelector
  , effectWithReversedSelector
  , effectWithNonReversedSelector


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

-- | The default draw off effect, determined by the system.
--
-- ObjC selector: @+ effect@
effect :: IO (Id NSSymbolDrawOffEffect)
effect  =
  do
    cls' <- getRequiredClass "NSSymbolDrawOffEffect"
    sendClassMsg cls' (mkSelector "effect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect requesting an animation that applies separately to each motion group.
--
-- ObjC selector: @- effectWithByLayer@
effectWithByLayer :: IsNSSymbolDrawOffEffect nsSymbolDrawOffEffect => nsSymbolDrawOffEffect -> IO (Id NSSymbolDrawOffEffect)
effectWithByLayer nsSymbolDrawOffEffect  =
  sendMsg nsSymbolDrawOffEffect (mkSelector "effectWithByLayer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect requesting an animation that applies to all motion groups simultaneously.
--
-- ObjC selector: @- effectWithWholeSymbol@
effectWithWholeSymbol :: IsNSSymbolDrawOffEffect nsSymbolDrawOffEffect => nsSymbolDrawOffEffect -> IO (Id NSSymbolDrawOffEffect)
effectWithWholeSymbol nsSymbolDrawOffEffect  =
  sendMsg nsSymbolDrawOffEffect (mkSelector "effectWithWholeSymbol") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect requesting an animation that applies separately to each motion group, where only one motion group is active at a time.
--
-- ObjC selector: @- effectWithIndividually@
effectWithIndividually :: IsNSSymbolDrawOffEffect nsSymbolDrawOffEffect => nsSymbolDrawOffEffect -> IO (Id NSSymbolDrawOffEffect)
effectWithIndividually nsSymbolDrawOffEffect  =
  sendMsg nsSymbolDrawOffEffect (mkSelector "effectWithIndividually") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that animates in reverse. This cancels the nonReversed variant.
--
-- ObjC selector: @- effectWithReversed@
effectWithReversed :: IsNSSymbolDrawOffEffect nsSymbolDrawOffEffect => nsSymbolDrawOffEffect -> IO (Id NSSymbolDrawOffEffect)
effectWithReversed nsSymbolDrawOffEffect  =
  sendMsg nsSymbolDrawOffEffect (mkSelector "effectWithReversed") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that only animates forwards. This cancels the reversed variant.
--
-- ObjC selector: @- effectWithNonReversed@
effectWithNonReversed :: IsNSSymbolDrawOffEffect nsSymbolDrawOffEffect => nsSymbolDrawOffEffect -> IO (Id NSSymbolDrawOffEffect)
effectWithNonReversed nsSymbolDrawOffEffect  =
  sendMsg nsSymbolDrawOffEffect (mkSelector "effectWithNonReversed") (retPtr retVoid) [] >>= retainedObject . castPtr

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

-- | @Selector@ for @effectWithReversed@
effectWithReversedSelector :: Selector
effectWithReversedSelector = mkSelector "effectWithReversed"

-- | @Selector@ for @effectWithNonReversed@
effectWithNonReversedSelector :: Selector
effectWithNonReversedSelector = mkSelector "effectWithNonReversed"

