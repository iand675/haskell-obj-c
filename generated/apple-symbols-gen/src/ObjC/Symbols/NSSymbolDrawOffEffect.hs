{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that applies the DrawOff animation to symbol images.
--
-- The DrawOff animation makes the symbol hidden either as a whole, or one motion group at a time, animating parts of the symbol with draw data.
--
-- Generated bindings for @NSSymbolDrawOffEffect@.
module ObjC.Symbols.NSSymbolDrawOffEffect
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
  , effectWithIndividuallySelector
  , effectWithNonReversedSelector
  , effectWithReversedSelector
  , effectWithWholeSymbolSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Symbols.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The default draw off effect, determined by the system.
--
-- ObjC selector: @+ effect@
effect :: IO (Id NSSymbolDrawOffEffect)
effect  =
  do
    cls' <- getRequiredClass "NSSymbolDrawOffEffect"
    sendClassMessage cls' effectSelector

-- | Returns a copy of the effect requesting an animation that applies separately to each motion group.
--
-- ObjC selector: @- effectWithByLayer@
effectWithByLayer :: IsNSSymbolDrawOffEffect nsSymbolDrawOffEffect => nsSymbolDrawOffEffect -> IO (Id NSSymbolDrawOffEffect)
effectWithByLayer nsSymbolDrawOffEffect =
  sendMessage nsSymbolDrawOffEffect effectWithByLayerSelector

-- | Returns a copy of the effect requesting an animation that applies to all motion groups simultaneously.
--
-- ObjC selector: @- effectWithWholeSymbol@
effectWithWholeSymbol :: IsNSSymbolDrawOffEffect nsSymbolDrawOffEffect => nsSymbolDrawOffEffect -> IO (Id NSSymbolDrawOffEffect)
effectWithWholeSymbol nsSymbolDrawOffEffect =
  sendMessage nsSymbolDrawOffEffect effectWithWholeSymbolSelector

-- | Returns a copy of the effect requesting an animation that applies separately to each motion group, where only one motion group is active at a time.
--
-- ObjC selector: @- effectWithIndividually@
effectWithIndividually :: IsNSSymbolDrawOffEffect nsSymbolDrawOffEffect => nsSymbolDrawOffEffect -> IO (Id NSSymbolDrawOffEffect)
effectWithIndividually nsSymbolDrawOffEffect =
  sendMessage nsSymbolDrawOffEffect effectWithIndividuallySelector

-- | Returns a copy of the effect that animates in reverse. This cancels the nonReversed variant.
--
-- ObjC selector: @- effectWithReversed@
effectWithReversed :: IsNSSymbolDrawOffEffect nsSymbolDrawOffEffect => nsSymbolDrawOffEffect -> IO (Id NSSymbolDrawOffEffect)
effectWithReversed nsSymbolDrawOffEffect =
  sendMessage nsSymbolDrawOffEffect effectWithReversedSelector

-- | Returns a copy of the effect that only animates forwards. This cancels the reversed variant.
--
-- ObjC selector: @- effectWithNonReversed@
effectWithNonReversed :: IsNSSymbolDrawOffEffect nsSymbolDrawOffEffect => nsSymbolDrawOffEffect -> IO (Id NSSymbolDrawOffEffect)
effectWithNonReversed nsSymbolDrawOffEffect =
  sendMessage nsSymbolDrawOffEffect effectWithNonReversedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effect@
effectSelector :: Selector '[] (Id NSSymbolDrawOffEffect)
effectSelector = mkSelector "effect"

-- | @Selector@ for @effectWithByLayer@
effectWithByLayerSelector :: Selector '[] (Id NSSymbolDrawOffEffect)
effectWithByLayerSelector = mkSelector "effectWithByLayer"

-- | @Selector@ for @effectWithWholeSymbol@
effectWithWholeSymbolSelector :: Selector '[] (Id NSSymbolDrawOffEffect)
effectWithWholeSymbolSelector = mkSelector "effectWithWholeSymbol"

-- | @Selector@ for @effectWithIndividually@
effectWithIndividuallySelector :: Selector '[] (Id NSSymbolDrawOffEffect)
effectWithIndividuallySelector = mkSelector "effectWithIndividually"

-- | @Selector@ for @effectWithReversed@
effectWithReversedSelector :: Selector '[] (Id NSSymbolDrawOffEffect)
effectWithReversedSelector = mkSelector "effectWithReversed"

-- | @Selector@ for @effectWithNonReversed@
effectWithNonReversedSelector :: Selector '[] (Id NSSymbolDrawOffEffect)
effectWithNonReversedSelector = mkSelector "effectWithNonReversed"

