{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that applies the DrawOn animation to symbol images.
--
-- The DrawOn animation makes the symbol visible either as a whole, or one motion group at a time, animating parts of the symbol with draw data.
--
-- Generated bindings for @NSSymbolDrawOnEffect@.
module ObjC.Symbols.NSSymbolDrawOnEffect
  ( NSSymbolDrawOnEffect
  , IsNSSymbolDrawOnEffect(..)
  , effect
  , effectWithByLayer
  , effectWithWholeSymbol
  , effectWithIndividually
  , effectSelector
  , effectWithByLayerSelector
  , effectWithIndividuallySelector
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

-- | The default draw on effect, determined by the system.
--
-- ObjC selector: @+ effect@
effect :: IO (Id NSSymbolDrawOnEffect)
effect  =
  do
    cls' <- getRequiredClass "NSSymbolDrawOnEffect"
    sendClassMessage cls' effectSelector

-- | Returns a copy of the effect requesting an animation that applies separately to each motion group.
--
-- ObjC selector: @- effectWithByLayer@
effectWithByLayer :: IsNSSymbolDrawOnEffect nsSymbolDrawOnEffect => nsSymbolDrawOnEffect -> IO (Id NSSymbolDrawOnEffect)
effectWithByLayer nsSymbolDrawOnEffect =
  sendMessage nsSymbolDrawOnEffect effectWithByLayerSelector

-- | Returns a copy of the effect requesting an animation that applies to all motion groups simultaneously.
--
-- ObjC selector: @- effectWithWholeSymbol@
effectWithWholeSymbol :: IsNSSymbolDrawOnEffect nsSymbolDrawOnEffect => nsSymbolDrawOnEffect -> IO (Id NSSymbolDrawOnEffect)
effectWithWholeSymbol nsSymbolDrawOnEffect =
  sendMessage nsSymbolDrawOnEffect effectWithWholeSymbolSelector

-- | Returns a copy of the effect requesting an animation that applies separately to each motion group, where only one motion group is active at a time.
--
-- ObjC selector: @- effectWithIndividually@
effectWithIndividually :: IsNSSymbolDrawOnEffect nsSymbolDrawOnEffect => nsSymbolDrawOnEffect -> IO (Id NSSymbolDrawOnEffect)
effectWithIndividually nsSymbolDrawOnEffect =
  sendMessage nsSymbolDrawOnEffect effectWithIndividuallySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effect@
effectSelector :: Selector '[] (Id NSSymbolDrawOnEffect)
effectSelector = mkSelector "effect"

-- | @Selector@ for @effectWithByLayer@
effectWithByLayerSelector :: Selector '[] (Id NSSymbolDrawOnEffect)
effectWithByLayerSelector = mkSelector "effectWithByLayer"

-- | @Selector@ for @effectWithWholeSymbol@
effectWithWholeSymbolSelector :: Selector '[] (Id NSSymbolDrawOnEffect)
effectWithWholeSymbolSelector = mkSelector "effectWithWholeSymbol"

-- | @Selector@ for @effectWithIndividually@
effectWithIndividuallySelector :: Selector '[] (Id NSSymbolDrawOnEffect)
effectWithIndividuallySelector = mkSelector "effectWithIndividually"

