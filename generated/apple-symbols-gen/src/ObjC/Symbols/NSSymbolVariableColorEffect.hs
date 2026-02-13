{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that applies the Variable Color animation to symbol images.
--
-- The Variable Color animation replaces the opacity of variable layers in the symbol by a possibly repeating pattern that moves up and possibly back down the variable layers. It has no effect for non-variable color symbol images.
--
-- Generated bindings for @NSSymbolVariableColorEffect@.
module ObjC.Symbols.NSSymbolVariableColorEffect
  ( NSSymbolVariableColorEffect
  , IsNSSymbolVariableColorEffect(..)
  , effect
  , effectWithIterative
  , effectWithCumulative
  , effectWithReversing
  , effectWithNonReversing
  , effectWithHideInactiveLayers
  , effectWithDimInactiveLayers
  , effectSelector
  , effectWithCumulativeSelector
  , effectWithDimInactiveLayersSelector
  , effectWithHideInactiveLayersSelector
  , effectWithIterativeSelector
  , effectWithNonReversingSelector
  , effectWithReversingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Symbols.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The default variable color effect, determined by the system.
--
-- ObjC selector: @+ effect@
effect :: IO (Id NSSymbolVariableColorEffect)
effect  =
  do
    cls' <- getRequiredClass "NSSymbolVariableColorEffect"
    sendClassMessage cls' effectSelector

-- | Returns a copy of the effect that activates one layer at a time. This cancels the cumulative variant.
--
-- ObjC selector: @- effectWithIterative@
effectWithIterative :: IsNSSymbolVariableColorEffect nsSymbolVariableColorEffect => nsSymbolVariableColorEffect -> IO (Id NSSymbolVariableColorEffect)
effectWithIterative nsSymbolVariableColorEffect =
  sendMessage nsSymbolVariableColorEffect effectWithIterativeSelector

-- | Returns a copy of the effect that activates each layer until all layers are active. This cancels the iterative variant.
--
-- ObjC selector: @- effectWithCumulative@
effectWithCumulative :: IsNSSymbolVariableColorEffect nsSymbolVariableColorEffect => nsSymbolVariableColorEffect -> IO (Id NSSymbolVariableColorEffect)
effectWithCumulative nsSymbolVariableColorEffect =
  sendMessage nsSymbolVariableColorEffect effectWithCumulativeSelector

-- | Returns a copy of the effect that animates in reverse after fully executing. This cancels the nonReversing variant.
--
-- ObjC selector: @- effectWithReversing@
effectWithReversing :: IsNSSymbolVariableColorEffect nsSymbolVariableColorEffect => nsSymbolVariableColorEffect -> IO (Id NSSymbolVariableColorEffect)
effectWithReversing nsSymbolVariableColorEffect =
  sendMessage nsSymbolVariableColorEffect effectWithReversingSelector

-- | Returns a copy of the effect that only animates forwards before restarting. This cancels the reversing variant.
--
-- ObjC selector: @- effectWithNonReversing@
effectWithNonReversing :: IsNSSymbolVariableColorEffect nsSymbolVariableColorEffect => nsSymbolVariableColorEffect -> IO (Id NSSymbolVariableColorEffect)
effectWithNonReversing nsSymbolVariableColorEffect =
  sendMessage nsSymbolVariableColorEffect effectWithNonReversingSelector

-- | Returns a copy of the effect that hides layers when they are inactive.
--
-- ObjC selector: @- effectWithHideInactiveLayers@
effectWithHideInactiveLayers :: IsNSSymbolVariableColorEffect nsSymbolVariableColorEffect => nsSymbolVariableColorEffect -> IO (Id NSSymbolVariableColorEffect)
effectWithHideInactiveLayers nsSymbolVariableColorEffect =
  sendMessage nsSymbolVariableColorEffect effectWithHideInactiveLayersSelector

-- | Returns a copy of the effect that draws layers with reduced (but non-zero) opacity when they are inactive.
--
-- ObjC selector: @- effectWithDimInactiveLayers@
effectWithDimInactiveLayers :: IsNSSymbolVariableColorEffect nsSymbolVariableColorEffect => nsSymbolVariableColorEffect -> IO (Id NSSymbolVariableColorEffect)
effectWithDimInactiveLayers nsSymbolVariableColorEffect =
  sendMessage nsSymbolVariableColorEffect effectWithDimInactiveLayersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effect@
effectSelector :: Selector '[] (Id NSSymbolVariableColorEffect)
effectSelector = mkSelector "effect"

-- | @Selector@ for @effectWithIterative@
effectWithIterativeSelector :: Selector '[] (Id NSSymbolVariableColorEffect)
effectWithIterativeSelector = mkSelector "effectWithIterative"

-- | @Selector@ for @effectWithCumulative@
effectWithCumulativeSelector :: Selector '[] (Id NSSymbolVariableColorEffect)
effectWithCumulativeSelector = mkSelector "effectWithCumulative"

-- | @Selector@ for @effectWithReversing@
effectWithReversingSelector :: Selector '[] (Id NSSymbolVariableColorEffect)
effectWithReversingSelector = mkSelector "effectWithReversing"

-- | @Selector@ for @effectWithNonReversing@
effectWithNonReversingSelector :: Selector '[] (Id NSSymbolVariableColorEffect)
effectWithNonReversingSelector = mkSelector "effectWithNonReversing"

-- | @Selector@ for @effectWithHideInactiveLayers@
effectWithHideInactiveLayersSelector :: Selector '[] (Id NSSymbolVariableColorEffect)
effectWithHideInactiveLayersSelector = mkSelector "effectWithHideInactiveLayers"

-- | @Selector@ for @effectWithDimInactiveLayers@
effectWithDimInactiveLayersSelector :: Selector '[] (Id NSSymbolVariableColorEffect)
effectWithDimInactiveLayersSelector = mkSelector "effectWithDimInactiveLayers"

