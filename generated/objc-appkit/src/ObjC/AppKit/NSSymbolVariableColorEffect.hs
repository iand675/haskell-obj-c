{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A symbol effect that applies the Variable Color animation to symbol images.
--
-- The Variable Color animation replaces the opacity of variable layers in the symbol by a possibly repeating pattern that moves up and possibly back down the variable layers. It has no effect for non-variable color symbol images.
--
-- Generated bindings for @NSSymbolVariableColorEffect@.
module ObjC.AppKit.NSSymbolVariableColorEffect
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
  , effectWithIterativeSelector
  , effectWithCumulativeSelector
  , effectWithReversingSelector
  , effectWithNonReversingSelector
  , effectWithHideInactiveLayersSelector
  , effectWithDimInactiveLayersSelector


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

-- | The default variable color effect, determined by the system.
--
-- ObjC selector: @+ effect@
effect :: IO (Id NSSymbolVariableColorEffect)
effect  =
  do
    cls' <- getRequiredClass "NSSymbolVariableColorEffect"
    sendClassMsg cls' (mkSelector "effect") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that activates one layer at a time. This cancels the cumulative variant.
--
-- ObjC selector: @- effectWithIterative@
effectWithIterative :: IsNSSymbolVariableColorEffect nsSymbolVariableColorEffect => nsSymbolVariableColorEffect -> IO (Id NSSymbolVariableColorEffect)
effectWithIterative nsSymbolVariableColorEffect  =
  sendMsg nsSymbolVariableColorEffect (mkSelector "effectWithIterative") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that activates each layer until all layers are active. This cancels the iterative variant.
--
-- ObjC selector: @- effectWithCumulative@
effectWithCumulative :: IsNSSymbolVariableColorEffect nsSymbolVariableColorEffect => nsSymbolVariableColorEffect -> IO (Id NSSymbolVariableColorEffect)
effectWithCumulative nsSymbolVariableColorEffect  =
  sendMsg nsSymbolVariableColorEffect (mkSelector "effectWithCumulative") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that animates in reverse after fully executing. This cancels the nonReversing variant.
--
-- ObjC selector: @- effectWithReversing@
effectWithReversing :: IsNSSymbolVariableColorEffect nsSymbolVariableColorEffect => nsSymbolVariableColorEffect -> IO (Id NSSymbolVariableColorEffect)
effectWithReversing nsSymbolVariableColorEffect  =
  sendMsg nsSymbolVariableColorEffect (mkSelector "effectWithReversing") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that only animates forwards before restarting. This cancels the reversing variant.
--
-- ObjC selector: @- effectWithNonReversing@
effectWithNonReversing :: IsNSSymbolVariableColorEffect nsSymbolVariableColorEffect => nsSymbolVariableColorEffect -> IO (Id NSSymbolVariableColorEffect)
effectWithNonReversing nsSymbolVariableColorEffect  =
  sendMsg nsSymbolVariableColorEffect (mkSelector "effectWithNonReversing") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that hides layers when they are inactive.
--
-- ObjC selector: @- effectWithHideInactiveLayers@
effectWithHideInactiveLayers :: IsNSSymbolVariableColorEffect nsSymbolVariableColorEffect => nsSymbolVariableColorEffect -> IO (Id NSSymbolVariableColorEffect)
effectWithHideInactiveLayers nsSymbolVariableColorEffect  =
  sendMsg nsSymbolVariableColorEffect (mkSelector "effectWithHideInactiveLayers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a copy of the effect that draws layers with reduced (but non-zero) opacity when they are inactive.
--
-- ObjC selector: @- effectWithDimInactiveLayers@
effectWithDimInactiveLayers :: IsNSSymbolVariableColorEffect nsSymbolVariableColorEffect => nsSymbolVariableColorEffect -> IO (Id NSSymbolVariableColorEffect)
effectWithDimInactiveLayers nsSymbolVariableColorEffect  =
  sendMsg nsSymbolVariableColorEffect (mkSelector "effectWithDimInactiveLayers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effect@
effectSelector :: Selector
effectSelector = mkSelector "effect"

-- | @Selector@ for @effectWithIterative@
effectWithIterativeSelector :: Selector
effectWithIterativeSelector = mkSelector "effectWithIterative"

-- | @Selector@ for @effectWithCumulative@
effectWithCumulativeSelector :: Selector
effectWithCumulativeSelector = mkSelector "effectWithCumulative"

-- | @Selector@ for @effectWithReversing@
effectWithReversingSelector :: Selector
effectWithReversingSelector = mkSelector "effectWithReversing"

-- | @Selector@ for @effectWithNonReversing@
effectWithNonReversingSelector :: Selector
effectWithNonReversingSelector = mkSelector "effectWithNonReversing"

-- | @Selector@ for @effectWithHideInactiveLayers@
effectWithHideInactiveLayersSelector :: Selector
effectWithHideInactiveLayersSelector = mkSelector "effectWithHideInactiveLayers"

-- | @Selector@ for @effectWithDimInactiveLayers@
effectWithDimInactiveLayersSelector :: Selector
effectWithDimInactiveLayersSelector = mkSelector "effectWithDimInactiveLayers"

