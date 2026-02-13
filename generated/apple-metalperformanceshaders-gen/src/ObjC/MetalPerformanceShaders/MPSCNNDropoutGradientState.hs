{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNDropoutGradientState
--
-- This depends on Metal.framework.
--
-- The MPSCNNDropoutGradientState is used to hold the mask used by both              MPSCNNDropout forward filter and MPSCNNDropoutGradient backward filter.              The MPSCNNDropout forward filter populates the MPSCNNDropoutGradientState              object and the MPSCNNDropoutGradient backward filter consumes the state              object.
--
-- While the mask is stored internally, the mask data is accessible by the              user for debugging purposes via an accessor method.
--
-- Generated bindings for @MPSCNNDropoutGradientState@.
module ObjC.MetalPerformanceShaders.MPSCNNDropoutGradientState
  ( MPSCNNDropoutGradientState
  , IsMPSCNNDropoutGradientState(..)
  , init_
  , maskData
  , initSelector
  , maskDataSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMPSCNNDropoutGradientState mpscnnDropoutGradientState => mpscnnDropoutGradientState -> IO (Id MPSCNNDropoutGradientState)
init_ mpscnnDropoutGradientState =
  sendOwnedMessage mpscnnDropoutGradientState initSelector

-- | Mask data accessor method.
--
-- Returns: An autoreleased NSData object, containing the mask data.              The mask data is populated in the -encode call, thus the contents              are undefined until you -encode the filter.              Use for debugging purposes only.
--
-- In order to gaurantee that the mask data is correctly synchronized for CPU side access,              it is the application's responsibility to call the [gradientState synchronizeOnCommandBuffer:]              method before accessing the mask data.
--
-- ObjC selector: @- maskData@
maskData :: IsMPSCNNDropoutGradientState mpscnnDropoutGradientState => mpscnnDropoutGradientState -> IO (Id NSData)
maskData mpscnnDropoutGradientState =
  sendMessage mpscnnDropoutGradientState maskDataSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPSCNNDropoutGradientState)
initSelector = mkSelector "init"

-- | @Selector@ for @maskData@
maskDataSelector :: Selector '[] (Id NSData)
maskDataSelector = mkSelector "maskData"

