{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNAddGradient
--
-- This depends on Metal.framework.
--
-- Specifies the addition gradient operator.              This arithmetic gradient filter requires the following inputs: gradient image from              the previous layer (going backwards) and either the primary or the secondary source              image from the forward pass. You will need a separate filter for the primary and              secondary source images.
--
-- Without broadcasting, the arithmetic add gradient operation is a copy operation on              the input gradient image. It is the same operation for both the primary and secondary              source images (for x + y, d/dx(x + y) = 1, d/dy(x + y) = 1). This copy operation can              be optimized away by the graph interface.
--
-- Setting the broadcasting parameters results in a reduction operation (sum) across all              of the applicable broadcasting dimensions (rows, columns, feature channels, or any              combination thereof) to produce the destination image of the size that matches the              primary/secondary input images used in the forward pass.
--
-- Generated bindings for @MPSCNNAddGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNAddGradient
  ( MPSCNNAddGradient
  , IsMPSCNNAddGradient(..)
  , initWithDevice_isSecondarySourceFilter
  , initWithDevice_isSecondarySourceFilterSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize the addition gradient operator.
--
-- @device@ — The device the filter will run on.
--
-- @isSecondarySourceFilter@ — A boolean indicating whether the arithmetic gradient             filter is operating on the primary or secondary source image from the forward pass.
--
-- Returns: A valid MPSCNNAddGradient object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:isSecondarySourceFilter:@
initWithDevice_isSecondarySourceFilter :: IsMPSCNNAddGradient mpscnnAddGradient => mpscnnAddGradient -> RawId -> Bool -> IO (Id MPSCNNAddGradient)
initWithDevice_isSecondarySourceFilter mpscnnAddGradient device isSecondarySourceFilter =
  sendOwnedMessage mpscnnAddGradient initWithDevice_isSecondarySourceFilterSelector device isSecondarySourceFilter

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:isSecondarySourceFilter:@
initWithDevice_isSecondarySourceFilterSelector :: Selector '[RawId, Bool] (Id MPSCNNAddGradient)
initWithDevice_isSecondarySourceFilterSelector = mkSelector "initWithDevice:isSecondarySourceFilter:"

