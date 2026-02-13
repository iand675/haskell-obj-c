{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNMultiplyGradient
--
-- This depends on Metal.framework.
--
-- Specifies the multiplication gradient operator.              This arithmetic gradient filter requires the following inputs: gradient image from              the previous layer (going backwards) and either the primary or the secondary source              image from the forward pass. You will need a separate filter for the primary and              secondary source images.
--
-- Without broadcasting, the arithmetic multiply gradient operation is an element-wise              multiplication operation between the gradient image from the previous layer (going              backwards) and:              - The secondary source image from the forward pass for the primary source filter                (for x * y, d/dx(x * y) = y).              - The primary source image from the forward pass for the secondary source filter                (for x * y, d/dy(x * y) = x).
--
-- Setting the broadcasting parameters results in a reduction operation (sum) across all              of the applicable broadcasting dimensions (rows, columns, feature channels, or any              combination thereof) to produce the destination image of the size that matches the              primary/secondary input images used in the forward pass.
--
-- Generated bindings for @MPSCNNMultiplyGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNMultiplyGradient
  ( MPSCNNMultiplyGradient
  , IsMPSCNNMultiplyGradient(..)
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

-- | Initialize the multiplication gradient operator.
--
-- @device@ — The device the filter will run on.
--
-- @isSecondarySourceFilter@ — A boolean indicating whether the arithmetic gradient             filter is operating on the primary or secondary source image from the forward pass.
--
-- Returns: A valid MPSCNNMultiplyGradient object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:isSecondarySourceFilter:@
initWithDevice_isSecondarySourceFilter :: IsMPSCNNMultiplyGradient mpscnnMultiplyGradient => mpscnnMultiplyGradient -> RawId -> Bool -> IO (Id MPSCNNMultiplyGradient)
initWithDevice_isSecondarySourceFilter mpscnnMultiplyGradient device isSecondarySourceFilter =
  sendOwnedMessage mpscnnMultiplyGradient initWithDevice_isSecondarySourceFilterSelector device isSecondarySourceFilter

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:isSecondarySourceFilter:@
initWithDevice_isSecondarySourceFilterSelector :: Selector '[RawId, Bool] (Id MPSCNNMultiplyGradient)
initWithDevice_isSecondarySourceFilterSelector = mkSelector "initWithDevice:isSecondarySourceFilter:"

