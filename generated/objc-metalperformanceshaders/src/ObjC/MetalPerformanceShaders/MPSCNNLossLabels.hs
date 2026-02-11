{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNLossLabels
--
-- This depends on Metal.framework.
--
-- The MPSCNNLossLabels is used to hold the per-element weights buffer              used by both MPSCNNLoss forward filter and MPSNNLossGradient backward filter.              The MPSCNNLoss forward filter populates the MPSCNNLossLabels object              and the MPSNNLossGradient backward filter consumes the state object.
--
-- Generated bindings for @MPSCNNLossLabels@.
module ObjC.MetalPerformanceShaders.MPSCNNLossLabels
  ( MPSCNNLossLabels
  , IsMPSCNNLossLabels(..)
  , init_
  , initWithDevice_labelsDescriptor
  , lossImage
  , labelsImage
  , weightsImage
  , initSelector
  , initWithDevice_labelsDescriptorSelector
  , lossImageSelector
  , labelsImageSelector
  , weightsImageSelector


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

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Use one of the interfaces below instead.
--
-- ObjC selector: @- init@
init_ :: IsMPSCNNLossLabels mpscnnLossLabels => mpscnnLossLabels -> IO (Id MPSCNNLossLabels)
init_ mpscnnLossLabels  =
  sendMsg mpscnnLossLabels (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Set labels (aka targets, ground truth) for the MPSCNNLossLabels object.
--
-- The labels and weights data are copied into internal storage. The computed loss can either be a                                      scalar value (in batch mode, a single value per image in a batch) or it                                      can be one value per feature channel. Thus, the size of the loss image                                      must either match the size of the input source image or be {1, 1, 1},                                      which results in a scalar value. In this convinience initializer, the                                      assumed size of the loss image is {1, 1, 1}.
--
-- @device@ — Device the state resources will be created on.
--
-- @labelsDescriptor@ — Describes the labels data. This includes:                                          - The per-element labels data. The data must be in floating point format.                                          - Data layout of labels data. See MPSImage.h for more information.                                          - Size of labels data: (width, height, feature channels}.                                          - Optionally, row bytes of labels data.                                          - Optionally, slice bytes of labels data.
--
-- ObjC selector: @- initWithDevice:labelsDescriptor:@
initWithDevice_labelsDescriptor :: (IsMPSCNNLossLabels mpscnnLossLabels, IsMPSCNNLossDataDescriptor labelsDescriptor) => mpscnnLossLabels -> RawId -> labelsDescriptor -> IO (Id MPSCNNLossLabels)
initWithDevice_labelsDescriptor mpscnnLossLabels  device labelsDescriptor =
withObjCPtr labelsDescriptor $ \raw_labelsDescriptor ->
    sendMsg mpscnnLossLabels (mkSelector "initWithDevice:labelsDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_labelsDescriptor :: Ptr ())] >>= ownedObject . castPtr

-- | Loss image accessor method.
--
-- Returns: An autoreleased MPSImage object, containing the loss data.              The loss data is populated in the -encode call, thus the contents              are undefined until you -encode the filter.
--
-- In order to guarantee that the image is correctly synchronized for CPU side access,              it is the application's responsibility to call the [gradientState synchronizeOnCommandBuffer:]              method before accessing the data in the image.
--
-- ObjC selector: @- lossImage@
lossImage :: IsMPSCNNLossLabels mpscnnLossLabels => mpscnnLossLabels -> IO (Id MPSImage)
lossImage mpscnnLossLabels  =
  sendMsg mpscnnLossLabels (mkSelector "lossImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Labels image accessor method.
--
-- Returns: An autoreleased MPSImage object, containing the labels data.              The labels data is populated in the -initWithDevice call.
--
-- In order to guarantee that the image is correctly synchronized for CPU side access,              it is the application's responsibility to call the [gradientState synchronizeOnCommandBuffer:]              method before accessing the data in the image.
--
-- ObjC selector: @- labelsImage@
labelsImage :: IsMPSCNNLossLabels mpscnnLossLabels => mpscnnLossLabels -> IO (Id MPSImage)
labelsImage mpscnnLossLabels  =
  sendMsg mpscnnLossLabels (mkSelector "labelsImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Weights image accessor method.
--
-- Returns: An autoreleased MPSImage object, containing the weights data.              The weights data is populated in the -initWithDevice call.
--
-- In order to guarantee that the image is correctly synchronized for CPU side access,              it is the application's responsibility to call the [gradientState synchronizeOnCommandBuffer:]              method before accessing the data in the image.
--
-- ObjC selector: @- weightsImage@
weightsImage :: IsMPSCNNLossLabels mpscnnLossLabels => mpscnnLossLabels -> IO (Id MPSImage)
weightsImage mpscnnLossLabels  =
  sendMsg mpscnnLossLabels (mkSelector "weightsImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDevice:labelsDescriptor:@
initWithDevice_labelsDescriptorSelector :: Selector
initWithDevice_labelsDescriptorSelector = mkSelector "initWithDevice:labelsDescriptor:"

-- | @Selector@ for @lossImage@
lossImageSelector :: Selector
lossImageSelector = mkSelector "lossImage"

-- | @Selector@ for @labelsImage@
labelsImageSelector :: Selector
labelsImageSelector = mkSelector "labelsImage"

-- | @Selector@ for @weightsImage@
weightsImageSelector :: Selector
weightsImageSelector = mkSelector "weightsImage"

