{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An observation resulting from an instance mask generation request. It contains an instance mask that labels instances in the mask that labels per pixel an instance.
--
-- Generated bindings for @VNInstanceMaskObservation@.
module ObjC.Vision.VNInstanceMaskObservation
  ( VNInstanceMaskObservation
  , IsVNInstanceMaskObservation(..)
  , generateMaskForInstances_error
  , generateMaskedImageOfInstances_fromRequestHandler_croppedToInstancesExtent_error
  , generateScaledMaskForImageForInstances_fromRequestHandler_error
  , instanceMask
  , allInstances
  , allInstancesSelector
  , generateMaskForInstances_errorSelector
  , generateMaskedImageOfInstances_fromRequestHandler_croppedToInstancesExtent_errorSelector
  , generateScaledMaskForImageForInstances_fromRequestHandler_errorSelector
  , instanceMaskSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The low res mask from the selected instances in the resolution of the performed analysis which is not upscaled to the image resolution.
--
-- @instances@ — An NSIndexSet of selected instances where 0 is the background. An empty set selects all instances but the background
--
-- @error@ — The address of a variable that will be populated with the error that describes the failure.  If the caller does not require this information, NULL can be passed. The pixel format of kCVPixelFormatType_OneComponent32Float
--
-- ObjC selector: @- generateMaskForInstances:error:@
generateMaskForInstances_error :: (IsVNInstanceMaskObservation vnInstanceMaskObservation, IsNSIndexSet instances, IsNSError error_) => vnInstanceMaskObservation -> instances -> error_ -> IO (Ptr ())
generateMaskForInstances_error vnInstanceMaskObservation instances error_ =
  sendMessage vnInstanceMaskObservation generateMaskForInstances_errorSelector (toNSIndexSet instances) (toNSError error_)

-- | High res image with everything but the selected instances removed to transparent black.
--
-- @instances@ — An NSIndexSet of selected instances where 0 is the background.
--
-- @croppedToInstancesExtent@ — Crops the image to the smallest rectangle containg all instances with remaining alpha elements. Setting this value to NO does not perform any cropping.
--
-- @error@ — The address of a variable that will be populated with the error that describes the failure.  If the caller does not require this information, NULL can be passed.
--
-- ObjC selector: @- generateMaskedImageOfInstances:fromRequestHandler:croppedToInstancesExtent:error:@
generateMaskedImageOfInstances_fromRequestHandler_croppedToInstancesExtent_error :: (IsVNInstanceMaskObservation vnInstanceMaskObservation, IsNSIndexSet instances, IsVNImageRequestHandler requestHandler, IsNSError error_) => vnInstanceMaskObservation -> instances -> requestHandler -> Bool -> error_ -> IO (Ptr ())
generateMaskedImageOfInstances_fromRequestHandler_croppedToInstancesExtent_error vnInstanceMaskObservation instances requestHandler cropResult error_ =
  sendMessage vnInstanceMaskObservation generateMaskedImageOfInstances_fromRequestHandler_croppedToInstancesExtent_errorSelector (toNSIndexSet instances) (toVNImageRequestHandler requestHandler) cropResult (toNSError error_)

-- | High res mask with the selected instances preserved while everything else is removed to transparent black.
--
-- @forInstances@ — An NSIndexSet of selected instances where 0 is the background.
--
-- @error@ — The address of a variable that will be populated with the error that describes the failure.  If the caller does not require this information, NULL can be passed.
--
-- ObjC selector: @- generateScaledMaskForImageForInstances:fromRequestHandler:error:@
generateScaledMaskForImageForInstances_fromRequestHandler_error :: (IsVNInstanceMaskObservation vnInstanceMaskObservation, IsNSIndexSet instances, IsVNImageRequestHandler requestHandler, IsNSError error_) => vnInstanceMaskObservation -> instances -> requestHandler -> error_ -> IO (Ptr ())
generateScaledMaskForImageForInstances_fromRequestHandler_error vnInstanceMaskObservation instances requestHandler error_ =
  sendMessage vnInstanceMaskObservation generateScaledMaskForImageForInstances_fromRequestHandler_errorSelector (toNSIndexSet instances) (toVNImageRequestHandler requestHandler) (toNSError error_)

-- | The resulting mask represents all instances in a mask image where 0 represents the background and all other values represent the indices of the instances identified. Note that a pixel can only correspond to one instance and not multiple instances.
--
-- ObjC selector: @- instanceMask@
instanceMask :: IsVNInstanceMaskObservation vnInstanceMaskObservation => vnInstanceMaskObservation -> IO (Ptr ())
instanceMask vnInstanceMaskObservation =
  sendMessage vnInstanceMaskObservation instanceMaskSelector

-- | *The IndexSet that encompases all instances except the background
--
-- ObjC selector: @- allInstances@
allInstances :: IsVNInstanceMaskObservation vnInstanceMaskObservation => vnInstanceMaskObservation -> IO (Id NSIndexSet)
allInstances vnInstanceMaskObservation =
  sendMessage vnInstanceMaskObservation allInstancesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @generateMaskForInstances:error:@
generateMaskForInstances_errorSelector :: Selector '[Id NSIndexSet, Id NSError] (Ptr ())
generateMaskForInstances_errorSelector = mkSelector "generateMaskForInstances:error:"

-- | @Selector@ for @generateMaskedImageOfInstances:fromRequestHandler:croppedToInstancesExtent:error:@
generateMaskedImageOfInstances_fromRequestHandler_croppedToInstancesExtent_errorSelector :: Selector '[Id NSIndexSet, Id VNImageRequestHandler, Bool, Id NSError] (Ptr ())
generateMaskedImageOfInstances_fromRequestHandler_croppedToInstancesExtent_errorSelector = mkSelector "generateMaskedImageOfInstances:fromRequestHandler:croppedToInstancesExtent:error:"

-- | @Selector@ for @generateScaledMaskForImageForInstances:fromRequestHandler:error:@
generateScaledMaskForImageForInstances_fromRequestHandler_errorSelector :: Selector '[Id NSIndexSet, Id VNImageRequestHandler, Id NSError] (Ptr ())
generateScaledMaskForImageForInstances_fromRequestHandler_errorSelector = mkSelector "generateScaledMaskForImageForInstances:fromRequestHandler:error:"

-- | @Selector@ for @instanceMask@
instanceMaskSelector :: Selector '[] (Ptr ())
instanceMaskSelector = mkSelector "instanceMask"

-- | @Selector@ for @allInstances@
allInstancesSelector :: Selector '[] (Id NSIndexSet)
allInstancesSelector = mkSelector "allInstances"

