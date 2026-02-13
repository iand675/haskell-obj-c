{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Detects specific landmark points on animal bodies.
--
-- This request will produce a collection of VNAnimalBodyPoseObservation objects which describe the pose of each detected animal body.
--
-- Generated bindings for @VNDetectAnimalBodyPoseRequest@.
module ObjC.Vision.VNDetectAnimalBodyPoseRequest
  ( VNDetectAnimalBodyPoseRequest
  , IsVNDetectAnimalBodyPoseRequest(..)
  , supportedJointNamesAndReturnError
  , supportedJointsGroupNamesAndReturnError
  , results
  , resultsSelector
  , supportedJointNamesAndReturnErrorSelector
  , supportedJointsGroupNamesAndReturnErrorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Obtain the collection of animal body joint names that are supported by a request object configured with a request revision.
--
-- @error@ — The address of a variable that will be populated with an error upon failure.  If the caller does not need this information, NULL can be passed.
--
-- Returns: An array of VNAnimalBodyPoseObservationJointName symbols that are supported by the request, or nil if a failure occurs.
--
-- ObjC selector: @- supportedJointNamesAndReturnError:@
supportedJointNamesAndReturnError :: (IsVNDetectAnimalBodyPoseRequest vnDetectAnimalBodyPoseRequest, IsNSError error_) => vnDetectAnimalBodyPoseRequest -> error_ -> IO (Id NSArray)
supportedJointNamesAndReturnError vnDetectAnimalBodyPoseRequest error_ =
  sendMessage vnDetectAnimalBodyPoseRequest supportedJointNamesAndReturnErrorSelector (toNSError error_)

-- | Obtain the collection of animal body joints group names that are supported by a request object configured with a request revision.
--
-- @error@ — The address of a variable that will be populated with an error upon failure.  If the caller does not need this information, NULL can be passed.
--
-- Returns: An array of VNAnimalBodyPoseObservationJointsGroupName symbols that are supported by the request, or nil if a failure occurs.
--
-- ObjC selector: @- supportedJointsGroupNamesAndReturnError:@
supportedJointsGroupNamesAndReturnError :: (IsVNDetectAnimalBodyPoseRequest vnDetectAnimalBodyPoseRequest, IsNSError error_) => vnDetectAnimalBodyPoseRequest -> error_ -> IO (Id NSArray)
supportedJointsGroupNamesAndReturnError vnDetectAnimalBodyPoseRequest error_ =
  sendMessage vnDetectAnimalBodyPoseRequest supportedJointsGroupNamesAndReturnErrorSelector (toNSError error_)

-- | VNAnimalBodyPoseObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectAnimalBodyPoseRequest vnDetectAnimalBodyPoseRequest => vnDetectAnimalBodyPoseRequest -> IO (Id NSArray)
results vnDetectAnimalBodyPoseRequest =
  sendMessage vnDetectAnimalBodyPoseRequest resultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportedJointNamesAndReturnError:@
supportedJointNamesAndReturnErrorSelector :: Selector '[Id NSError] (Id NSArray)
supportedJointNamesAndReturnErrorSelector = mkSelector "supportedJointNamesAndReturnError:"

-- | @Selector@ for @supportedJointsGroupNamesAndReturnError:@
supportedJointsGroupNamesAndReturnErrorSelector :: Selector '[Id NSError] (Id NSArray)
supportedJointsGroupNamesAndReturnErrorSelector = mkSelector "supportedJointsGroupNamesAndReturnError:"

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

