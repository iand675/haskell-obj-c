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
  , supportedJointNamesAndReturnErrorSelector
  , supportedJointsGroupNamesAndReturnErrorSelector
  , resultsSelector


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
supportedJointNamesAndReturnError vnDetectAnimalBodyPoseRequest  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg vnDetectAnimalBodyPoseRequest (mkSelector "supportedJointNamesAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Obtain the collection of animal body joints group names that are supported by a request object configured with a request revision.
--
-- @error@ — The address of a variable that will be populated with an error upon failure.  If the caller does not need this information, NULL can be passed.
--
-- Returns: An array of VNAnimalBodyPoseObservationJointsGroupName symbols that are supported by the request, or nil if a failure occurs.
--
-- ObjC selector: @- supportedJointsGroupNamesAndReturnError:@
supportedJointsGroupNamesAndReturnError :: (IsVNDetectAnimalBodyPoseRequest vnDetectAnimalBodyPoseRequest, IsNSError error_) => vnDetectAnimalBodyPoseRequest -> error_ -> IO (Id NSArray)
supportedJointsGroupNamesAndReturnError vnDetectAnimalBodyPoseRequest  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg vnDetectAnimalBodyPoseRequest (mkSelector "supportedJointsGroupNamesAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | VNAnimalBodyPoseObservation results.
--
-- ObjC selector: @- results@
results :: IsVNDetectAnimalBodyPoseRequest vnDetectAnimalBodyPoseRequest => vnDetectAnimalBodyPoseRequest -> IO (Id NSArray)
results vnDetectAnimalBodyPoseRequest  =
  sendMsg vnDetectAnimalBodyPoseRequest (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportedJointNamesAndReturnError:@
supportedJointNamesAndReturnErrorSelector :: Selector
supportedJointNamesAndReturnErrorSelector = mkSelector "supportedJointNamesAndReturnError:"

-- | @Selector@ for @supportedJointsGroupNamesAndReturnError:@
supportedJointsGroupNamesAndReturnErrorSelector :: Selector
supportedJointsGroupNamesAndReturnErrorSelector = mkSelector "supportedJointsGroupNamesAndReturnError:"

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

