{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @VNHumanBodyPoseObservation@.
module ObjC.Vision.VNHumanBodyPoseObservation
  ( VNHumanBodyPoseObservation
  , IsVNHumanBodyPoseObservation(..)
  , recognizedPointForJointName_error
  , recognizedPointsForJointsGroupName_error
  , availableJointNames
  , availableJointsGroupNames
  , availableJointNamesSelector
  , availableJointsGroupNamesSelector
  , recognizedPointForJointName_errorSelector
  , recognizedPointsForJointsGroupName_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Obtain a specific normalized point for a named human body joint.
--
-- @jointName@ — The name of the human body joint.
--
-- @error@ — The address of a variable that will be populated with the error that describes the failure.  If the caller does not require this information, NULL can be passed.
--
-- Returns: the recognized point, or nil if the point could not be obtained.
--
-- ObjC selector: @- recognizedPointForJointName:error:@
recognizedPointForJointName_error :: (IsVNHumanBodyPoseObservation vnHumanBodyPoseObservation, IsNSString jointName, IsNSError error_) => vnHumanBodyPoseObservation -> jointName -> error_ -> IO (Id VNRecognizedPoint)
recognizedPointForJointName_error vnHumanBodyPoseObservation jointName error_ =
  sendMessage vnHumanBodyPoseObservation recognizedPointForJointName_errorSelector (toNSString jointName) (toNSError error_)

-- | Obtains the collection of points associated with a named human body joints group.
--
-- The obtained collection is a dictionary that provides the mapping of human body join names to the recognized point.
--
-- @jointsGroupName@ — The name of the human body joints group.
--
-- @error@ — The address of a variable that will be populated with the error that describes the failure.  If the caller does not require this information, NULL can be passed.
--
-- Returns: a dictionary of recognized points in the group, or nil if an error was encountered.
--
-- ObjC selector: @- recognizedPointsForJointsGroupName:error:@
recognizedPointsForJointsGroupName_error :: (IsVNHumanBodyPoseObservation vnHumanBodyPoseObservation, IsNSString jointsGroupName, IsNSError error_) => vnHumanBodyPoseObservation -> jointsGroupName -> error_ -> IO (Id NSDictionary)
recognizedPointsForJointsGroupName_error vnHumanBodyPoseObservation jointsGroupName error_ =
  sendMessage vnHumanBodyPoseObservation recognizedPointsForJointsGroupName_errorSelector (toNSString jointsGroupName) (toNSError error_)

-- | All of the joint names available in the observation.
--
-- ObjC selector: @- availableJointNames@
availableJointNames :: IsVNHumanBodyPoseObservation vnHumanBodyPoseObservation => vnHumanBodyPoseObservation -> IO (Id NSArray)
availableJointNames vnHumanBodyPoseObservation =
  sendMessage vnHumanBodyPoseObservation availableJointNamesSelector

-- | All of the joints group names available in the observation.
--
-- ObjC selector: @- availableJointsGroupNames@
availableJointsGroupNames :: IsVNHumanBodyPoseObservation vnHumanBodyPoseObservation => vnHumanBodyPoseObservation -> IO (Id NSArray)
availableJointsGroupNames vnHumanBodyPoseObservation =
  sendMessage vnHumanBodyPoseObservation availableJointsGroupNamesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @recognizedPointForJointName:error:@
recognizedPointForJointName_errorSelector :: Selector '[Id NSString, Id NSError] (Id VNRecognizedPoint)
recognizedPointForJointName_errorSelector = mkSelector "recognizedPointForJointName:error:"

-- | @Selector@ for @recognizedPointsForJointsGroupName:error:@
recognizedPointsForJointsGroupName_errorSelector :: Selector '[Id NSString, Id NSError] (Id NSDictionary)
recognizedPointsForJointsGroupName_errorSelector = mkSelector "recognizedPointsForJointsGroupName:error:"

-- | @Selector@ for @availableJointNames@
availableJointNamesSelector :: Selector '[] (Id NSArray)
availableJointNamesSelector = mkSelector "availableJointNames"

-- | @Selector@ for @availableJointsGroupNames@
availableJointsGroupNamesSelector :: Selector '[] (Id NSArray)
availableJointsGroupNamesSelector = mkSelector "availableJointsGroupNames"

