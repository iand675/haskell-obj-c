{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @VNHumanHandPoseObservation@.
module ObjC.Vision.VNHumanHandPoseObservation
  ( VNHumanHandPoseObservation
  , IsVNHumanHandPoseObservation(..)
  , recognizedPointForJointName_error
  , recognizedPointsForJointsGroupName_error
  , availableJointNames
  , availableJointsGroupNames
  , chirality
  , availableJointNamesSelector
  , availableJointsGroupNamesSelector
  , chiralitySelector
  , recognizedPointForJointName_errorSelector
  , recognizedPointsForJointsGroupName_errorSelector

  -- * Enum types
  , VNChirality(VNChirality)
  , pattern VNChiralityUnknown
  , pattern VNChiralityLeft
  , pattern VNChiralityRight

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Vision.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Obtain a specific normalized point for a named human hand joint.
--
-- @jointName@ — The name of the human hand joint.
--
-- @error@ — The address of a variable that will be populated with the error that describes the failure.  If the caller does not require this information, NULL can be passed.
--
-- Returns: the recognized point, or nil if the point could not be obtained.
--
-- ObjC selector: @- recognizedPointForJointName:error:@
recognizedPointForJointName_error :: (IsVNHumanHandPoseObservation vnHumanHandPoseObservation, IsNSString jointName, IsNSError error_) => vnHumanHandPoseObservation -> jointName -> error_ -> IO (Id VNRecognizedPoint)
recognizedPointForJointName_error vnHumanHandPoseObservation jointName error_ =
  sendMessage vnHumanHandPoseObservation recognizedPointForJointName_errorSelector (toNSString jointName) (toNSError error_)

-- | Obtains the collection of points associated with a named human hand joints group.
--
-- The obtained collection is a dictionary that provides the mapping of human hand join names to the recognized point.
--
-- @jointsGroupName@ — The name of the human hand joints group.
--
-- @error@ — The address of a variable that will be populated with the error that describes the failure.  If the caller does not require this information, NULL can be passed.
--
-- Returns: a dictionary of recognized points in the group, or nil if an error was encountered.
--
-- ObjC selector: @- recognizedPointsForJointsGroupName:error:@
recognizedPointsForJointsGroupName_error :: (IsVNHumanHandPoseObservation vnHumanHandPoseObservation, IsNSString jointsGroupName, IsNSError error_) => vnHumanHandPoseObservation -> jointsGroupName -> error_ -> IO (Id NSDictionary)
recognizedPointsForJointsGroupName_error vnHumanHandPoseObservation jointsGroupName error_ =
  sendMessage vnHumanHandPoseObservation recognizedPointsForJointsGroupName_errorSelector (toNSString jointsGroupName) (toNSError error_)

-- | All of the joint names available in the observation.
--
-- ObjC selector: @- availableJointNames@
availableJointNames :: IsVNHumanHandPoseObservation vnHumanHandPoseObservation => vnHumanHandPoseObservation -> IO (Id NSArray)
availableJointNames vnHumanHandPoseObservation =
  sendMessage vnHumanHandPoseObservation availableJointNamesSelector

-- | All of the joints group names available in the observation.
--
-- ObjC selector: @- availableJointsGroupNames@
availableJointsGroupNames :: IsVNHumanHandPoseObservation vnHumanHandPoseObservation => vnHumanHandPoseObservation -> IO (Id NSArray)
availableJointsGroupNames vnHumanHandPoseObservation =
  sendMessage vnHumanHandPoseObservation availableJointsGroupNamesSelector

-- | The chirality of the hand.
--
-- ObjC selector: @- chirality@
chirality :: IsVNHumanHandPoseObservation vnHumanHandPoseObservation => vnHumanHandPoseObservation -> IO VNChirality
chirality vnHumanHandPoseObservation =
  sendMessage vnHumanHandPoseObservation chiralitySelector

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

-- | @Selector@ for @chirality@
chiralitySelector :: Selector '[] VNChirality
chiralitySelector = mkSelector "chirality"

