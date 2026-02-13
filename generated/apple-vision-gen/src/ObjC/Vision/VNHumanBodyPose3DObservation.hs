{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @VNHumanBodyPose3DObservation@.
module ObjC.Vision.VNHumanBodyPose3DObservation
  ( VNHumanBodyPose3DObservation
  , IsVNHumanBodyPose3DObservation(..)
  , recognizedPointsForJointsGroupName_error
  , recognizedPointForJointName_error
  , pointInImageForJointName_error
  , parentJointNameForJointName
  , heightEstimation
  , availableJointsGroupNames
  , availableJointNames
  , bodyHeight
  , availableJointNamesSelector
  , availableJointsGroupNamesSelector
  , bodyHeightSelector
  , heightEstimationSelector
  , parentJointNameForJointNameSelector
  , pointInImageForJointName_errorSelector
  , recognizedPointForJointName_errorSelector
  , recognizedPointsForJointsGroupName_errorSelector

  -- * Enum types
  , VNHumanBodyPose3DObservationHeightEstimation(VNHumanBodyPose3DObservationHeightEstimation)
  , pattern VNHumanBodyPose3DObservationHeightEstimationReference
  , pattern VNHumanBodyPose3DObservationHeightEstimationMeasured

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

-- | Obtains the collection of joints associated with a named human body joints group.
--
-- The obtained collection is a dictionary that provides the mapping of human joint names to the recognized point.
--
-- @jointsGroupName@ — The name of the human body joints group.
--
-- @error@ — The address of a variable that will be populated with the error that describes the failure.  If the caller does not require this information, NULL can be passed.
--
-- Returns: a dictionary of recognized points in the group, or nil if an error was encountered.
--
-- ObjC selector: @- recognizedPointsForJointsGroupName:error:@
recognizedPointsForJointsGroupName_error :: (IsVNHumanBodyPose3DObservation vnHumanBodyPose3DObservation, IsNSString jointsGroupName, IsNSError error_) => vnHumanBodyPose3DObservation -> jointsGroupName -> error_ -> IO (Id NSDictionary)
recognizedPointsForJointsGroupName_error vnHumanBodyPose3DObservation jointsGroupName error_ =
  sendMessage vnHumanBodyPose3DObservation recognizedPointsForJointsGroupName_errorSelector (toNSString jointsGroupName) (toNSError error_)

-- | Obtain a specific point for a named human body joint.    Each returned @VNHumanBodyRecognizedPoint3D@ instance contains position relative to the model (@position@) and the parent joint (@localPosition@)    Model position is relative to root joint (hip) for a named human body joint in meters .    Local position is relative to parent joint for a named human body joint in meters.
--
-- @jointName@ — The name of the human body joint.
--
-- @error@ — The address of a variable that will be populated with the error that describes the failure.  If the caller does not require this information, NULL can be passed.
--
-- Returns: The recognized point, or nil if the point could not be obtained.
--
-- ObjC selector: @- recognizedPointForJointName:error:@
recognizedPointForJointName_error :: (IsVNHumanBodyPose3DObservation vnHumanBodyPose3DObservation, IsNSString jointName, IsNSError error_) => vnHumanBodyPose3DObservation -> jointName -> error_ -> IO (Id VNHumanBodyRecognizedPoint3D)
recognizedPointForJointName_error vnHumanBodyPose3DObservation jointName error_ =
  sendMessage vnHumanBodyPose3DObservation recognizedPointForJointName_errorSelector (toNSString jointName) (toNSError error_)

-- | Obtain 2D point relative to the input image for named human body joint
--
-- @jointName@ — The name of the human body joint
--
-- Returns: A projection of the determined 3D position onto the original 2D image in normalized, lower left origin coordinates
--
-- ObjC selector: @- pointInImageForJointName:error:@
pointInImageForJointName_error :: (IsVNHumanBodyPose3DObservation vnHumanBodyPose3DObservation, IsNSString jointName, IsNSError error_) => vnHumanBodyPose3DObservation -> jointName -> error_ -> IO (Id VNPoint)
pointInImageForJointName_error vnHumanBodyPose3DObservation jointName error_ =
  sendMessage vnHumanBodyPose3DObservation pointInImageForJointName_errorSelector (toNSString jointName) (toNSError error_)

-- | Obtain the parent joint of a specified joint
--
-- @jointName@ — The name of the human body joint
--
-- Returns: The name of the parent joint
--
-- ObjC selector: @- parentJointNameForJointName:@
parentJointNameForJointName :: (IsVNHumanBodyPose3DObservation vnHumanBodyPose3DObservation, IsNSString jointName) => vnHumanBodyPose3DObservation -> jointName -> IO (Id NSString)
parentJointNameForJointName vnHumanBodyPose3DObservation jointName =
  sendMessage vnHumanBodyPose3DObservation parentJointNameForJointNameSelector (toNSString jointName)

-- | Technique used to estimate body height.   @VNHumanBodyPose3DObservationHeightEstimationMeasured@   indicates@bodyHeight@ returns measured height in meters more accurate to true world height. @VNHumanBodyPose3DObservationHeightEstimationReference@ indicates @bodyHeight@ returns reference height of 1.8 m
--
-- ObjC selector: @- heightEstimation@
heightEstimation :: IsVNHumanBodyPose3DObservation vnHumanBodyPose3DObservation => vnHumanBodyPose3DObservation -> IO VNHumanBodyPose3DObservationHeightEstimation
heightEstimation vnHumanBodyPose3DObservation =
  sendMessage vnHumanBodyPose3DObservation heightEstimationSelector

-- | All of the joints group names available in the observation.
--
-- ObjC selector: @- availableJointsGroupNames@
availableJointsGroupNames :: IsVNHumanBodyPose3DObservation vnHumanBodyPose3DObservation => vnHumanBodyPose3DObservation -> IO (Id NSArray)
availableJointsGroupNames vnHumanBodyPose3DObservation =
  sendMessage vnHumanBodyPose3DObservation availableJointsGroupNamesSelector

-- | All of the joint names available in the observation.
--
-- ObjC selector: @- availableJointNames@
availableJointNames :: IsVNHumanBodyPose3DObservation vnHumanBodyPose3DObservation => vnHumanBodyPose3DObservation -> IO (Id NSArray)
availableJointNames vnHumanBodyPose3DObservation =
  sendMessage vnHumanBodyPose3DObservation availableJointNamesSelector

-- | Estimated human height, in meters.
--
-- Note: A measured height will be returned in meters if  @heightEstimation@ is  @VNHumanBodyPose3DObservationHeightEstimationMeasured@, otherwise reference height of 1.8 meters is returned for @VNHumanBodyPose3DObservationHeightEstimationReference@
--
-- ObjC selector: @- bodyHeight@
bodyHeight :: IsVNHumanBodyPose3DObservation vnHumanBodyPose3DObservation => vnHumanBodyPose3DObservation -> IO CFloat
bodyHeight vnHumanBodyPose3DObservation =
  sendMessage vnHumanBodyPose3DObservation bodyHeightSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @recognizedPointsForJointsGroupName:error:@
recognizedPointsForJointsGroupName_errorSelector :: Selector '[Id NSString, Id NSError] (Id NSDictionary)
recognizedPointsForJointsGroupName_errorSelector = mkSelector "recognizedPointsForJointsGroupName:error:"

-- | @Selector@ for @recognizedPointForJointName:error:@
recognizedPointForJointName_errorSelector :: Selector '[Id NSString, Id NSError] (Id VNHumanBodyRecognizedPoint3D)
recognizedPointForJointName_errorSelector = mkSelector "recognizedPointForJointName:error:"

-- | @Selector@ for @pointInImageForJointName:error:@
pointInImageForJointName_errorSelector :: Selector '[Id NSString, Id NSError] (Id VNPoint)
pointInImageForJointName_errorSelector = mkSelector "pointInImageForJointName:error:"

-- | @Selector@ for @parentJointNameForJointName:@
parentJointNameForJointNameSelector :: Selector '[Id NSString] (Id NSString)
parentJointNameForJointNameSelector = mkSelector "parentJointNameForJointName:"

-- | @Selector@ for @heightEstimation@
heightEstimationSelector :: Selector '[] VNHumanBodyPose3DObservationHeightEstimation
heightEstimationSelector = mkSelector "heightEstimation"

-- | @Selector@ for @availableJointsGroupNames@
availableJointsGroupNamesSelector :: Selector '[] (Id NSArray)
availableJointsGroupNamesSelector = mkSelector "availableJointsGroupNames"

-- | @Selector@ for @availableJointNames@
availableJointNamesSelector :: Selector '[] (Id NSArray)
availableJointNamesSelector = mkSelector "availableJointNames"

-- | @Selector@ for @bodyHeight@
bodyHeightSelector :: Selector '[] CFloat
bodyHeightSelector = mkSelector "bodyHeight"

