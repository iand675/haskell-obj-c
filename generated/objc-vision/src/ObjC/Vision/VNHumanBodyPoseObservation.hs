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
  , recognizedPointForJointName_errorSelector
  , recognizedPointsForJointsGroupName_errorSelector
  , availableJointNamesSelector
  , availableJointsGroupNamesSelector


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
recognizedPointForJointName_error vnHumanBodyPoseObservation  jointName error_ =
withObjCPtr jointName $ \raw_jointName ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg vnHumanBodyPoseObservation (mkSelector "recognizedPointForJointName:error:") (retPtr retVoid) [argPtr (castPtr raw_jointName :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

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
recognizedPointsForJointsGroupName_error vnHumanBodyPoseObservation  jointsGroupName error_ =
withObjCPtr jointsGroupName $ \raw_jointsGroupName ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg vnHumanBodyPoseObservation (mkSelector "recognizedPointsForJointsGroupName:error:") (retPtr retVoid) [argPtr (castPtr raw_jointsGroupName :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | All of the joint names available in the observation.
--
-- ObjC selector: @- availableJointNames@
availableJointNames :: IsVNHumanBodyPoseObservation vnHumanBodyPoseObservation => vnHumanBodyPoseObservation -> IO (Id NSArray)
availableJointNames vnHumanBodyPoseObservation  =
  sendMsg vnHumanBodyPoseObservation (mkSelector "availableJointNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | All of the joints group names available in the observation.
--
-- ObjC selector: @- availableJointsGroupNames@
availableJointsGroupNames :: IsVNHumanBodyPoseObservation vnHumanBodyPoseObservation => vnHumanBodyPoseObservation -> IO (Id NSArray)
availableJointsGroupNames vnHumanBodyPoseObservation  =
  sendMsg vnHumanBodyPoseObservation (mkSelector "availableJointsGroupNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @recognizedPointForJointName:error:@
recognizedPointForJointName_errorSelector :: Selector
recognizedPointForJointName_errorSelector = mkSelector "recognizedPointForJointName:error:"

-- | @Selector@ for @recognizedPointsForJointsGroupName:error:@
recognizedPointsForJointsGroupName_errorSelector :: Selector
recognizedPointsForJointsGroupName_errorSelector = mkSelector "recognizedPointsForJointsGroupName:error:"

-- | @Selector@ for @availableJointNames@
availableJointNamesSelector :: Selector
availableJointNamesSelector = mkSelector "availableJointNames"

-- | @Selector@ for @availableJointsGroupNames@
availableJointsGroupNamesSelector :: Selector
availableJointsGroupNamesSelector = mkSelector "availableJointsGroupNames"

