{-# LANGUAGE PatternSynonyms #-}
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
  , recognizedPointForJointName_errorSelector
  , recognizedPointsForJointsGroupName_errorSelector
  , availableJointNamesSelector
  , availableJointsGroupNamesSelector
  , chiralitySelector

  -- * Enum types
  , VNChirality(VNChirality)
  , pattern VNChiralityUnknown
  , pattern VNChiralityLeft
  , pattern VNChiralityRight

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
recognizedPointForJointName_error vnHumanHandPoseObservation  jointName error_ =
withObjCPtr jointName $ \raw_jointName ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg vnHumanHandPoseObservation (mkSelector "recognizedPointForJointName:error:") (retPtr retVoid) [argPtr (castPtr raw_jointName :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

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
recognizedPointsForJointsGroupName_error vnHumanHandPoseObservation  jointsGroupName error_ =
withObjCPtr jointsGroupName $ \raw_jointsGroupName ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg vnHumanHandPoseObservation (mkSelector "recognizedPointsForJointsGroupName:error:") (retPtr retVoid) [argPtr (castPtr raw_jointsGroupName :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | All of the joint names available in the observation.
--
-- ObjC selector: @- availableJointNames@
availableJointNames :: IsVNHumanHandPoseObservation vnHumanHandPoseObservation => vnHumanHandPoseObservation -> IO (Id NSArray)
availableJointNames vnHumanHandPoseObservation  =
  sendMsg vnHumanHandPoseObservation (mkSelector "availableJointNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | All of the joints group names available in the observation.
--
-- ObjC selector: @- availableJointsGroupNames@
availableJointsGroupNames :: IsVNHumanHandPoseObservation vnHumanHandPoseObservation => vnHumanHandPoseObservation -> IO (Id NSArray)
availableJointsGroupNames vnHumanHandPoseObservation  =
  sendMsg vnHumanHandPoseObservation (mkSelector "availableJointsGroupNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The chirality of the hand.
--
-- ObjC selector: @- chirality@
chirality :: IsVNHumanHandPoseObservation vnHumanHandPoseObservation => vnHumanHandPoseObservation -> IO VNChirality
chirality vnHumanHandPoseObservation  =
  fmap (coerce :: CLong -> VNChirality) $ sendMsg vnHumanHandPoseObservation (mkSelector "chirality") retCLong []

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

-- | @Selector@ for @chirality@
chiralitySelector :: Selector
chiralitySelector = mkSelector "chirality"

