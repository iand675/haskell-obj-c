{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @VNAnimalBodyPoseObservation@.
module ObjC.Vision.VNAnimalBodyPoseObservation
  ( VNAnimalBodyPoseObservation
  , IsVNAnimalBodyPoseObservation(..)
  , recognizedPointForJointName_error
  , recognizedPointsForJointsGroupName_error
  , availableJointNames
  , availableJointGroupNames
  , recognizedPointForJointName_errorSelector
  , recognizedPointsForJointsGroupName_errorSelector
  , availableJointNamesSelector
  , availableJointGroupNamesSelector


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

-- | Obtain a specific normalized point for a named animal body joint.
--
-- @jointName@ — The name of the animal body joint.
--
-- @error@ — The address of a variable that will be populated with the error that describes the failure.  If the caller does not require this information, NULL can be passed.
--
-- Returns: the recognized point, or nil if the point could not be obtained.
--
-- ObjC selector: @- recognizedPointForJointName:error:@
recognizedPointForJointName_error :: (IsVNAnimalBodyPoseObservation vnAnimalBodyPoseObservation, IsNSString jointName, IsNSError error_) => vnAnimalBodyPoseObservation -> jointName -> error_ -> IO (Id VNRecognizedPoint)
recognizedPointForJointName_error vnAnimalBodyPoseObservation  jointName error_ =
withObjCPtr jointName $ \raw_jointName ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg vnAnimalBodyPoseObservation (mkSelector "recognizedPointForJointName:error:") (retPtr retVoid) [argPtr (castPtr raw_jointName :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Obtains the collection of points associated with a named animal body joints group.
--
-- The obtained collection is a dictionary that provides the mapping of animal join names to the recognized point.
--
-- @jointsGroupName@ — The name of the animal body joints group.
--
-- @error@ — The address of a variable that will be populated with the error that describes the failure.  If the caller does not require this information, NULL can be passed.
--
-- Returns: a dictionary of recognized points in the group, or nil if an error was encountered.
--
-- ObjC selector: @- recognizedPointsForJointsGroupName:error:@
recognizedPointsForJointsGroupName_error :: (IsVNAnimalBodyPoseObservation vnAnimalBodyPoseObservation, IsNSString jointsGroupName, IsNSError error_) => vnAnimalBodyPoseObservation -> jointsGroupName -> error_ -> IO (Id NSDictionary)
recognizedPointsForJointsGroupName_error vnAnimalBodyPoseObservation  jointsGroupName error_ =
withObjCPtr jointsGroupName $ \raw_jointsGroupName ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg vnAnimalBodyPoseObservation (mkSelector "recognizedPointsForJointsGroupName:error:") (retPtr retVoid) [argPtr (castPtr raw_jointsGroupName :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | All animal joint names available in the observation.
--
-- ObjC selector: @- availableJointNames@
availableJointNames :: IsVNAnimalBodyPoseObservation vnAnimalBodyPoseObservation => vnAnimalBodyPoseObservation -> IO (Id NSArray)
availableJointNames vnAnimalBodyPoseObservation  =
  sendMsg vnAnimalBodyPoseObservation (mkSelector "availableJointNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | All animal joints group names available in the observation.
--
-- ObjC selector: @- availableJointGroupNames@
availableJointGroupNames :: IsVNAnimalBodyPoseObservation vnAnimalBodyPoseObservation => vnAnimalBodyPoseObservation -> IO (Id NSArray)
availableJointGroupNames vnAnimalBodyPoseObservation  =
  sendMsg vnAnimalBodyPoseObservation (mkSelector "availableJointGroupNames") (retPtr retVoid) [] >>= retainedObject . castPtr

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

-- | @Selector@ for @availableJointGroupNames@
availableJointGroupNamesSelector :: Selector
availableJointGroupNamesSelector = mkSelector "availableJointGroupNames"

