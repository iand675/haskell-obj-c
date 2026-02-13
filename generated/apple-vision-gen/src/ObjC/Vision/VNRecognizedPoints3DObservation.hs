{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNRecognizedPoints3D
--
-- Observation
--
-- VNObservation
--
-- VNRecognizedPointsObservation is a request result detailing points in an image.
--
-- Generated bindings for @VNRecognizedPoints3DObservation@.
module ObjC.Vision.VNRecognizedPoints3DObservation
  ( VNRecognizedPoints3DObservation
  , IsVNRecognizedPoints3DObservation(..)
  , new
  , init_
  , recognizedPointForKey_error
  , recognizedPointsForGroupKey_error
  , availableKeys
  , availableGroupKeys
  , availableGroupKeysSelector
  , availableKeysSelector
  , initSelector
  , newSelector
  , recognizedPointForKey_errorSelector
  , recognizedPointsForGroupKey_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VNRecognizedPoints3DObservation)
new  =
  do
    cls' <- getRequiredClass "VNRecognizedPoints3DObservation"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVNRecognizedPoints3DObservation vnRecognizedPoints3DObservation => vnRecognizedPoints3DObservation -> IO (Id VNRecognizedPoints3DObservation)
init_ vnRecognizedPoints3DObservation =
  sendOwnedMessage vnRecognizedPoints3DObservation initSelector

-- | Obtains a specific normalized recognized point.
--
-- @pointKey@ — The key specifying the desired recognized point.
--
-- @error@ — The address of a variable that will be populated with the error that describes the failure.  If the caller does not require this information, NULL can be passed.
--
-- Returns: the recognized point, or nil if the specific point is not defined.
--
-- ObjC selector: @- recognizedPointForKey:error:@
recognizedPointForKey_error :: (IsVNRecognizedPoints3DObservation vnRecognizedPoints3DObservation, IsNSString pointKey, IsNSError error_) => vnRecognizedPoints3DObservation -> pointKey -> error_ -> IO (Id VNRecognizedPoint3D)
recognizedPointForKey_error vnRecognizedPoints3DObservation pointKey error_ =
  sendMessage vnRecognizedPoints3DObservation recognizedPointForKey_errorSelector (toNSString pointKey) (toNSError error_)

-- | Obtains the collection of points associated with an identified grouping.
--
-- The obtained collection is a dictionary that provides the mapping of a recognized point's key to the recognized point.
--
-- @groupKey@ — The key representing a specific grouping of points.
--
-- @error@ — The address of a variable that will be populated with the error that describes the failure.  If the caller does not require this information, NULL can be passed.
--
-- Returns: the dictionary of recognized points in the group, or nil if an error was encountered.
--
-- ObjC selector: @- recognizedPointsForGroupKey:error:@
recognizedPointsForGroupKey_error :: (IsVNRecognizedPoints3DObservation vnRecognizedPoints3DObservation, IsNSString groupKey, IsNSError error_) => vnRecognizedPoints3DObservation -> groupKey -> error_ -> IO (Id NSDictionary)
recognizedPointsForGroupKey_error vnRecognizedPoints3DObservation groupKey error_ =
  sendMessage vnRecognizedPoints3DObservation recognizedPointsForGroupKey_errorSelector (toNSString groupKey) (toNSError error_)

-- | Returns all of the point group keys available in the observation.
--
-- ObjC selector: @- availableKeys@
availableKeys :: IsVNRecognizedPoints3DObservation vnRecognizedPoints3DObservation => vnRecognizedPoints3DObservation -> IO (Id NSArray)
availableKeys vnRecognizedPoints3DObservation =
  sendMessage vnRecognizedPoints3DObservation availableKeysSelector

-- | The availableGroupKeys property returns all of the point group labels usable with the observation.
--
-- ObjC selector: @- availableGroupKeys@
availableGroupKeys :: IsVNRecognizedPoints3DObservation vnRecognizedPoints3DObservation => vnRecognizedPoints3DObservation -> IO (Id NSArray)
availableGroupKeys vnRecognizedPoints3DObservation =
  sendMessage vnRecognizedPoints3DObservation availableGroupKeysSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VNRecognizedPoints3DObservation)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNRecognizedPoints3DObservation)
initSelector = mkSelector "init"

-- | @Selector@ for @recognizedPointForKey:error:@
recognizedPointForKey_errorSelector :: Selector '[Id NSString, Id NSError] (Id VNRecognizedPoint3D)
recognizedPointForKey_errorSelector = mkSelector "recognizedPointForKey:error:"

-- | @Selector@ for @recognizedPointsForGroupKey:error:@
recognizedPointsForGroupKey_errorSelector :: Selector '[Id NSString, Id NSError] (Id NSDictionary)
recognizedPointsForGroupKey_errorSelector = mkSelector "recognizedPointsForGroupKey:error:"

-- | @Selector@ for @availableKeys@
availableKeysSelector :: Selector '[] (Id NSArray)
availableKeysSelector = mkSelector "availableKeys"

-- | @Selector@ for @availableGroupKeys@
availableGroupKeysSelector :: Selector '[] (Id NSArray)
availableGroupKeysSelector = mkSelector "availableGroupKeys"

