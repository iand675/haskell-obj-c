{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNRecognizedPointsObservation
--
-- VNObservation
--
-- VNRecognizedPointsObservation is a request result detailing points in an image.
--
-- Generated bindings for @VNRecognizedPointsObservation@.
module ObjC.Vision.VNRecognizedPointsObservation
  ( VNRecognizedPointsObservation
  , IsVNRecognizedPointsObservation(..)
  , new
  , init_
  , recognizedPointForKey_error
  , recognizedPointsForGroupKey_error
  , keypointsMultiArrayAndReturnError
  , availableKeys
  , availableGroupKeys
  , availableGroupKeysSelector
  , availableKeysSelector
  , initSelector
  , keypointsMultiArrayAndReturnErrorSelector
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
import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VNRecognizedPointsObservation)
new  =
  do
    cls' <- getRequiredClass "VNRecognizedPointsObservation"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVNRecognizedPointsObservation vnRecognizedPointsObservation => vnRecognizedPointsObservation -> IO (Id VNRecognizedPointsObservation)
init_ vnRecognizedPointsObservation =
  sendOwnedMessage vnRecognizedPointsObservation initSelector

-- | Obtains a specific normalized recognized point.
--
-- @pointKey@ — The key specifying the desired recognized point.
--
-- @error@ — The address of a variable that will be populated with the error that describes the failure.  If the caller does not require this information, NULL can be passed.
--
-- Returns: the recognized point, or nil if the specific point is not defined.
--
-- ObjC selector: @- recognizedPointForKey:error:@
recognizedPointForKey_error :: (IsVNRecognizedPointsObservation vnRecognizedPointsObservation, IsNSString pointKey, IsNSError error_) => vnRecognizedPointsObservation -> pointKey -> error_ -> IO (Id VNRecognizedPoint)
recognizedPointForKey_error vnRecognizedPointsObservation pointKey error_ =
  sendMessage vnRecognizedPointsObservation recognizedPointForKey_errorSelector (toNSString pointKey) (toNSError error_)

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
recognizedPointsForGroupKey_error :: (IsVNRecognizedPointsObservation vnRecognizedPointsObservation, IsNSString groupKey, IsNSError error_) => vnRecognizedPointsObservation -> groupKey -> error_ -> IO (Id NSDictionary)
recognizedPointsForGroupKey_error vnRecognizedPointsObservation groupKey error_ =
  sendMessage vnRecognizedPointsObservation recognizedPointsForGroupKey_errorSelector (toNSString groupKey) (toNSError error_)

-- | Returns the recognized points packaged into an MLMultiArray.
--
-- The MLMultiArray will contain the raw data output of (x coordinate, y coordinate, confidence) for specific points in the format expected by CreateML action recognition models. The datatype of the elements in the array is double and the dimensions are [1, 3, # of possible points].  If an expected point key is not available in the obeservation, that entry in the MLMultiArray will be populated with 0s.
--
-- @error@ — The address of a variable that will be populated with the error that describes the failure.  If the caller does not require this information, NULL can be passed.
--
-- Returns: the MLMultiArray representation of the points, or nil if an error was encountered.
--
-- ObjC selector: @- keypointsMultiArrayAndReturnError:@
keypointsMultiArrayAndReturnError :: (IsVNRecognizedPointsObservation vnRecognizedPointsObservation, IsNSError error_) => vnRecognizedPointsObservation -> error_ -> IO (Id MLMultiArray)
keypointsMultiArrayAndReturnError vnRecognizedPointsObservation error_ =
  sendMessage vnRecognizedPointsObservation keypointsMultiArrayAndReturnErrorSelector (toNSError error_)

-- | Returns all of the point group keys available in the observation.
--
-- ObjC selector: @- availableKeys@
availableKeys :: IsVNRecognizedPointsObservation vnRecognizedPointsObservation => vnRecognizedPointsObservation -> IO (Id NSArray)
availableKeys vnRecognizedPointsObservation =
  sendMessage vnRecognizedPointsObservation availableKeysSelector

-- | The availableGroupKeys property returns all of the point group labels usable with the observation.
--
-- ObjC selector: @- availableGroupKeys@
availableGroupKeys :: IsVNRecognizedPointsObservation vnRecognizedPointsObservation => vnRecognizedPointsObservation -> IO (Id NSArray)
availableGroupKeys vnRecognizedPointsObservation =
  sendMessage vnRecognizedPointsObservation availableGroupKeysSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VNRecognizedPointsObservation)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNRecognizedPointsObservation)
initSelector = mkSelector "init"

-- | @Selector@ for @recognizedPointForKey:error:@
recognizedPointForKey_errorSelector :: Selector '[Id NSString, Id NSError] (Id VNRecognizedPoint)
recognizedPointForKey_errorSelector = mkSelector "recognizedPointForKey:error:"

-- | @Selector@ for @recognizedPointsForGroupKey:error:@
recognizedPointsForGroupKey_errorSelector :: Selector '[Id NSString, Id NSError] (Id NSDictionary)
recognizedPointsForGroupKey_errorSelector = mkSelector "recognizedPointsForGroupKey:error:"

-- | @Selector@ for @keypointsMultiArrayAndReturnError:@
keypointsMultiArrayAndReturnErrorSelector :: Selector '[Id NSError] (Id MLMultiArray)
keypointsMultiArrayAndReturnErrorSelector = mkSelector "keypointsMultiArrayAndReturnError:"

-- | @Selector@ for @availableKeys@
availableKeysSelector :: Selector '[] (Id NSArray)
availableKeysSelector = mkSelector "availableKeys"

-- | @Selector@ for @availableGroupKeys@
availableGroupKeysSelector :: Selector '[] (Id NSArray)
availableGroupKeysSelector = mkSelector "availableGroupKeys"

