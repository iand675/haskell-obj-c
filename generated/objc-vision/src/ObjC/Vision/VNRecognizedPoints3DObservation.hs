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
  , newSelector
  , initSelector
  , recognizedPointForKey_errorSelector
  , recognizedPointsForGroupKey_errorSelector
  , availableKeysSelector
  , availableGroupKeysSelector


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

-- | @+ new@
new :: IO (Id VNRecognizedPoints3DObservation)
new  =
  do
    cls' <- getRequiredClass "VNRecognizedPoints3DObservation"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVNRecognizedPoints3DObservation vnRecognizedPoints3DObservation => vnRecognizedPoints3DObservation -> IO (Id VNRecognizedPoints3DObservation)
init_ vnRecognizedPoints3DObservation  =
  sendMsg vnRecognizedPoints3DObservation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
recognizedPointForKey_error vnRecognizedPoints3DObservation  pointKey error_ =
withObjCPtr pointKey $ \raw_pointKey ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg vnRecognizedPoints3DObservation (mkSelector "recognizedPointForKey:error:") (retPtr retVoid) [argPtr (castPtr raw_pointKey :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

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
recognizedPointsForGroupKey_error vnRecognizedPoints3DObservation  groupKey error_ =
withObjCPtr groupKey $ \raw_groupKey ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg vnRecognizedPoints3DObservation (mkSelector "recognizedPointsForGroupKey:error:") (retPtr retVoid) [argPtr (castPtr raw_groupKey :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | Returns all of the point group keys available in the observation.
--
-- ObjC selector: @- availableKeys@
availableKeys :: IsVNRecognizedPoints3DObservation vnRecognizedPoints3DObservation => vnRecognizedPoints3DObservation -> IO (Id NSArray)
availableKeys vnRecognizedPoints3DObservation  =
  sendMsg vnRecognizedPoints3DObservation (mkSelector "availableKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The availableGroupKeys property returns all of the point group labels usable with the observation.
--
-- ObjC selector: @- availableGroupKeys@
availableGroupKeys :: IsVNRecognizedPoints3DObservation vnRecognizedPoints3DObservation => vnRecognizedPoints3DObservation -> IO (Id NSArray)
availableGroupKeys vnRecognizedPoints3DObservation  =
  sendMsg vnRecognizedPoints3DObservation (mkSelector "availableGroupKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @recognizedPointForKey:error:@
recognizedPointForKey_errorSelector :: Selector
recognizedPointForKey_errorSelector = mkSelector "recognizedPointForKey:error:"

-- | @Selector@ for @recognizedPointsForGroupKey:error:@
recognizedPointsForGroupKey_errorSelector :: Selector
recognizedPointsForGroupKey_errorSelector = mkSelector "recognizedPointsForGroupKey:error:"

-- | @Selector@ for @availableKeys@
availableKeysSelector :: Selector
availableKeysSelector = mkSelector "availableKeys"

-- | @Selector@ for @availableGroupKeys@
availableGroupKeysSelector :: Selector
availableGroupKeysSelector = mkSelector "availableGroupKeys"

