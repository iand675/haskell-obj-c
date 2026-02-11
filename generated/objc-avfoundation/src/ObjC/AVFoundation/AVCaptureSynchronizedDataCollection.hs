{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureSynchronizedDataCollection
--
-- A collection of AVCaptureSynchronizedData objects.
--
-- AVCaptureDataOutputSynchronizer's -dataOutputSynchronizer:didOutputSynchronizedDataCollection: delegate method delivers a collection of AVCaptureSynchronizedData objects which can be iterated by AVCaptureOutput. AVCaptureSynchronizedDataCollection supports object subscripting and fast enumeration of the data outputs as keys.
--
-- Generated bindings for @AVCaptureSynchronizedDataCollection@.
module ObjC.AVFoundation.AVCaptureSynchronizedDataCollection
  ( AVCaptureSynchronizedDataCollection
  , IsAVCaptureSynchronizedDataCollection(..)
  , init_
  , new
  , synchronizedDataForCaptureOutput
  , objectForKeyedSubscript
  , count
  , initSelector
  , newSelector
  , synchronizedDataForCaptureOutputSelector
  , objectForKeyedSubscriptSelector
  , countSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureSynchronizedDataCollection avCaptureSynchronizedDataCollection => avCaptureSynchronizedDataCollection -> IO (Id AVCaptureSynchronizedDataCollection)
init_ avCaptureSynchronizedDataCollection  =
  sendMsg avCaptureSynchronizedDataCollection (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptureSynchronizedDataCollection)
new  =
  do
    cls' <- getRequiredClass "AVCaptureSynchronizedDataCollection"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | synchronizedDataForCaptureOutput:
--
-- Provides the synchronized data object for a given capture output.
--
-- @captureOutput@ — The data output whose synchronized data you'd like to inspect.
--
-- Returns: The synchronized data object associated with the provided output, or nil, if there is none.
--
-- ObjC selector: @- synchronizedDataForCaptureOutput:@
synchronizedDataForCaptureOutput :: (IsAVCaptureSynchronizedDataCollection avCaptureSynchronizedDataCollection, IsAVCaptureOutput captureOutput) => avCaptureSynchronizedDataCollection -> captureOutput -> IO (Id AVCaptureSynchronizedData)
synchronizedDataForCaptureOutput avCaptureSynchronizedDataCollection  captureOutput =
withObjCPtr captureOutput $ \raw_captureOutput ->
    sendMsg avCaptureSynchronizedDataCollection (mkSelector "synchronizedDataForCaptureOutput:") (retPtr retVoid) [argPtr (castPtr raw_captureOutput :: Ptr ())] >>= retainedObject . castPtr

-- | objectForKeyedSubscript:
--
-- Method that provides support for object subscripting.
--
-- @key@ — The data output whose synchronized data you'd like to inspect.
--
-- Returns: The synchronized data object associated with the provided output, or nil, if there is none.
--
-- AVCaptureSynchronizedDataCollection supports object subscripting. If you'd like to find the synchronized data for a given data output, simply:        AVCaptureSynchronizedData *synchronizedData = synchronizedDataCollection[dataOutput];
--
-- ObjC selector: @- objectForKeyedSubscript:@
objectForKeyedSubscript :: (IsAVCaptureSynchronizedDataCollection avCaptureSynchronizedDataCollection, IsAVCaptureOutput key) => avCaptureSynchronizedDataCollection -> key -> IO (Id AVCaptureSynchronizedData)
objectForKeyedSubscript avCaptureSynchronizedDataCollection  key =
withObjCPtr key $ \raw_key ->
    sendMsg avCaptureSynchronizedDataCollection (mkSelector "objectForKeyedSubscript:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | count
--
-- The number of items in the collection.
--
-- Returns the number of data output / synchronized data pairs present in the collection.
--
-- ObjC selector: @- count@
count :: IsAVCaptureSynchronizedDataCollection avCaptureSynchronizedDataCollection => avCaptureSynchronizedDataCollection -> IO CULong
count avCaptureSynchronizedDataCollection  =
  sendMsg avCaptureSynchronizedDataCollection (mkSelector "count") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @synchronizedDataForCaptureOutput:@
synchronizedDataForCaptureOutputSelector :: Selector
synchronizedDataForCaptureOutputSelector = mkSelector "synchronizedDataForCaptureOutput:"

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @count@
countSelector :: Selector
countSelector = mkSelector "count"

