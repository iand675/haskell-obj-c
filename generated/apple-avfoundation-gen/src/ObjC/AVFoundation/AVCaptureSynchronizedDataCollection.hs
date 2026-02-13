{-# LANGUAGE DataKinds #-}
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
  , countSelector
  , initSelector
  , newSelector
  , objectForKeyedSubscriptSelector
  , synchronizedDataForCaptureOutputSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptureSynchronizedDataCollection avCaptureSynchronizedDataCollection => avCaptureSynchronizedDataCollection -> IO (Id AVCaptureSynchronizedDataCollection)
init_ avCaptureSynchronizedDataCollection =
  sendOwnedMessage avCaptureSynchronizedDataCollection initSelector

-- | @+ new@
new :: IO (Id AVCaptureSynchronizedDataCollection)
new  =
  do
    cls' <- getRequiredClass "AVCaptureSynchronizedDataCollection"
    sendOwnedClassMessage cls' newSelector

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
synchronizedDataForCaptureOutput avCaptureSynchronizedDataCollection captureOutput =
  sendMessage avCaptureSynchronizedDataCollection synchronizedDataForCaptureOutputSelector (toAVCaptureOutput captureOutput)

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
objectForKeyedSubscript avCaptureSynchronizedDataCollection key =
  sendMessage avCaptureSynchronizedDataCollection objectForKeyedSubscriptSelector (toAVCaptureOutput key)

-- | count
--
-- The number of items in the collection.
--
-- Returns the number of data output / synchronized data pairs present in the collection.
--
-- ObjC selector: @- count@
count :: IsAVCaptureSynchronizedDataCollection avCaptureSynchronizedDataCollection => avCaptureSynchronizedDataCollection -> IO CULong
count avCaptureSynchronizedDataCollection =
  sendMessage avCaptureSynchronizedDataCollection countSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptureSynchronizedDataCollection)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptureSynchronizedDataCollection)
newSelector = mkSelector "new"

-- | @Selector@ for @synchronizedDataForCaptureOutput:@
synchronizedDataForCaptureOutputSelector :: Selector '[Id AVCaptureOutput] (Id AVCaptureSynchronizedData)
synchronizedDataForCaptureOutputSelector = mkSelector "synchronizedDataForCaptureOutput:"

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector '[Id AVCaptureOutput] (Id AVCaptureSynchronizedData)
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @count@
countSelector :: Selector '[] CULong
countSelector = mkSelector "count"

