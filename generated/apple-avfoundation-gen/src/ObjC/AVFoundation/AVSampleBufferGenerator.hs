{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVSampleBufferGenerator@.
module ObjC.AVFoundation.AVSampleBufferGenerator
  ( AVSampleBufferGenerator
  , IsAVSampleBufferGenerator(..)
  , init_
  , new
  , initWithAsset_timebase
  , createSampleBufferForRequest_error
  , createSampleBufferForRequest
  , makeBatch
  , createSampleBufferForRequest_addingToBatch_error
  , notifyOfDataReadyForSampleBuffer_completionHandler
  , createSampleBufferForRequestSelector
  , createSampleBufferForRequest_addingToBatch_errorSelector
  , createSampleBufferForRequest_errorSelector
  , initSelector
  , initWithAsset_timebaseSelector
  , makeBatchSelector
  , newSelector
  , notifyOfDataReadyForSampleBuffer_completionHandlerSelector


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
init_ :: IsAVSampleBufferGenerator avSampleBufferGenerator => avSampleBufferGenerator -> IO (Id AVSampleBufferGenerator)
init_ avSampleBufferGenerator =
  sendOwnedMessage avSampleBufferGenerator initSelector

-- | @+ new@
new :: IO (Id AVSampleBufferGenerator)
new  =
  do
    cls' <- getRequiredClass "AVSampleBufferGenerator"
    sendOwnedClassMessage cls' newSelector

-- | initWithAsset: timebase:
--
-- Creates an instance of AVSampleBufferGenerator to generate sample buffers from the specified asset.
--
-- @asset@ — The asset from which sample buffers will be created.
--
-- @timebase@ — The generator timebase, which governs when sample data for sample buffers is loaded. If NULL, sample data is loaded synchronously.
--
-- Returns: An instance of AVSampleBufferGenerator.
--
-- If the specified asset is an HTTP Live Streaming asset, the generator cannot create sample buffers.
--
-- ObjC selector: @- initWithAsset:timebase:@
initWithAsset_timebase :: (IsAVSampleBufferGenerator avSampleBufferGenerator, IsAVAsset asset) => avSampleBufferGenerator -> asset -> Ptr () -> IO (Id AVSampleBufferGenerator)
initWithAsset_timebase avSampleBufferGenerator asset timebase =
  sendOwnedMessage avSampleBufferGenerator initWithAsset_timebaseSelector (toAVAsset asset) timebase

-- | createSampleBufferForRequest: error:
--
-- Creates a sample buffer and if requested, attempts to load its data asynchronously. Attempt may fail based on generator configuration or file format.				See [AVSampleBufferGenerator notifyOfDataReadyForSampleBuffer: completionHandler:] to get notified when the sample buffer data is available.
--
-- @request@ — An instance of AVSampleBufferRequest representing the CMSampleBuffer creation request.
--
-- @outError@ — A pointer to an NSError object that will be populated with failure information, if sample buffer creation fails.
--
-- Returns: A CMSampleBuffer object referencing the output sample buffer.
--
-- If the AVSampleBufferGenerator was created with a NULL timebase, any associated AVSampleBufferRequest will default to using AVSampleBufferRequestModeImmediate.
--
-- ObjC selector: @- createSampleBufferForRequest:error:@
createSampleBufferForRequest_error :: (IsAVSampleBufferGenerator avSampleBufferGenerator, IsAVSampleBufferRequest request, IsNSError outError) => avSampleBufferGenerator -> request -> outError -> IO (Ptr ())
createSampleBufferForRequest_error avSampleBufferGenerator request outError =
  sendMessage avSampleBufferGenerator createSampleBufferForRequest_errorSelector (toAVSampleBufferRequest request) (toNSError outError)

-- | @- createSampleBufferForRequest:@
createSampleBufferForRequest :: (IsAVSampleBufferGenerator avSampleBufferGenerator, IsAVSampleBufferRequest request) => avSampleBufferGenerator -> request -> IO (Ptr ())
createSampleBufferForRequest avSampleBufferGenerator request =
  sendMessage avSampleBufferGenerator createSampleBufferForRequestSelector (toAVSampleBufferRequest request)

-- | makeBatch
--
-- Creates a batch to handle multiple sample buffers, allowing to asynchronously load sample data and optimize I/O when possible.
--
-- Returns: An instance of an AVSampleBufferGeneratorBatch that can be used in calls to createSampleBufferForRequest:addingToBatch:error: of the same AVSampleBufferGenerator instance.
--
-- ObjC selector: @- makeBatch@
makeBatch :: IsAVSampleBufferGenerator avSampleBufferGenerator => avSampleBufferGenerator -> IO (Id AVSampleBufferGeneratorBatch)
makeBatch avSampleBufferGenerator =
  sendMessage avSampleBufferGenerator makeBatchSelector

-- | createSampleBufferForRequest: addingToBatch: error:
--
-- Creates a sample buffer and attempts to defer I/O for its data. Attempt may fail based on generator configuration or file format.				The [AVSampleBufferGeneratorBatch makeDataReadyWithCompletionHandler:] should be called once to commence I/O and load sample data for all CMSampleBuffers within a batch.				Any subsequent calls to createSampleBufferForRequest:addingToBatch:error: will throw an exception.
--
-- @request@ — An instance of AVSampleBufferRequest representing the CMSampleBuffer creation request
--
-- @batch@ — An instance of AVSampleBufferGeneratorBatch to contain the output sample buffer. If nil, an exception is thrown.				Must be created by calling makeBatch on the same instance of AVSampleBufferGenerator. An exception will be thrown otherwise.
--
-- @outError@ — A pointer to an NSError object that will be populated with failure information, if sample buffer creation fails.
--
-- Returns: A CMSampleBuffer object referencing the output sample buffer. The generator may defer I/O to fetch sample data depending on the source of the sample data and				the generator's timebase.
--
-- ObjC selector: @- createSampleBufferForRequest:addingToBatch:error:@
createSampleBufferForRequest_addingToBatch_error :: (IsAVSampleBufferGenerator avSampleBufferGenerator, IsAVSampleBufferRequest request, IsAVSampleBufferGeneratorBatch batch, IsNSError outError) => avSampleBufferGenerator -> request -> batch -> outError -> IO (Ptr ())
createSampleBufferForRequest_addingToBatch_error avSampleBufferGenerator request batch outError =
  sendMessage avSampleBufferGenerator createSampleBufferForRequest_addingToBatch_errorSelector (toAVSampleBufferRequest request) (toAVSampleBufferGeneratorBatch batch) (toNSError outError)

-- | notifyOfDataReadyForSampleBuffer: completionHandler:
--
-- Allows the client to get notified when the sample buffer data is ready, or as soon as an error has occured.
--
-- @completionHandler@ — The completionHandler will be called, when the sample buffer data is ready, or as soon as an error has occurred.
--
-- ObjC selector: @+ notifyOfDataReadyForSampleBuffer:completionHandler:@
notifyOfDataReadyForSampleBuffer_completionHandler :: Ptr () -> Ptr () -> IO ()
notifyOfDataReadyForSampleBuffer_completionHandler sbuf completionHandler =
  do
    cls' <- getRequiredClass "AVSampleBufferGenerator"
    sendClassMessage cls' notifyOfDataReadyForSampleBuffer_completionHandlerSelector sbuf completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVSampleBufferGenerator)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVSampleBufferGenerator)
newSelector = mkSelector "new"

-- | @Selector@ for @initWithAsset:timebase:@
initWithAsset_timebaseSelector :: Selector '[Id AVAsset, Ptr ()] (Id AVSampleBufferGenerator)
initWithAsset_timebaseSelector = mkSelector "initWithAsset:timebase:"

-- | @Selector@ for @createSampleBufferForRequest:error:@
createSampleBufferForRequest_errorSelector :: Selector '[Id AVSampleBufferRequest, Id NSError] (Ptr ())
createSampleBufferForRequest_errorSelector = mkSelector "createSampleBufferForRequest:error:"

-- | @Selector@ for @createSampleBufferForRequest:@
createSampleBufferForRequestSelector :: Selector '[Id AVSampleBufferRequest] (Ptr ())
createSampleBufferForRequestSelector = mkSelector "createSampleBufferForRequest:"

-- | @Selector@ for @makeBatch@
makeBatchSelector :: Selector '[] (Id AVSampleBufferGeneratorBatch)
makeBatchSelector = mkSelector "makeBatch"

-- | @Selector@ for @createSampleBufferForRequest:addingToBatch:error:@
createSampleBufferForRequest_addingToBatch_errorSelector :: Selector '[Id AVSampleBufferRequest, Id AVSampleBufferGeneratorBatch, Id NSError] (Ptr ())
createSampleBufferForRequest_addingToBatch_errorSelector = mkSelector "createSampleBufferForRequest:addingToBatch:error:"

-- | @Selector@ for @notifyOfDataReadyForSampleBuffer:completionHandler:@
notifyOfDataReadyForSampleBuffer_completionHandlerSelector :: Selector '[Ptr (), Ptr ()] ()
notifyOfDataReadyForSampleBuffer_completionHandlerSelector = mkSelector "notifyOfDataReadyForSampleBuffer:completionHandler:"

