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
  , initSelector
  , newSelector
  , initWithAsset_timebaseSelector
  , createSampleBufferForRequest_errorSelector
  , createSampleBufferForRequestSelector
  , makeBatchSelector
  , createSampleBufferForRequest_addingToBatch_errorSelector
  , notifyOfDataReadyForSampleBuffer_completionHandlerSelector


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
init_ :: IsAVSampleBufferGenerator avSampleBufferGenerator => avSampleBufferGenerator -> IO (Id AVSampleBufferGenerator)
init_ avSampleBufferGenerator  =
  sendMsg avSampleBufferGenerator (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVSampleBufferGenerator)
new  =
  do
    cls' <- getRequiredClass "AVSampleBufferGenerator"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
initWithAsset_timebase avSampleBufferGenerator  asset timebase =
withObjCPtr asset $ \raw_asset ->
    sendMsg avSampleBufferGenerator (mkSelector "initWithAsset:timebase:") (retPtr retVoid) [argPtr (castPtr raw_asset :: Ptr ()), argPtr timebase] >>= ownedObject . castPtr

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
createSampleBufferForRequest_error avSampleBufferGenerator  request outError =
withObjCPtr request $ \raw_request ->
  withObjCPtr outError $ \raw_outError ->
      fmap castPtr $ sendMsg avSampleBufferGenerator (mkSelector "createSampleBufferForRequest:error:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

-- | @- createSampleBufferForRequest:@
createSampleBufferForRequest :: (IsAVSampleBufferGenerator avSampleBufferGenerator, IsAVSampleBufferRequest request) => avSampleBufferGenerator -> request -> IO (Ptr ())
createSampleBufferForRequest avSampleBufferGenerator  request =
withObjCPtr request $ \raw_request ->
    fmap castPtr $ sendMsg avSampleBufferGenerator (mkSelector "createSampleBufferForRequest:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ())]

-- | makeBatch
--
-- Creates a batch to handle multiple sample buffers, allowing to asynchronously load sample data and optimize I/O when possible.
--
-- Returns: An instance of an AVSampleBufferGeneratorBatch that can be used in calls to createSampleBufferForRequest:addingToBatch:error: of the same AVSampleBufferGenerator instance.
--
-- ObjC selector: @- makeBatch@
makeBatch :: IsAVSampleBufferGenerator avSampleBufferGenerator => avSampleBufferGenerator -> IO (Id AVSampleBufferGeneratorBatch)
makeBatch avSampleBufferGenerator  =
  sendMsg avSampleBufferGenerator (mkSelector "makeBatch") (retPtr retVoid) [] >>= retainedObject . castPtr

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
createSampleBufferForRequest_addingToBatch_error avSampleBufferGenerator  request batch outError =
withObjCPtr request $ \raw_request ->
  withObjCPtr batch $ \raw_batch ->
    withObjCPtr outError $ \raw_outError ->
        fmap castPtr $ sendMsg avSampleBufferGenerator (mkSelector "createSampleBufferForRequest:addingToBatch:error:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_batch :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())]

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
    sendClassMsg cls' (mkSelector "notifyOfDataReadyForSampleBuffer:completionHandler:") retVoid [argPtr sbuf, argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithAsset:timebase:@
initWithAsset_timebaseSelector :: Selector
initWithAsset_timebaseSelector = mkSelector "initWithAsset:timebase:"

-- | @Selector@ for @createSampleBufferForRequest:error:@
createSampleBufferForRequest_errorSelector :: Selector
createSampleBufferForRequest_errorSelector = mkSelector "createSampleBufferForRequest:error:"

-- | @Selector@ for @createSampleBufferForRequest:@
createSampleBufferForRequestSelector :: Selector
createSampleBufferForRequestSelector = mkSelector "createSampleBufferForRequest:"

-- | @Selector@ for @makeBatch@
makeBatchSelector :: Selector
makeBatchSelector = mkSelector "makeBatch"

-- | @Selector@ for @createSampleBufferForRequest:addingToBatch:error:@
createSampleBufferForRequest_addingToBatch_errorSelector :: Selector
createSampleBufferForRequest_addingToBatch_errorSelector = mkSelector "createSampleBufferForRequest:addingToBatch:error:"

-- | @Selector@ for @notifyOfDataReadyForSampleBuffer:completionHandler:@
notifyOfDataReadyForSampleBuffer_completionHandlerSelector :: Selector
notifyOfDataReadyForSampleBuffer_completionHandlerSelector = mkSelector "notifyOfDataReadyForSampleBuffer:completionHandler:"

