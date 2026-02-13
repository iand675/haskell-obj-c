{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNRequest
--
-- VNRequest objects describe the operation to be performed as well as act as the recipient of the operation's resultant observations.
--
-- VNRequest objects are instantiated in a pre-configured nominal state. Prior to sending a VNRequest to a request handler to perform a desired operation, the default configuration can be changed by modifying the values of VNRequest properties. The VNRequest class itself acts as a base class and is not meant to be directly instantiated.
--
-- Generated bindings for @VNRequest@.
module ObjC.Vision.VNRequest
  ( VNRequest
  , IsVNRequest(..)
  , init_
  , initWithCompletionHandler
  , cancel
  , supportedComputeStageDevicesAndReturnError
  , computeDeviceForComputeStage
  , setComputeDevice_forComputeStage
  , preferBackgroundProcessing
  , setPreferBackgroundProcessing
  , usesCPUOnly
  , setUsesCPUOnly
  , results
  , completionHandler
  , revision
  , setRevision
  , supportedRevisions
  , defaultRevision
  , currentRevision
  , cancelSelector
  , completionHandlerSelector
  , computeDeviceForComputeStageSelector
  , currentRevisionSelector
  , defaultRevisionSelector
  , initSelector
  , initWithCompletionHandlerSelector
  , preferBackgroundProcessingSelector
  , resultsSelector
  , revisionSelector
  , setComputeDevice_forComputeStageSelector
  , setPreferBackgroundProcessingSelector
  , setRevisionSelector
  , setUsesCPUOnlySelector
  , supportedComputeStageDevicesAndReturnErrorSelector
  , supportedRevisionsSelector
  , usesCPUOnlySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a new VNRequest with no completion handler.
--
-- ObjC selector: @- init@
init_ :: IsVNRequest vnRequest => vnRequest -> IO (Id VNRequest)
init_ vnRequest =
  sendOwnedMessage vnRequest initSelector

-- | Creates a new VNRequest with an optional completion handler.
--
-- @completionHandler@ — The block to be invoked after the request has completed its processing. The completion handler gets executed on the same dispatch queue as the request being executed.
--
-- ObjC selector: @- initWithCompletionHandler:@
initWithCompletionHandler :: IsVNRequest vnRequest => vnRequest -> Ptr () -> IO (Id VNRequest)
initWithCompletionHandler vnRequest completionHandler =
  sendOwnedMessage vnRequest initWithCompletionHandlerSelector completionHandler

-- | Tries to abort the request as soon as possible. Results will be nil. The completionHandler (if present) will be called with an error of VNErrorRequestCancelled.
--
-- ObjC selector: @- cancel@
cancel :: IsVNRequest vnRequest => vnRequest -> IO ()
cancel vnRequest =
  sendMessage vnRequest cancelSelector

-- | Obtain the collection of compute device per stage that are supported by the request.
--
-- This method's result is based on the current state of configuration of the target request at the time of the call.
--
-- @error@ — The address of a variable that will be populated with the error that describes the failure.  If the caller does not require this information, NULL can be passed.
--
-- Returns: A dictionary of per-stage supported compute devices, or @nil@ if an error occurs.
--
-- ObjC selector: @- supportedComputeStageDevicesAndReturnError:@
supportedComputeStageDevicesAndReturnError :: (IsVNRequest vnRequest, IsNSError error_) => vnRequest -> error_ -> IO (Id NSDictionary)
supportedComputeStageDevicesAndReturnError vnRequest error_ =
  sendMessage vnRequest supportedComputeStageDevicesAndReturnErrorSelector (toNSError error_)

-- | Determine what the currently configured compute device is for a specific compute stage.
--
-- @computeStage@ — The compute stage to be introspected.
--
-- Returns: The currently assigned compute device, or @nil@ if there is no explicit assignment.
--
-- ObjC selector: @- computeDeviceForComputeStage:@
computeDeviceForComputeStage :: (IsVNRequest vnRequest, IsNSString computeStage) => vnRequest -> computeStage -> IO RawId
computeDeviceForComputeStage vnRequest computeStage =
  sendMessage vnRequest computeDeviceForComputeStageSelector (toNSString computeStage)

-- | Assign a specific compute device for a compute stage.
--
-- It is important to note that any compute device can be configured for a given compute stage.  Only when the request is performed is the validity of the (compute device / compute stage) assignments checked.  Valid compute devices for a request's compute stages can be obtained via @-supportedComputeStageDevicesAndReturnError:@.
--
-- @computeDevice@ — The compute device to assign to the compute stage.  Passing nil for this parameter will remove any explicit compute device assignment, allowing Vision to select which device to use.
--
-- @computeStage@ — The compute stage being configured.
--
-- ObjC selector: @- setComputeDevice:forComputeStage:@
setComputeDevice_forComputeStage :: (IsVNRequest vnRequest, IsNSString computeStage) => vnRequest -> RawId -> computeStage -> IO ()
setComputeDevice_forComputeStage vnRequest computeDevice computeStage =
  sendMessage vnRequest setComputeDevice_forComputeStageSelector computeDevice (toNSString computeStage)

-- | A hint used to minimize the resource burden of the request. Memory footprint, processing footprint and/or CPU/GPU contention will be reduced (depending on the request), at the potential cost of longer execution time. This can help, for example, with ensuring UI updates and rendering are not getting blocked by Vision processing.
--
-- ObjC selector: @- preferBackgroundProcessing@
preferBackgroundProcessing :: IsVNRequest vnRequest => vnRequest -> IO Bool
preferBackgroundProcessing vnRequest =
  sendMessage vnRequest preferBackgroundProcessingSelector

-- | A hint used to minimize the resource burden of the request. Memory footprint, processing footprint and/or CPU/GPU contention will be reduced (depending on the request), at the potential cost of longer execution time. This can help, for example, with ensuring UI updates and rendering are not getting blocked by Vision processing.
--
-- ObjC selector: @- setPreferBackgroundProcessing:@
setPreferBackgroundProcessing :: IsVNRequest vnRequest => vnRequest -> Bool -> IO ()
setPreferBackgroundProcessing vnRequest value =
  sendMessage vnRequest setPreferBackgroundProcessingSelector value

-- | This property, if set to YES, signifies that the request should be performed exclusively on the CPU and not on the GPU. The default value is NO, which signifies that the request is free to leverage the GPU to accelerate any work the request may require.
--
-- ObjC selector: @- usesCPUOnly@
usesCPUOnly :: IsVNRequest vnRequest => vnRequest -> IO Bool
usesCPUOnly vnRequest =
  sendMessage vnRequest usesCPUOnlySelector

-- | This property, if set to YES, signifies that the request should be performed exclusively on the CPU and not on the GPU. The default value is NO, which signifies that the request is free to leverage the GPU to accelerate any work the request may require.
--
-- ObjC selector: @- setUsesCPUOnly:@
setUsesCPUOnly :: IsVNRequest vnRequest => vnRequest -> Bool -> IO ()
setUsesCPUOnly vnRequest value =
  sendMessage vnRequest setUsesCPUOnlySelector value

-- | results
--
-- The collection of VNObservations generated by the processing of the request.
--
-- The only valid time to access this property is after the request has been processed by a request handler.  If the request failed, this property will be nil; otherwise, it will be an array of zero or more VNObservation subclasses specific to the VNRequest subclass.
--
-- ObjC selector: @- results@
results :: IsVNRequest vnRequest => vnRequest -> IO (Id NSArray)
results vnRequest =
  sendMessage vnRequest resultsSelector

-- | completionHandler
--
-- The completion handler block that will be invoked after the request has completed processing.
--
-- ObjC selector: @- completionHandler@
completionHandler :: IsVNRequest vnRequest => vnRequest -> IO (Ptr ())
completionHandler vnRequest =
  sendMessage vnRequest completionHandlerSelector

-- | The specific algorithm or implementation revision that is to be used to perform the request.
--
-- ObjC selector: @- revision@
revision :: IsVNRequest vnRequest => vnRequest -> IO CULong
revision vnRequest =
  sendMessage vnRequest revisionSelector

-- | The specific algorithm or implementation revision that is to be used to perform the request.
--
-- ObjC selector: @- setRevision:@
setRevision :: IsVNRequest vnRequest => vnRequest -> CULong -> IO ()
setRevision vnRequest value =
  sendMessage vnRequest setRevisionSelector value

-- | Provides the collection of currently-supported algorithm or implementation versions for the class of request.
--
-- This method allows clients to introspect at runtime what capabilities are available for each class of VNRequest in the Vision framework.
--
-- ObjC selector: @+ supportedRevisions@
supportedRevisions :: IO (Id NSIndexSet)
supportedRevisions  =
  do
    cls' <- getRequiredClass "VNRequest"
    sendClassMessage cls' supportedRevisionsSelector

-- | Provides the revision of the request that was latest for the particular SDK that was linked with the client application.
--
-- ObjC selector: @+ defaultRevision@
defaultRevision :: IO CULong
defaultRevision  =
  do
    cls' <- getRequiredClass "VNRequest"
    sendClassMessage cls' defaultRevisionSelector

-- | Provides the current revision supported by the request.
--
-- ObjC selector: @+ currentRevision@
currentRevision :: IO CULong
currentRevision  =
  do
    cls' <- getRequiredClass "VNRequest"
    sendClassMessage cls' currentRevisionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCompletionHandler:@
initWithCompletionHandlerSelector :: Selector '[Ptr ()] (Id VNRequest)
initWithCompletionHandlerSelector = mkSelector "initWithCompletionHandler:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @supportedComputeStageDevicesAndReturnError:@
supportedComputeStageDevicesAndReturnErrorSelector :: Selector '[Id NSError] (Id NSDictionary)
supportedComputeStageDevicesAndReturnErrorSelector = mkSelector "supportedComputeStageDevicesAndReturnError:"

-- | @Selector@ for @computeDeviceForComputeStage:@
computeDeviceForComputeStageSelector :: Selector '[Id NSString] RawId
computeDeviceForComputeStageSelector = mkSelector "computeDeviceForComputeStage:"

-- | @Selector@ for @setComputeDevice:forComputeStage:@
setComputeDevice_forComputeStageSelector :: Selector '[RawId, Id NSString] ()
setComputeDevice_forComputeStageSelector = mkSelector "setComputeDevice:forComputeStage:"

-- | @Selector@ for @preferBackgroundProcessing@
preferBackgroundProcessingSelector :: Selector '[] Bool
preferBackgroundProcessingSelector = mkSelector "preferBackgroundProcessing"

-- | @Selector@ for @setPreferBackgroundProcessing:@
setPreferBackgroundProcessingSelector :: Selector '[Bool] ()
setPreferBackgroundProcessingSelector = mkSelector "setPreferBackgroundProcessing:"

-- | @Selector@ for @usesCPUOnly@
usesCPUOnlySelector :: Selector '[] Bool
usesCPUOnlySelector = mkSelector "usesCPUOnly"

-- | @Selector@ for @setUsesCPUOnly:@
setUsesCPUOnlySelector :: Selector '[Bool] ()
setUsesCPUOnlySelector = mkSelector "setUsesCPUOnly:"

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

-- | @Selector@ for @completionHandler@
completionHandlerSelector :: Selector '[] (Ptr ())
completionHandlerSelector = mkSelector "completionHandler"

-- | @Selector@ for @revision@
revisionSelector :: Selector '[] CULong
revisionSelector = mkSelector "revision"

-- | @Selector@ for @setRevision:@
setRevisionSelector :: Selector '[CULong] ()
setRevisionSelector = mkSelector "setRevision:"

-- | @Selector@ for @supportedRevisions@
supportedRevisionsSelector :: Selector '[] (Id NSIndexSet)
supportedRevisionsSelector = mkSelector "supportedRevisions"

-- | @Selector@ for @defaultRevision@
defaultRevisionSelector :: Selector '[] CULong
defaultRevisionSelector = mkSelector "defaultRevision"

-- | @Selector@ for @currentRevision@
currentRevisionSelector :: Selector '[] CULong
currentRevisionSelector = mkSelector "currentRevision"

