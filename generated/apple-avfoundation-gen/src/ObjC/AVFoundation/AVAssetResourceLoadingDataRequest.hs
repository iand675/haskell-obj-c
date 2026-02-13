{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVAssetResourceLoadingDataRequest@.
module ObjC.AVFoundation.AVAssetResourceLoadingDataRequest
  ( AVAssetResourceLoadingDataRequest
  , IsAVAssetResourceLoadingDataRequest(..)
  , init_
  , new
  , respondWithData
  , requestedOffset
  , requestedLength
  , requestsAllDataToEndOfResource
  , currentOffset
  , currentOffsetSelector
  , initSelector
  , newSelector
  , requestedLengthSelector
  , requestedOffsetSelector
  , requestsAllDataToEndOfResourceSelector
  , respondWithDataSelector


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
init_ :: IsAVAssetResourceLoadingDataRequest avAssetResourceLoadingDataRequest => avAssetResourceLoadingDataRequest -> IO (Id AVAssetResourceLoadingDataRequest)
init_ avAssetResourceLoadingDataRequest =
  sendOwnedMessage avAssetResourceLoadingDataRequest initSelector

-- | @+ new@
new :: IO (Id AVAssetResourceLoadingDataRequest)
new  =
  do
    cls' <- getRequiredClass "AVAssetResourceLoadingDataRequest"
    sendOwnedClassMessage cls' newSelector

-- | respondWithData:
--
-- Provides data to the receiver.
--
-- @data@ — An instance of NSData containing some or all of the requested bytes.
--
-- May be invoked multiple times on the same instance of AVAssetResourceLoadingDataRequest to provide the full range of requested data incrementally. Upon each invocation, the value of currentOffset will be updated to accord with the amount of data provided.				The instance of NSData that you provide may be retained for use in parsing or other processing for an indefinite period of time after this method returns. For this reason, if you are providing an instance of NSMutableData, you should avoid mutating it further after sharing its contents. If you are managing your own memory pool for I/O and resource loading, consider using -[NSData initWithBytesNoCopy:length:deallocator:] in order to receive notification of the earliest opportunity for safe recycling of the underlying memory.
--
-- ObjC selector: @- respondWithData:@
respondWithData :: (IsAVAssetResourceLoadingDataRequest avAssetResourceLoadingDataRequest, IsNSData data_) => avAssetResourceLoadingDataRequest -> data_ -> IO ()
respondWithData avAssetResourceLoadingDataRequest data_ =
  sendMessage avAssetResourceLoadingDataRequest respondWithDataSelector (toNSData data_)

-- | requestedOffset
--
-- The position within the resource of the first byte requested.
--
-- ObjC selector: @- requestedOffset@
requestedOffset :: IsAVAssetResourceLoadingDataRequest avAssetResourceLoadingDataRequest => avAssetResourceLoadingDataRequest -> IO CLong
requestedOffset avAssetResourceLoadingDataRequest =
  sendMessage avAssetResourceLoadingDataRequest requestedOffsetSelector

-- | requestedLength
--
-- The length of the data requested.
--
-- Note that requestsAllDataToEndOfResource will be set to YES when the entire remaining length of the resource is being requested from requestedOffset to the end of the resource. This can occur even when the content length has not yet been reported by you via a prior finished loading request. 				When requestsAllDataToEndOfResource has a value of YES, you should disregard the value of requestedLength and incrementally provide as much data starting from the requestedOffset as the resource contains, until you have provided all of the available data successfully and invoked -finishLoading, until you have encountered a failure and invoked -finishLoadingWithError:, or until you have received -resourceLoader:didCancelLoadingRequest: for the AVAssetResourceLoadingRequest from which the AVAssetResourceLoadingDataRequest was obtained. 				When requestsAllDataToEndOfResource is YES and the content length has not yet been provided by you via a prior finished loading request, the value of requestedLength is set to NSIntegerMax. Starting in macOS 10.11 and iOS 9.0, in 32-bit applications requestedLength is also set to NSIntegerMax when all of the remaining resource data is being requested and the known length of the remaining data exceeds NSIntegerMax.
--
-- ObjC selector: @- requestedLength@
requestedLength :: IsAVAssetResourceLoadingDataRequest avAssetResourceLoadingDataRequest => avAssetResourceLoadingDataRequest -> IO CLong
requestedLength avAssetResourceLoadingDataRequest =
  sendMessage avAssetResourceLoadingDataRequest requestedLengthSelector

-- | requestsAllDataToEndOfResource
--
-- Specifies that the entire remaining length of the resource from requestedOffset to the end of the resource is being requested.
--
-- When requestsAllDataToEndOfResource has a value of YES, you should disregard the value of requestedLength and incrementally provide as much data starting from the requestedOffset as the resource contains, until you have provided all of the available data successfully and invoked -finishLoading, until you have encountered a failure and invoked -finishLoadingWithError:, or until you have received -resourceLoader:didCancelLoadingRequest: for the AVAssetResourceLoadingRequest from which the AVAssetResourceLoadingDataRequest was obtained.
--
-- ObjC selector: @- requestsAllDataToEndOfResource@
requestsAllDataToEndOfResource :: IsAVAssetResourceLoadingDataRequest avAssetResourceLoadingDataRequest => avAssetResourceLoadingDataRequest -> IO Bool
requestsAllDataToEndOfResource avAssetResourceLoadingDataRequest =
  sendMessage avAssetResourceLoadingDataRequest requestsAllDataToEndOfResourceSelector

-- | currentOffset
--
-- The position within the resource of the next byte within the resource following the bytes that have already been provided via prior invocations of -respondWithData.
--
-- ObjC selector: @- currentOffset@
currentOffset :: IsAVAssetResourceLoadingDataRequest avAssetResourceLoadingDataRequest => avAssetResourceLoadingDataRequest -> IO CLong
currentOffset avAssetResourceLoadingDataRequest =
  sendMessage avAssetResourceLoadingDataRequest currentOffsetSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetResourceLoadingDataRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetResourceLoadingDataRequest)
newSelector = mkSelector "new"

-- | @Selector@ for @respondWithData:@
respondWithDataSelector :: Selector '[Id NSData] ()
respondWithDataSelector = mkSelector "respondWithData:"

-- | @Selector@ for @requestedOffset@
requestedOffsetSelector :: Selector '[] CLong
requestedOffsetSelector = mkSelector "requestedOffset"

-- | @Selector@ for @requestedLength@
requestedLengthSelector :: Selector '[] CLong
requestedLengthSelector = mkSelector "requestedLength"

-- | @Selector@ for @requestsAllDataToEndOfResource@
requestsAllDataToEndOfResourceSelector :: Selector '[] Bool
requestsAllDataToEndOfResourceSelector = mkSelector "requestsAllDataToEndOfResource"

-- | @Selector@ for @currentOffset@
currentOffsetSelector :: Selector '[] CLong
currentOffsetSelector = mkSelector "currentOffset"

