{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVAssetResourceLoadingContentInformationRequest@.
module ObjC.AVFoundation.AVAssetResourceLoadingContentInformationRequest
  ( AVAssetResourceLoadingContentInformationRequest
  , IsAVAssetResourceLoadingContentInformationRequest(..)
  , init_
  , new
  , contentType
  , setContentType
  , allowedContentTypes
  , contentLength
  , setContentLength
  , byteRangeAccessSupported
  , setByteRangeAccessSupported
  , renewalDate
  , setRenewalDate
  , entireLengthAvailableOnDemand
  , setEntireLengthAvailableOnDemand
  , allowedContentTypesSelector
  , byteRangeAccessSupportedSelector
  , contentLengthSelector
  , contentTypeSelector
  , entireLengthAvailableOnDemandSelector
  , initSelector
  , newSelector
  , renewalDateSelector
  , setByteRangeAccessSupportedSelector
  , setContentLengthSelector
  , setContentTypeSelector
  , setEntireLengthAvailableOnDemandSelector
  , setRenewalDateSelector


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
init_ :: IsAVAssetResourceLoadingContentInformationRequest avAssetResourceLoadingContentInformationRequest => avAssetResourceLoadingContentInformationRequest -> IO (Id AVAssetResourceLoadingContentInformationRequest)
init_ avAssetResourceLoadingContentInformationRequest =
  sendOwnedMessage avAssetResourceLoadingContentInformationRequest initSelector

-- | @+ new@
new :: IO (Id AVAssetResourceLoadingContentInformationRequest)
new  =
  do
    cls' <- getRequiredClass "AVAssetResourceLoadingContentInformationRequest"
    sendOwnedClassMessage cls' newSelector

-- | contentType
--
-- A UTI that indicates the type of data contained by the requested resource.
--
-- Before you finish loading an AVAssetResourceLoadingRequest, if its contentInformationRequest is not nil, you should set the value of this property to a UTI indicating the type of data contained by the requested resource.
--
-- ObjC selector: @- contentType@
contentType :: IsAVAssetResourceLoadingContentInformationRequest avAssetResourceLoadingContentInformationRequest => avAssetResourceLoadingContentInformationRequest -> IO (Id NSString)
contentType avAssetResourceLoadingContentInformationRequest =
  sendMessage avAssetResourceLoadingContentInformationRequest contentTypeSelector

-- | contentType
--
-- A UTI that indicates the type of data contained by the requested resource.
--
-- Before you finish loading an AVAssetResourceLoadingRequest, if its contentInformationRequest is not nil, you should set the value of this property to a UTI indicating the type of data contained by the requested resource.
--
-- ObjC selector: @- setContentType:@
setContentType :: (IsAVAssetResourceLoadingContentInformationRequest avAssetResourceLoadingContentInformationRequest, IsNSString value) => avAssetResourceLoadingContentInformationRequest -> value -> IO ()
setContentType avAssetResourceLoadingContentInformationRequest value =
  sendMessage avAssetResourceLoadingContentInformationRequest setContentTypeSelector (toNSString value)

-- | allowedContentTypes
--
-- An array showing the types of data which will be accepted as a valid response for the requested resource.
--
-- If an AVAssetResourceLoadingRequest's contentInformationRequest is not nil, ensure that the value assigned to the contentType property is in this array. Otherwise, calling -finishLoading on the associated request will result in an exception.
--
-- ObjC selector: @- allowedContentTypes@
allowedContentTypes :: IsAVAssetResourceLoadingContentInformationRequest avAssetResourceLoadingContentInformationRequest => avAssetResourceLoadingContentInformationRequest -> IO (Id NSArray)
allowedContentTypes avAssetResourceLoadingContentInformationRequest =
  sendMessage avAssetResourceLoadingContentInformationRequest allowedContentTypesSelector

-- | contentLength
--
-- Indicates the length of the requested resource, in bytes.
--
-- Before you finish loading an AVAssetResourceLoadingRequest, if its contentInformationRequest is not nil, you should set the value of this property to the number of bytes contained by the requested resource.
--
-- ObjC selector: @- contentLength@
contentLength :: IsAVAssetResourceLoadingContentInformationRequest avAssetResourceLoadingContentInformationRequest => avAssetResourceLoadingContentInformationRequest -> IO CLong
contentLength avAssetResourceLoadingContentInformationRequest =
  sendMessage avAssetResourceLoadingContentInformationRequest contentLengthSelector

-- | contentLength
--
-- Indicates the length of the requested resource, in bytes.
--
-- Before you finish loading an AVAssetResourceLoadingRequest, if its contentInformationRequest is not nil, you should set the value of this property to the number of bytes contained by the requested resource.
--
-- ObjC selector: @- setContentLength:@
setContentLength :: IsAVAssetResourceLoadingContentInformationRequest avAssetResourceLoadingContentInformationRequest => avAssetResourceLoadingContentInformationRequest -> CLong -> IO ()
setContentLength avAssetResourceLoadingContentInformationRequest value =
  sendMessage avAssetResourceLoadingContentInformationRequest setContentLengthSelector value

-- | byteRangeAccessSupported
--
-- Indicates whether random access to arbitrary ranges of bytes of the resource is supported. Such support also allows portions of the resource to be requested more than once.
--
-- Before you finish loading an AVAssetResourceLoadingRequest, if its contentInformationRequest is not nil, you should set the value of this property to YES if you support random access to arbitrary ranges of bytes of the resource. If you do not set this property to YES for resources that must be loaded incrementally, loading of the resource may fail. Such resources include anything that contains media data.
--
-- ObjC selector: @- byteRangeAccessSupported@
byteRangeAccessSupported :: IsAVAssetResourceLoadingContentInformationRequest avAssetResourceLoadingContentInformationRequest => avAssetResourceLoadingContentInformationRequest -> IO Bool
byteRangeAccessSupported avAssetResourceLoadingContentInformationRequest =
  sendMessage avAssetResourceLoadingContentInformationRequest byteRangeAccessSupportedSelector

-- | byteRangeAccessSupported
--
-- Indicates whether random access to arbitrary ranges of bytes of the resource is supported. Such support also allows portions of the resource to be requested more than once.
--
-- Before you finish loading an AVAssetResourceLoadingRequest, if its contentInformationRequest is not nil, you should set the value of this property to YES if you support random access to arbitrary ranges of bytes of the resource. If you do not set this property to YES for resources that must be loaded incrementally, loading of the resource may fail. Such resources include anything that contains media data.
--
-- ObjC selector: @- setByteRangeAccessSupported:@
setByteRangeAccessSupported :: IsAVAssetResourceLoadingContentInformationRequest avAssetResourceLoadingContentInformationRequest => avAssetResourceLoadingContentInformationRequest -> Bool -> IO ()
setByteRangeAccessSupported avAssetResourceLoadingContentInformationRequest value =
  sendMessage avAssetResourceLoadingContentInformationRequest setByteRangeAccessSupportedSelector value

-- | renewalDate
--
-- For resources that expire, the date at which a new AVAssetResourceLoadingRequest will be issued for a renewal of this resource, if the media system still requires it.
--
-- Before you finish loading an AVAssetResourceLoadingRequest, if the resource is prone to expiry you should set the value of this property to the date at which a renewal should be triggered. This value should be set sufficiently early enough to allow an AVAssetResourceRenewalRequest, delivered to your delegate via -resourceLoader:shouldWaitForRenewalOfRequestedResource:, to finish before the actual expiry time. Otherwise media playback may fail.
--
-- ObjC selector: @- renewalDate@
renewalDate :: IsAVAssetResourceLoadingContentInformationRequest avAssetResourceLoadingContentInformationRequest => avAssetResourceLoadingContentInformationRequest -> IO (Id NSDate)
renewalDate avAssetResourceLoadingContentInformationRequest =
  sendMessage avAssetResourceLoadingContentInformationRequest renewalDateSelector

-- | renewalDate
--
-- For resources that expire, the date at which a new AVAssetResourceLoadingRequest will be issued for a renewal of this resource, if the media system still requires it.
--
-- Before you finish loading an AVAssetResourceLoadingRequest, if the resource is prone to expiry you should set the value of this property to the date at which a renewal should be triggered. This value should be set sufficiently early enough to allow an AVAssetResourceRenewalRequest, delivered to your delegate via -resourceLoader:shouldWaitForRenewalOfRequestedResource:, to finish before the actual expiry time. Otherwise media playback may fail.
--
-- ObjC selector: @- setRenewalDate:@
setRenewalDate :: (IsAVAssetResourceLoadingContentInformationRequest avAssetResourceLoadingContentInformationRequest, IsNSDate value) => avAssetResourceLoadingContentInformationRequest -> value -> IO ()
setRenewalDate avAssetResourceLoadingContentInformationRequest value =
  sendMessage avAssetResourceLoadingContentInformationRequest setRenewalDateSelector (toNSDate value)

-- | entireLengthAvailableOnDemand
--
-- Indicates whether asset data loading can expect data to be produced immediately.
--
-- Before you finish loading an AVAssetResourceLoadingRequest, if its contentInformationRequest is not nil, you may set this property to YES to indicate that all asset data can be produced immediately, e.g., because the data is fully cached, or because the custom URL scheme ultimately refers to files on local storage. This allows significant data flow optimizations. For backward compatibility, this property defaults to NO.
--
-- ObjC selector: @- entireLengthAvailableOnDemand@
entireLengthAvailableOnDemand :: IsAVAssetResourceLoadingContentInformationRequest avAssetResourceLoadingContentInformationRequest => avAssetResourceLoadingContentInformationRequest -> IO Bool
entireLengthAvailableOnDemand avAssetResourceLoadingContentInformationRequest =
  sendMessage avAssetResourceLoadingContentInformationRequest entireLengthAvailableOnDemandSelector

-- | entireLengthAvailableOnDemand
--
-- Indicates whether asset data loading can expect data to be produced immediately.
--
-- Before you finish loading an AVAssetResourceLoadingRequest, if its contentInformationRequest is not nil, you may set this property to YES to indicate that all asset data can be produced immediately, e.g., because the data is fully cached, or because the custom URL scheme ultimately refers to files on local storage. This allows significant data flow optimizations. For backward compatibility, this property defaults to NO.
--
-- ObjC selector: @- setEntireLengthAvailableOnDemand:@
setEntireLengthAvailableOnDemand :: IsAVAssetResourceLoadingContentInformationRequest avAssetResourceLoadingContentInformationRequest => avAssetResourceLoadingContentInformationRequest -> Bool -> IO ()
setEntireLengthAvailableOnDemand avAssetResourceLoadingContentInformationRequest value =
  sendMessage avAssetResourceLoadingContentInformationRequest setEntireLengthAvailableOnDemandSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetResourceLoadingContentInformationRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetResourceLoadingContentInformationRequest)
newSelector = mkSelector "new"

-- | @Selector@ for @contentType@
contentTypeSelector :: Selector '[] (Id NSString)
contentTypeSelector = mkSelector "contentType"

-- | @Selector@ for @setContentType:@
setContentTypeSelector :: Selector '[Id NSString] ()
setContentTypeSelector = mkSelector "setContentType:"

-- | @Selector@ for @allowedContentTypes@
allowedContentTypesSelector :: Selector '[] (Id NSArray)
allowedContentTypesSelector = mkSelector "allowedContentTypes"

-- | @Selector@ for @contentLength@
contentLengthSelector :: Selector '[] CLong
contentLengthSelector = mkSelector "contentLength"

-- | @Selector@ for @setContentLength:@
setContentLengthSelector :: Selector '[CLong] ()
setContentLengthSelector = mkSelector "setContentLength:"

-- | @Selector@ for @byteRangeAccessSupported@
byteRangeAccessSupportedSelector :: Selector '[] Bool
byteRangeAccessSupportedSelector = mkSelector "byteRangeAccessSupported"

-- | @Selector@ for @setByteRangeAccessSupported:@
setByteRangeAccessSupportedSelector :: Selector '[Bool] ()
setByteRangeAccessSupportedSelector = mkSelector "setByteRangeAccessSupported:"

-- | @Selector@ for @renewalDate@
renewalDateSelector :: Selector '[] (Id NSDate)
renewalDateSelector = mkSelector "renewalDate"

-- | @Selector@ for @setRenewalDate:@
setRenewalDateSelector :: Selector '[Id NSDate] ()
setRenewalDateSelector = mkSelector "setRenewalDate:"

-- | @Selector@ for @entireLengthAvailableOnDemand@
entireLengthAvailableOnDemandSelector :: Selector '[] Bool
entireLengthAvailableOnDemandSelector = mkSelector "entireLengthAvailableOnDemand"

-- | @Selector@ for @setEntireLengthAvailableOnDemand:@
setEntireLengthAvailableOnDemandSelector :: Selector '[Bool] ()
setEntireLengthAvailableOnDemandSelector = mkSelector "setEntireLengthAvailableOnDemand:"

