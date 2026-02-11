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
  , contentLength
  , setContentLength
  , byteRangeAccessSupported
  , setByteRangeAccessSupported
  , entireLengthAvailableOnDemand
  , setEntireLengthAvailableOnDemand
  , initSelector
  , newSelector
  , contentTypeSelector
  , setContentTypeSelector
  , contentLengthSelector
  , setContentLengthSelector
  , byteRangeAccessSupportedSelector
  , setByteRangeAccessSupportedSelector
  , entireLengthAvailableOnDemandSelector
  , setEntireLengthAvailableOnDemandSelector


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
init_ :: IsAVAssetResourceLoadingContentInformationRequest avAssetResourceLoadingContentInformationRequest => avAssetResourceLoadingContentInformationRequest -> IO (Id AVAssetResourceLoadingContentInformationRequest)
init_ avAssetResourceLoadingContentInformationRequest  =
  sendMsg avAssetResourceLoadingContentInformationRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetResourceLoadingContentInformationRequest)
new  =
  do
    cls' <- getRequiredClass "AVAssetResourceLoadingContentInformationRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | contentType
--
-- A UTI that indicates the type of data contained by the requested resource.
--
-- Before you finish loading an AVAssetResourceLoadingRequest, if its contentInformationRequest is not nil, you should set the value of this property to a UTI indicating the type of data contained by the requested resource.
--
-- ObjC selector: @- contentType@
contentType :: IsAVAssetResourceLoadingContentInformationRequest avAssetResourceLoadingContentInformationRequest => avAssetResourceLoadingContentInformationRequest -> IO (Id NSString)
contentType avAssetResourceLoadingContentInformationRequest  =
  sendMsg avAssetResourceLoadingContentInformationRequest (mkSelector "contentType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | contentType
--
-- A UTI that indicates the type of data contained by the requested resource.
--
-- Before you finish loading an AVAssetResourceLoadingRequest, if its contentInformationRequest is not nil, you should set the value of this property to a UTI indicating the type of data contained by the requested resource.
--
-- ObjC selector: @- setContentType:@
setContentType :: (IsAVAssetResourceLoadingContentInformationRequest avAssetResourceLoadingContentInformationRequest, IsNSString value) => avAssetResourceLoadingContentInformationRequest -> value -> IO ()
setContentType avAssetResourceLoadingContentInformationRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg avAssetResourceLoadingContentInformationRequest (mkSelector "setContentType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | contentLength
--
-- Indicates the length of the requested resource, in bytes.
--
-- Before you finish loading an AVAssetResourceLoadingRequest, if its contentInformationRequest is not nil, you should set the value of this property to the number of bytes contained by the requested resource.
--
-- ObjC selector: @- contentLength@
contentLength :: IsAVAssetResourceLoadingContentInformationRequest avAssetResourceLoadingContentInformationRequest => avAssetResourceLoadingContentInformationRequest -> IO CLong
contentLength avAssetResourceLoadingContentInformationRequest  =
  sendMsg avAssetResourceLoadingContentInformationRequest (mkSelector "contentLength") retCLong []

-- | contentLength
--
-- Indicates the length of the requested resource, in bytes.
--
-- Before you finish loading an AVAssetResourceLoadingRequest, if its contentInformationRequest is not nil, you should set the value of this property to the number of bytes contained by the requested resource.
--
-- ObjC selector: @- setContentLength:@
setContentLength :: IsAVAssetResourceLoadingContentInformationRequest avAssetResourceLoadingContentInformationRequest => avAssetResourceLoadingContentInformationRequest -> CLong -> IO ()
setContentLength avAssetResourceLoadingContentInformationRequest  value =
  sendMsg avAssetResourceLoadingContentInformationRequest (mkSelector "setContentLength:") retVoid [argCLong (fromIntegral value)]

-- | byteRangeAccessSupported
--
-- Indicates whether random access to arbitrary ranges of bytes of the resource is supported. Such support also allows portions of the resource to be requested more than once.
--
-- Before you finish loading an AVAssetResourceLoadingRequest, if its contentInformationRequest is not nil, you should set the value of this property to YES if you support random access to arbitrary ranges of bytes of the resource. If you do not set this property to YES for resources that must be loaded incrementally, loading of the resource may fail. Such resources include anything that contains media data.
--
-- ObjC selector: @- byteRangeAccessSupported@
byteRangeAccessSupported :: IsAVAssetResourceLoadingContentInformationRequest avAssetResourceLoadingContentInformationRequest => avAssetResourceLoadingContentInformationRequest -> IO Bool
byteRangeAccessSupported avAssetResourceLoadingContentInformationRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetResourceLoadingContentInformationRequest (mkSelector "byteRangeAccessSupported") retCULong []

-- | byteRangeAccessSupported
--
-- Indicates whether random access to arbitrary ranges of bytes of the resource is supported. Such support also allows portions of the resource to be requested more than once.
--
-- Before you finish loading an AVAssetResourceLoadingRequest, if its contentInformationRequest is not nil, you should set the value of this property to YES if you support random access to arbitrary ranges of bytes of the resource. If you do not set this property to YES for resources that must be loaded incrementally, loading of the resource may fail. Such resources include anything that contains media data.
--
-- ObjC selector: @- setByteRangeAccessSupported:@
setByteRangeAccessSupported :: IsAVAssetResourceLoadingContentInformationRequest avAssetResourceLoadingContentInformationRequest => avAssetResourceLoadingContentInformationRequest -> Bool -> IO ()
setByteRangeAccessSupported avAssetResourceLoadingContentInformationRequest  value =
  sendMsg avAssetResourceLoadingContentInformationRequest (mkSelector "setByteRangeAccessSupported:") retVoid [argCULong (if value then 1 else 0)]

-- | entireLengthAvailableOnDemand
--
-- Indicates whether asset data loading can expect data to be produced immediately.
--
-- Before you finish loading an AVAssetResourceLoadingRequest, if its contentInformationRequest is not nil, you may set this property to YES to indicate that all asset data can be produced immediately, e.g., because the data is fully cached, or because the custom URL scheme ultimately refers to files on local storage. This allows significant data flow optimizations. For backward compatibility, this property defaults to NO.
--
-- ObjC selector: @- entireLengthAvailableOnDemand@
entireLengthAvailableOnDemand :: IsAVAssetResourceLoadingContentInformationRequest avAssetResourceLoadingContentInformationRequest => avAssetResourceLoadingContentInformationRequest -> IO Bool
entireLengthAvailableOnDemand avAssetResourceLoadingContentInformationRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetResourceLoadingContentInformationRequest (mkSelector "entireLengthAvailableOnDemand") retCULong []

-- | entireLengthAvailableOnDemand
--
-- Indicates whether asset data loading can expect data to be produced immediately.
--
-- Before you finish loading an AVAssetResourceLoadingRequest, if its contentInformationRequest is not nil, you may set this property to YES to indicate that all asset data can be produced immediately, e.g., because the data is fully cached, or because the custom URL scheme ultimately refers to files on local storage. This allows significant data flow optimizations. For backward compatibility, this property defaults to NO.
--
-- ObjC selector: @- setEntireLengthAvailableOnDemand:@
setEntireLengthAvailableOnDemand :: IsAVAssetResourceLoadingContentInformationRequest avAssetResourceLoadingContentInformationRequest => avAssetResourceLoadingContentInformationRequest -> Bool -> IO ()
setEntireLengthAvailableOnDemand avAssetResourceLoadingContentInformationRequest  value =
  sendMsg avAssetResourceLoadingContentInformationRequest (mkSelector "setEntireLengthAvailableOnDemand:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @contentType@
contentTypeSelector :: Selector
contentTypeSelector = mkSelector "contentType"

-- | @Selector@ for @setContentType:@
setContentTypeSelector :: Selector
setContentTypeSelector = mkSelector "setContentType:"

-- | @Selector@ for @contentLength@
contentLengthSelector :: Selector
contentLengthSelector = mkSelector "contentLength"

-- | @Selector@ for @setContentLength:@
setContentLengthSelector :: Selector
setContentLengthSelector = mkSelector "setContentLength:"

-- | @Selector@ for @byteRangeAccessSupported@
byteRangeAccessSupportedSelector :: Selector
byteRangeAccessSupportedSelector = mkSelector "byteRangeAccessSupported"

-- | @Selector@ for @setByteRangeAccessSupported:@
setByteRangeAccessSupportedSelector :: Selector
setByteRangeAccessSupportedSelector = mkSelector "setByteRangeAccessSupported:"

-- | @Selector@ for @entireLengthAvailableOnDemand@
entireLengthAvailableOnDemandSelector :: Selector
entireLengthAvailableOnDemandSelector = mkSelector "entireLengthAvailableOnDemand"

-- | @Selector@ for @setEntireLengthAvailableOnDemand:@
setEntireLengthAvailableOnDemandSelector :: Selector
setEntireLengthAvailableOnDemandSelector = mkSelector "setEntireLengthAvailableOnDemand:"

