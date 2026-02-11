{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSCachedURLResponse
--
-- NSCachedURLResponse is a class whose objects functions as a wrapper for    objects that are stored in the framework's caching system.     It is used to maintain characteristics and attributes of a cached     object.
--
-- Generated bindings for @NSCachedURLResponse@.
module ObjC.Foundation.NSCachedURLResponse
  ( NSCachedURLResponse
  , IsNSCachedURLResponse(..)
  , initWithResponse_data
  , initWithResponse_data_userInfo_storagePolicy
  , response
  , data_
  , userInfo
  , storagePolicy
  , initWithResponse_dataSelector
  , initWithResponse_data_userInfo_storagePolicySelector
  , responseSelector
  , dataSelector
  , userInfoSelector
  , storagePolicySelector

  -- * Enum types
  , NSURLCacheStoragePolicy(NSURLCacheStoragePolicy)
  , pattern NSURLCacheStorageAllowed
  , pattern NSURLCacheStorageAllowedInMemoryOnly
  , pattern NSURLCacheStorageNotAllowed

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | initWithResponse:data
--
-- Initializes an NSCachedURLResponse with the given    response and data.
--
-- A default NSURLCacheStoragePolicy is used for    NSCachedURLResponse objects initialized with this method:    NSURLCacheStorageAllowed.
--
-- @response@ — a NSURLResponse object.
--
-- @data@ — an NSData object representing the URL content    corresponding to the given response.
--
-- Returns: an initialized NSCachedURLResponse.
--
-- ObjC selector: @- initWithResponse:data:@
initWithResponse_data :: (IsNSCachedURLResponse nsCachedURLResponse, IsNSURLResponse response, IsNSData data_) => nsCachedURLResponse -> response -> data_ -> IO (Id NSCachedURLResponse)
initWithResponse_data nsCachedURLResponse  response data_ =
withObjCPtr response $ \raw_response ->
  withObjCPtr data_ $ \raw_data_ ->
      sendMsg nsCachedURLResponse (mkSelector "initWithResponse:data:") (retPtr retVoid) [argPtr (castPtr raw_response :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | initWithResponse:data:userInfo:storagePolicy:
--
-- Initializes an NSCachedURLResponse with the given    response, data, user-info dictionary, and storage policy.
--
-- @response@ — a NSURLResponse object.
--
-- @data@ — an NSData object representing the URL content    corresponding to the given response.
--
-- @userInfo@ — a dictionary user-specified information to be    stored with the NSCachedURLResponse.
--
-- @storagePolicy@ — an NSURLCacheStoragePolicy constant.
--
-- Returns: an initialized NSCachedURLResponse.
--
-- ObjC selector: @- initWithResponse:data:userInfo:storagePolicy:@
initWithResponse_data_userInfo_storagePolicy :: (IsNSCachedURLResponse nsCachedURLResponse, IsNSURLResponse response, IsNSData data_, IsNSDictionary userInfo) => nsCachedURLResponse -> response -> data_ -> userInfo -> NSURLCacheStoragePolicy -> IO (Id NSCachedURLResponse)
initWithResponse_data_userInfo_storagePolicy nsCachedURLResponse  response data_ userInfo storagePolicy =
withObjCPtr response $ \raw_response ->
  withObjCPtr data_ $ \raw_data_ ->
    withObjCPtr userInfo $ \raw_userInfo ->
        sendMsg nsCachedURLResponse (mkSelector "initWithResponse:data:userInfo:storagePolicy:") (retPtr retVoid) [argPtr (castPtr raw_response :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr raw_userInfo :: Ptr ()), argCULong (coerce storagePolicy)] >>= ownedObject . castPtr

-- | Returns the response wrapped by this instance.
--
-- Returns: The response wrapped by this instance.
--
-- ObjC selector: @- response@
response :: IsNSCachedURLResponse nsCachedURLResponse => nsCachedURLResponse -> IO (Id NSURLResponse)
response nsCachedURLResponse  =
  sendMsg nsCachedURLResponse (mkSelector "response") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the data of the receiver.
--
-- Returns: The data of the receiver.
--
-- ObjC selector: @- data@
data_ :: IsNSCachedURLResponse nsCachedURLResponse => nsCachedURLResponse -> IO (Id NSData)
data_ nsCachedURLResponse  =
  sendMsg nsCachedURLResponse (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the userInfo dictionary of the receiver.
--
-- Returns: The userInfo dictionary of the receiver.
--
-- ObjC selector: @- userInfo@
userInfo :: IsNSCachedURLResponse nsCachedURLResponse => nsCachedURLResponse -> IO (Id NSDictionary)
userInfo nsCachedURLResponse  =
  sendMsg nsCachedURLResponse (mkSelector "userInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the NSURLCacheStoragePolicy constant of the receiver.
--
-- Returns: The NSURLCacheStoragePolicy constant of the receiver.
--
-- ObjC selector: @- storagePolicy@
storagePolicy :: IsNSCachedURLResponse nsCachedURLResponse => nsCachedURLResponse -> IO NSURLCacheStoragePolicy
storagePolicy nsCachedURLResponse  =
  fmap (coerce :: CULong -> NSURLCacheStoragePolicy) $ sendMsg nsCachedURLResponse (mkSelector "storagePolicy") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponse:data:@
initWithResponse_dataSelector :: Selector
initWithResponse_dataSelector = mkSelector "initWithResponse:data:"

-- | @Selector@ for @initWithResponse:data:userInfo:storagePolicy:@
initWithResponse_data_userInfo_storagePolicySelector :: Selector
initWithResponse_data_userInfo_storagePolicySelector = mkSelector "initWithResponse:data:userInfo:storagePolicy:"

-- | @Selector@ for @response@
responseSelector :: Selector
responseSelector = mkSelector "response"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @storagePolicy@
storagePolicySelector :: Selector
storagePolicySelector = mkSelector "storagePolicy"

