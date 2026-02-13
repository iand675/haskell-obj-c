{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , dataSelector
  , initWithResponse_dataSelector
  , initWithResponse_data_userInfo_storagePolicySelector
  , responseSelector
  , storagePolicySelector
  , userInfoSelector

  -- * Enum types
  , NSURLCacheStoragePolicy(NSURLCacheStoragePolicy)
  , pattern NSURLCacheStorageAllowed
  , pattern NSURLCacheStorageAllowedInMemoryOnly
  , pattern NSURLCacheStorageNotAllowed

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithResponse_data nsCachedURLResponse response data_ =
  sendOwnedMessage nsCachedURLResponse initWithResponse_dataSelector (toNSURLResponse response) (toNSData data_)

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
initWithResponse_data_userInfo_storagePolicy nsCachedURLResponse response data_ userInfo storagePolicy =
  sendOwnedMessage nsCachedURLResponse initWithResponse_data_userInfo_storagePolicySelector (toNSURLResponse response) (toNSData data_) (toNSDictionary userInfo) storagePolicy

-- | Returns the response wrapped by this instance.
--
-- Returns: The response wrapped by this instance.
--
-- ObjC selector: @- response@
response :: IsNSCachedURLResponse nsCachedURLResponse => nsCachedURLResponse -> IO (Id NSURLResponse)
response nsCachedURLResponse =
  sendMessage nsCachedURLResponse responseSelector

-- | Returns the data of the receiver.
--
-- Returns: The data of the receiver.
--
-- ObjC selector: @- data@
data_ :: IsNSCachedURLResponse nsCachedURLResponse => nsCachedURLResponse -> IO (Id NSData)
data_ nsCachedURLResponse =
  sendMessage nsCachedURLResponse dataSelector

-- | Returns the userInfo dictionary of the receiver.
--
-- Returns: The userInfo dictionary of the receiver.
--
-- ObjC selector: @- userInfo@
userInfo :: IsNSCachedURLResponse nsCachedURLResponse => nsCachedURLResponse -> IO (Id NSDictionary)
userInfo nsCachedURLResponse =
  sendMessage nsCachedURLResponse userInfoSelector

-- | Returns the NSURLCacheStoragePolicy constant of the receiver.
--
-- Returns: The NSURLCacheStoragePolicy constant of the receiver.
--
-- ObjC selector: @- storagePolicy@
storagePolicy :: IsNSCachedURLResponse nsCachedURLResponse => nsCachedURLResponse -> IO NSURLCacheStoragePolicy
storagePolicy nsCachedURLResponse =
  sendMessage nsCachedURLResponse storagePolicySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponse:data:@
initWithResponse_dataSelector :: Selector '[Id NSURLResponse, Id NSData] (Id NSCachedURLResponse)
initWithResponse_dataSelector = mkSelector "initWithResponse:data:"

-- | @Selector@ for @initWithResponse:data:userInfo:storagePolicy:@
initWithResponse_data_userInfo_storagePolicySelector :: Selector '[Id NSURLResponse, Id NSData, Id NSDictionary, NSURLCacheStoragePolicy] (Id NSCachedURLResponse)
initWithResponse_data_userInfo_storagePolicySelector = mkSelector "initWithResponse:data:userInfo:storagePolicy:"

-- | @Selector@ for @response@
responseSelector :: Selector '[] (Id NSURLResponse)
responseSelector = mkSelector "response"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSDictionary)
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @storagePolicy@
storagePolicySelector :: Selector '[] NSURLCacheStoragePolicy
storagePolicySelector = mkSelector "storagePolicy"

