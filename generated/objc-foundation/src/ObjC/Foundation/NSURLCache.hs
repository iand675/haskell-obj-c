{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSURLCache@.
module ObjC.Foundation.NSURLCache
  ( NSURLCache
  , IsNSURLCache(..)
  , initWithMemoryCapacity_diskCapacity_diskPath
  , initWithMemoryCapacity_diskCapacity_directoryURL
  , cachedResponseForRequest
  , storeCachedResponse_forRequest
  , removeCachedResponseForRequest
  , removeAllCachedResponses
  , removeCachedResponsesSinceDate
  , storeCachedResponse_forDataTask
  , getCachedResponseForDataTask_completionHandler
  , removeCachedResponseForDataTask
  , sharedURLCache
  , setSharedURLCache
  , memoryCapacity
  , setMemoryCapacity
  , diskCapacity
  , setDiskCapacity
  , currentMemoryUsage
  , currentDiskUsage
  , initWithMemoryCapacity_diskCapacity_diskPathSelector
  , initWithMemoryCapacity_diskCapacity_directoryURLSelector
  , cachedResponseForRequestSelector
  , storeCachedResponse_forRequestSelector
  , removeCachedResponseForRequestSelector
  , removeAllCachedResponsesSelector
  , removeCachedResponsesSinceDateSelector
  , storeCachedResponse_forDataTaskSelector
  , getCachedResponseForDataTask_completionHandlerSelector
  , removeCachedResponseForDataTaskSelector
  , sharedURLCacheSelector
  , setSharedURLCacheSelector
  , memoryCapacitySelector
  , setMemoryCapacitySelector
  , diskCapacitySelector
  , setDiskCapacitySelector
  , currentMemoryUsageSelector
  , currentDiskUsageSelector


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

-- | initWithMemoryCapacity:diskCapacity:diskPath:
--
-- Initializes an NSURLCache with the given capacity and    path.
--
-- The returned NSURLCache is backed by disk, so    developers can be more liberal with space when choosing the    capacity for this kind of cache. A disk cache measured in the tens    of megabytes should be acceptable in most cases.
--
-- @memoryCapacity@ — the capacity, measured in bytes, for the cache in memory.
--
-- @diskCapacity@ — the capacity, measured in bytes, for the cache on disk.
--
-- @path@ — the path on disk where the cache data is stored.
--
-- Returns: an initialized NSURLCache, with the given capacity, backed    by disk.
--
-- ObjC selector: @- initWithMemoryCapacity:diskCapacity:diskPath:@
initWithMemoryCapacity_diskCapacity_diskPath :: (IsNSURLCache nsurlCache, IsNSString path) => nsurlCache -> CULong -> CULong -> path -> IO (Id NSURLCache)
initWithMemoryCapacity_diskCapacity_diskPath nsurlCache  memoryCapacity diskCapacity path =
withObjCPtr path $ \raw_path ->
    sendMsg nsurlCache (mkSelector "initWithMemoryCapacity:diskCapacity:diskPath:") (retPtr retVoid) [argCULong (fromIntegral memoryCapacity), argCULong (fromIntegral diskCapacity), argPtr (castPtr raw_path :: Ptr ())] >>= ownedObject . castPtr

-- | initWithMemoryCapacity:diskCapacity:directoryURL:
--
-- Initializes an NSURLCache with the given capacity and directory.
--
-- @memoryCapacity@ — the capacity, measured in bytes, for the cache in memory. Or 0 to disable memory cache.
--
-- @diskCapacity@ — the capacity, measured in bytes, for the cache on disk. Or 0 to disable disk cache.
--
-- @directoryURL@ — the path to a directory on disk where the cache data is stored. Or nil for default directory.
--
-- Returns: an initialized NSURLCache, with the given capacity, optionally backed by disk.
--
-- ObjC selector: @- initWithMemoryCapacity:diskCapacity:directoryURL:@
initWithMemoryCapacity_diskCapacity_directoryURL :: (IsNSURLCache nsurlCache, IsNSURL directoryURL) => nsurlCache -> CULong -> CULong -> directoryURL -> IO (Id NSURLCache)
initWithMemoryCapacity_diskCapacity_directoryURL nsurlCache  memoryCapacity diskCapacity directoryURL =
withObjCPtr directoryURL $ \raw_directoryURL ->
    sendMsg nsurlCache (mkSelector "initWithMemoryCapacity:diskCapacity:directoryURL:") (retPtr retVoid) [argCULong (fromIntegral memoryCapacity), argCULong (fromIntegral diskCapacity), argPtr (castPtr raw_directoryURL :: Ptr ())] >>= ownedObject . castPtr

-- | cachedResponseForRequest:
--
-- Returns the NSCachedURLResponse stored in the cache with    the given request.
--
-- The method returns nil if there is no    NSCachedURLResponse stored using the given request.
--
-- @request@ — the NSURLRequest to use as a key for the lookup.
--
-- Returns: The NSCachedURLResponse stored in the cache with the given    request, or nil if there is no NSCachedURLResponse stored with the    given request.
--
-- ObjC selector: @- cachedResponseForRequest:@
cachedResponseForRequest :: (IsNSURLCache nsurlCache, IsNSURLRequest request) => nsurlCache -> request -> IO (Id NSCachedURLResponse)
cachedResponseForRequest nsurlCache  request =
withObjCPtr request $ \raw_request ->
    sendMsg nsurlCache (mkSelector "cachedResponseForRequest:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ())] >>= retainedObject . castPtr

-- | storeCachedResponse:forRequest:
--
-- Stores the given NSCachedURLResponse in the cache using    the given request.
--
-- @cachedResponse@ — The cached response to store.
--
-- @request@ — the NSURLRequest to use as a key for the storage.
--
-- ObjC selector: @- storeCachedResponse:forRequest:@
storeCachedResponse_forRequest :: (IsNSURLCache nsurlCache, IsNSCachedURLResponse cachedResponse, IsNSURLRequest request) => nsurlCache -> cachedResponse -> request -> IO ()
storeCachedResponse_forRequest nsurlCache  cachedResponse request =
withObjCPtr cachedResponse $ \raw_cachedResponse ->
  withObjCPtr request $ \raw_request ->
      sendMsg nsurlCache (mkSelector "storeCachedResponse:forRequest:") retVoid [argPtr (castPtr raw_cachedResponse :: Ptr ()), argPtr (castPtr raw_request :: Ptr ())]

-- | removeCachedResponseForRequest:
--
-- Removes the NSCachedURLResponse from the cache that is    stored using the given request.
--
-- No action is taken if there is no NSCachedURLResponse    stored with the given request.
--
-- @request@ — the NSURLRequest to use as a key for the lookup.
--
-- ObjC selector: @- removeCachedResponseForRequest:@
removeCachedResponseForRequest :: (IsNSURLCache nsurlCache, IsNSURLRequest request) => nsurlCache -> request -> IO ()
removeCachedResponseForRequest nsurlCache  request =
withObjCPtr request $ \raw_request ->
    sendMsg nsurlCache (mkSelector "removeCachedResponseForRequest:") retVoid [argPtr (castPtr raw_request :: Ptr ())]

-- | removeAllCachedResponses
--
-- Clears the given cache, removing all NSCachedURLResponse    objects that it stores.
--
-- ObjC selector: @- removeAllCachedResponses@
removeAllCachedResponses :: IsNSURLCache nsurlCache => nsurlCache -> IO ()
removeAllCachedResponses nsurlCache  =
  sendMsg nsurlCache (mkSelector "removeAllCachedResponses") retVoid []

-- | removeCachedResponsesSince:
--
-- Clears the given cache of any cached responses since the provided date.
--
-- ObjC selector: @- removeCachedResponsesSinceDate:@
removeCachedResponsesSinceDate :: (IsNSURLCache nsurlCache, IsNSDate date) => nsurlCache -> date -> IO ()
removeCachedResponsesSinceDate nsurlCache  date =
withObjCPtr date $ \raw_date ->
    sendMsg nsurlCache (mkSelector "removeCachedResponsesSinceDate:") retVoid [argPtr (castPtr raw_date :: Ptr ())]

-- | @- storeCachedResponse:forDataTask:@
storeCachedResponse_forDataTask :: (IsNSURLCache nsurlCache, IsNSCachedURLResponse cachedResponse, IsNSURLSessionDataTask dataTask) => nsurlCache -> cachedResponse -> dataTask -> IO ()
storeCachedResponse_forDataTask nsurlCache  cachedResponse dataTask =
withObjCPtr cachedResponse $ \raw_cachedResponse ->
  withObjCPtr dataTask $ \raw_dataTask ->
      sendMsg nsurlCache (mkSelector "storeCachedResponse:forDataTask:") retVoid [argPtr (castPtr raw_cachedResponse :: Ptr ()), argPtr (castPtr raw_dataTask :: Ptr ())]

-- | @- getCachedResponseForDataTask:completionHandler:@
getCachedResponseForDataTask_completionHandler :: (IsNSURLCache nsurlCache, IsNSURLSessionDataTask dataTask) => nsurlCache -> dataTask -> Ptr () -> IO ()
getCachedResponseForDataTask_completionHandler nsurlCache  dataTask completionHandler =
withObjCPtr dataTask $ \raw_dataTask ->
    sendMsg nsurlCache (mkSelector "getCachedResponseForDataTask:completionHandler:") retVoid [argPtr (castPtr raw_dataTask :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- removeCachedResponseForDataTask:@
removeCachedResponseForDataTask :: (IsNSURLCache nsurlCache, IsNSURLSessionDataTask dataTask) => nsurlCache -> dataTask -> IO ()
removeCachedResponseForDataTask nsurlCache  dataTask =
withObjCPtr dataTask $ \raw_dataTask ->
    sendMsg nsurlCache (mkSelector "removeCachedResponseForDataTask:") retVoid [argPtr (castPtr raw_dataTask :: Ptr ())]

-- | sharedURLCache
--
-- Returns the shared NSURLCache instance or    sets the NSURLCache instance shared by all clients of    the current process. This will be the new object returned when    calls to the sharedURLCache method are made.
--
-- Unless set explicitly through a call to    +setSharedURLCache:, this method returns an NSURLCache    instance created with the following default values:        Memory capacity: 4 megabytes (4 * 1024 * 1024 bytes)    Disk capacity: 20 megabytes (20 * 1024 * 1024 bytes)    Disk path: <nobr>(user home directory)/Library/Caches/(application bundle id)</nobr>         Users who do not have special caching requirements or    constraints should find the default shared cache instance    acceptable. If this default shared cache instance is not    acceptable, +setSharedURLCache: can be called to set a    different NSURLCache instance to be returned from this method.     Callers should take care to ensure that the setter is called    at a time when no other caller has a reference to the previously-set     shared URL cache. This is to prevent storing cache data from     becoming unexpectedly unretrievable.
--
-- Returns: the shared NSURLCache instance.
--
-- ObjC selector: @+ sharedURLCache@
sharedURLCache :: IO (Id NSURLCache)
sharedURLCache  =
  do
    cls' <- getRequiredClass "NSURLCache"
    sendClassMsg cls' (mkSelector "sharedURLCache") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sharedURLCache
--
-- Returns the shared NSURLCache instance or    sets the NSURLCache instance shared by all clients of    the current process. This will be the new object returned when    calls to the sharedURLCache method are made.
--
-- Unless set explicitly through a call to    +setSharedURLCache:, this method returns an NSURLCache    instance created with the following default values:        Memory capacity: 4 megabytes (4 * 1024 * 1024 bytes)    Disk capacity: 20 megabytes (20 * 1024 * 1024 bytes)    Disk path: <nobr>(user home directory)/Library/Caches/(application bundle id)</nobr>         Users who do not have special caching requirements or    constraints should find the default shared cache instance    acceptable. If this default shared cache instance is not    acceptable, +setSharedURLCache: can be called to set a    different NSURLCache instance to be returned from this method.     Callers should take care to ensure that the setter is called    at a time when no other caller has a reference to the previously-set     shared URL cache. This is to prevent storing cache data from     becoming unexpectedly unretrievable.
--
-- Returns: the shared NSURLCache instance.
--
-- ObjC selector: @+ setSharedURLCache:@
setSharedURLCache :: IsNSURLCache value => value -> IO ()
setSharedURLCache value =
  do
    cls' <- getRequiredClass "NSURLCache"
    withObjCPtr value $ \raw_value ->
      sendClassMsg cls' (mkSelector "setSharedURLCache:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | In-memory capacity of the receiver.
--
-- At the time this call is made, the in-memory cache will truncate its contents to the size given, if necessary.
--
-- Returns: The in-memory capacity, measured in bytes, for the receiver.
--
-- ObjC selector: @- memoryCapacity@
memoryCapacity :: IsNSURLCache nsurlCache => nsurlCache -> IO CULong
memoryCapacity nsurlCache  =
  sendMsg nsurlCache (mkSelector "memoryCapacity") retCULong []

-- | In-memory capacity of the receiver.
--
-- At the time this call is made, the in-memory cache will truncate its contents to the size given, if necessary.
--
-- Returns: The in-memory capacity, measured in bytes, for the receiver.
--
-- ObjC selector: @- setMemoryCapacity:@
setMemoryCapacity :: IsNSURLCache nsurlCache => nsurlCache -> CULong -> IO ()
setMemoryCapacity nsurlCache  value =
  sendMsg nsurlCache (mkSelector "setMemoryCapacity:") retVoid [argCULong (fromIntegral value)]

-- | The on-disk capacity of the receiver.
--
-- The on-disk capacity, measured in bytes, for the receiver. On mutation the on-disk cache will truncate its contents to the size given, if necessary.
--
-- ObjC selector: @- diskCapacity@
diskCapacity :: IsNSURLCache nsurlCache => nsurlCache -> IO CULong
diskCapacity nsurlCache  =
  sendMsg nsurlCache (mkSelector "diskCapacity") retCULong []

-- | The on-disk capacity of the receiver.
--
-- The on-disk capacity, measured in bytes, for the receiver. On mutation the on-disk cache will truncate its contents to the size given, if necessary.
--
-- ObjC selector: @- setDiskCapacity:@
setDiskCapacity :: IsNSURLCache nsurlCache => nsurlCache -> CULong -> IO ()
setDiskCapacity nsurlCache  value =
  sendMsg nsurlCache (mkSelector "setDiskCapacity:") retVoid [argCULong (fromIntegral value)]

-- | Returns the current amount of space consumed by the    in-memory cache of the receiver.
--
-- This size, measured in bytes, indicates the current    usage of the in-memory cache.
--
-- Returns: the current usage of the in-memory cache of the receiver.
--
-- ObjC selector: @- currentMemoryUsage@
currentMemoryUsage :: IsNSURLCache nsurlCache => nsurlCache -> IO CULong
currentMemoryUsage nsurlCache  =
  sendMsg nsurlCache (mkSelector "currentMemoryUsage") retCULong []

-- | Returns the current amount of space consumed by the    on-disk cache of the receiver.
--
-- This size, measured in bytes, indicates the current    usage of the on-disk cache.
--
-- Returns: the current usage of the on-disk cache of the receiver.
--
-- ObjC selector: @- currentDiskUsage@
currentDiskUsage :: IsNSURLCache nsurlCache => nsurlCache -> IO CULong
currentDiskUsage nsurlCache  =
  sendMsg nsurlCache (mkSelector "currentDiskUsage") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMemoryCapacity:diskCapacity:diskPath:@
initWithMemoryCapacity_diskCapacity_diskPathSelector :: Selector
initWithMemoryCapacity_diskCapacity_diskPathSelector = mkSelector "initWithMemoryCapacity:diskCapacity:diskPath:"

-- | @Selector@ for @initWithMemoryCapacity:diskCapacity:directoryURL:@
initWithMemoryCapacity_diskCapacity_directoryURLSelector :: Selector
initWithMemoryCapacity_diskCapacity_directoryURLSelector = mkSelector "initWithMemoryCapacity:diskCapacity:directoryURL:"

-- | @Selector@ for @cachedResponseForRequest:@
cachedResponseForRequestSelector :: Selector
cachedResponseForRequestSelector = mkSelector "cachedResponseForRequest:"

-- | @Selector@ for @storeCachedResponse:forRequest:@
storeCachedResponse_forRequestSelector :: Selector
storeCachedResponse_forRequestSelector = mkSelector "storeCachedResponse:forRequest:"

-- | @Selector@ for @removeCachedResponseForRequest:@
removeCachedResponseForRequestSelector :: Selector
removeCachedResponseForRequestSelector = mkSelector "removeCachedResponseForRequest:"

-- | @Selector@ for @removeAllCachedResponses@
removeAllCachedResponsesSelector :: Selector
removeAllCachedResponsesSelector = mkSelector "removeAllCachedResponses"

-- | @Selector@ for @removeCachedResponsesSinceDate:@
removeCachedResponsesSinceDateSelector :: Selector
removeCachedResponsesSinceDateSelector = mkSelector "removeCachedResponsesSinceDate:"

-- | @Selector@ for @storeCachedResponse:forDataTask:@
storeCachedResponse_forDataTaskSelector :: Selector
storeCachedResponse_forDataTaskSelector = mkSelector "storeCachedResponse:forDataTask:"

-- | @Selector@ for @getCachedResponseForDataTask:completionHandler:@
getCachedResponseForDataTask_completionHandlerSelector :: Selector
getCachedResponseForDataTask_completionHandlerSelector = mkSelector "getCachedResponseForDataTask:completionHandler:"

-- | @Selector@ for @removeCachedResponseForDataTask:@
removeCachedResponseForDataTaskSelector :: Selector
removeCachedResponseForDataTaskSelector = mkSelector "removeCachedResponseForDataTask:"

-- | @Selector@ for @sharedURLCache@
sharedURLCacheSelector :: Selector
sharedURLCacheSelector = mkSelector "sharedURLCache"

-- | @Selector@ for @setSharedURLCache:@
setSharedURLCacheSelector :: Selector
setSharedURLCacheSelector = mkSelector "setSharedURLCache:"

-- | @Selector@ for @memoryCapacity@
memoryCapacitySelector :: Selector
memoryCapacitySelector = mkSelector "memoryCapacity"

-- | @Selector@ for @setMemoryCapacity:@
setMemoryCapacitySelector :: Selector
setMemoryCapacitySelector = mkSelector "setMemoryCapacity:"

-- | @Selector@ for @diskCapacity@
diskCapacitySelector :: Selector
diskCapacitySelector = mkSelector "diskCapacity"

-- | @Selector@ for @setDiskCapacity:@
setDiskCapacitySelector :: Selector
setDiskCapacitySelector = mkSelector "setDiskCapacity:"

-- | @Selector@ for @currentMemoryUsage@
currentMemoryUsageSelector :: Selector
currentMemoryUsageSelector = mkSelector "currentMemoryUsage"

-- | @Selector@ for @currentDiskUsage@
currentDiskUsageSelector :: Selector
currentDiskUsageSelector = mkSelector "currentDiskUsage"

