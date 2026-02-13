{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSURLHandle@.
module ObjC.Foundation.NSURLHandle
  ( NSURLHandle
  , IsNSURLHandle(..)
  , registerURLHandleClass
  , urlHandleClassForURL
  , status
  , failureReason
  , addClient
  , removeClient
  , loadInBackground
  , cancelLoadInBackground
  , resourceData
  , availableResourceData
  , expectedResourceDataSize
  , flushCachedData
  , backgroundLoadDidFailWithReason
  , didLoadBytes_loadComplete
  , canInitWithURL
  , cachedHandleForURL
  , initWithURL_cached
  , propertyForKey
  , propertyForKeyIfAvailable
  , writeProperty_forKey
  , writeData
  , loadInForeground
  , beginLoadInBackground
  , endLoadInBackground
  , addClientSelector
  , availableResourceDataSelector
  , backgroundLoadDidFailWithReasonSelector
  , beginLoadInBackgroundSelector
  , cachedHandleForURLSelector
  , canInitWithURLSelector
  , cancelLoadInBackgroundSelector
  , didLoadBytes_loadCompleteSelector
  , endLoadInBackgroundSelector
  , expectedResourceDataSizeSelector
  , failureReasonSelector
  , flushCachedDataSelector
  , initWithURL_cachedSelector
  , loadInBackgroundSelector
  , loadInForegroundSelector
  , propertyForKeyIfAvailableSelector
  , propertyForKeySelector
  , registerURLHandleClassSelector
  , removeClientSelector
  , resourceDataSelector
  , statusSelector
  , urlHandleClassForURLSelector
  , writeDataSelector
  , writeProperty_forKeySelector

  -- * Enum types
  , NSURLHandleStatus(NSURLHandleStatus)
  , pattern NSURLHandleNotLoaded
  , pattern NSURLHandleLoadSucceeded
  , pattern NSURLHandleLoadInProgress
  , pattern NSURLHandleLoadFailed

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @+ registerURLHandleClass:@
registerURLHandleClass :: Class -> IO ()
registerURLHandleClass anURLHandleSubclass =
  do
    cls' <- getRequiredClass "NSURLHandle"
    sendClassMessage cls' registerURLHandleClassSelector anURLHandleSubclass

-- | @+ URLHandleClassForURL:@
urlHandleClassForURL :: IsNSURL anURL => anURL -> IO Class
urlHandleClassForURL anURL =
  do
    cls' <- getRequiredClass "NSURLHandle"
    sendClassMessage cls' urlHandleClassForURLSelector (toNSURL anURL)

-- | @- status@
status :: IsNSURLHandle nsurlHandle => nsurlHandle -> IO NSURLHandleStatus
status nsurlHandle =
  sendMessage nsurlHandle statusSelector

-- | @- failureReason@
failureReason :: IsNSURLHandle nsurlHandle => nsurlHandle -> IO (Id NSString)
failureReason nsurlHandle =
  sendMessage nsurlHandle failureReasonSelector

-- | @- addClient:@
addClient :: IsNSURLHandle nsurlHandle => nsurlHandle -> RawId -> IO ()
addClient nsurlHandle client =
  sendMessage nsurlHandle addClientSelector client

-- | @- removeClient:@
removeClient :: IsNSURLHandle nsurlHandle => nsurlHandle -> RawId -> IO ()
removeClient nsurlHandle client =
  sendMessage nsurlHandle removeClientSelector client

-- | @- loadInBackground@
loadInBackground :: IsNSURLHandle nsurlHandle => nsurlHandle -> IO ()
loadInBackground nsurlHandle =
  sendMessage nsurlHandle loadInBackgroundSelector

-- | @- cancelLoadInBackground@
cancelLoadInBackground :: IsNSURLHandle nsurlHandle => nsurlHandle -> IO ()
cancelLoadInBackground nsurlHandle =
  sendMessage nsurlHandle cancelLoadInBackgroundSelector

-- | @- resourceData@
resourceData :: IsNSURLHandle nsurlHandle => nsurlHandle -> IO (Id NSData)
resourceData nsurlHandle =
  sendMessage nsurlHandle resourceDataSelector

-- | @- availableResourceData@
availableResourceData :: IsNSURLHandle nsurlHandle => nsurlHandle -> IO (Id NSData)
availableResourceData nsurlHandle =
  sendMessage nsurlHandle availableResourceDataSelector

-- | @- expectedResourceDataSize@
expectedResourceDataSize :: IsNSURLHandle nsurlHandle => nsurlHandle -> IO CLong
expectedResourceDataSize nsurlHandle =
  sendMessage nsurlHandle expectedResourceDataSizeSelector

-- | @- flushCachedData@
flushCachedData :: IsNSURLHandle nsurlHandle => nsurlHandle -> IO ()
flushCachedData nsurlHandle =
  sendMessage nsurlHandle flushCachedDataSelector

-- | @- backgroundLoadDidFailWithReason:@
backgroundLoadDidFailWithReason :: (IsNSURLHandle nsurlHandle, IsNSString reason) => nsurlHandle -> reason -> IO ()
backgroundLoadDidFailWithReason nsurlHandle reason =
  sendMessage nsurlHandle backgroundLoadDidFailWithReasonSelector (toNSString reason)

-- | @- didLoadBytes:loadComplete:@
didLoadBytes_loadComplete :: (IsNSURLHandle nsurlHandle, IsNSData newBytes) => nsurlHandle -> newBytes -> Bool -> IO ()
didLoadBytes_loadComplete nsurlHandle newBytes yorn =
  sendMessage nsurlHandle didLoadBytes_loadCompleteSelector (toNSData newBytes) yorn

-- | @+ canInitWithURL:@
canInitWithURL :: IsNSURL anURL => anURL -> IO Bool
canInitWithURL anURL =
  do
    cls' <- getRequiredClass "NSURLHandle"
    sendClassMessage cls' canInitWithURLSelector (toNSURL anURL)

-- | @+ cachedHandleForURL:@
cachedHandleForURL :: IsNSURL anURL => anURL -> IO (Id NSURLHandle)
cachedHandleForURL anURL =
  do
    cls' <- getRequiredClass "NSURLHandle"
    sendClassMessage cls' cachedHandleForURLSelector (toNSURL anURL)

-- | @- initWithURL:cached:@
initWithURL_cached :: (IsNSURLHandle nsurlHandle, IsNSURL anURL) => nsurlHandle -> anURL -> Bool -> IO RawId
initWithURL_cached nsurlHandle anURL willCache =
  sendOwnedMessage nsurlHandle initWithURL_cachedSelector (toNSURL anURL) willCache

-- | @- propertyForKey:@
propertyForKey :: (IsNSURLHandle nsurlHandle, IsNSString propertyKey) => nsurlHandle -> propertyKey -> IO RawId
propertyForKey nsurlHandle propertyKey =
  sendMessage nsurlHandle propertyForKeySelector (toNSString propertyKey)

-- | @- propertyForKeyIfAvailable:@
propertyForKeyIfAvailable :: (IsNSURLHandle nsurlHandle, IsNSString propertyKey) => nsurlHandle -> propertyKey -> IO RawId
propertyForKeyIfAvailable nsurlHandle propertyKey =
  sendMessage nsurlHandle propertyForKeyIfAvailableSelector (toNSString propertyKey)

-- | @- writeProperty:forKey:@
writeProperty_forKey :: (IsNSURLHandle nsurlHandle, IsNSString propertyKey) => nsurlHandle -> RawId -> propertyKey -> IO Bool
writeProperty_forKey nsurlHandle propertyValue propertyKey =
  sendMessage nsurlHandle writeProperty_forKeySelector propertyValue (toNSString propertyKey)

-- | @- writeData:@
writeData :: (IsNSURLHandle nsurlHandle, IsNSData data_) => nsurlHandle -> data_ -> IO Bool
writeData nsurlHandle data_ =
  sendMessage nsurlHandle writeDataSelector (toNSData data_)

-- | @- loadInForeground@
loadInForeground :: IsNSURLHandle nsurlHandle => nsurlHandle -> IO (Id NSData)
loadInForeground nsurlHandle =
  sendMessage nsurlHandle loadInForegroundSelector

-- | @- beginLoadInBackground@
beginLoadInBackground :: IsNSURLHandle nsurlHandle => nsurlHandle -> IO ()
beginLoadInBackground nsurlHandle =
  sendMessage nsurlHandle beginLoadInBackgroundSelector

-- | @- endLoadInBackground@
endLoadInBackground :: IsNSURLHandle nsurlHandle => nsurlHandle -> IO ()
endLoadInBackground nsurlHandle =
  sendMessage nsurlHandle endLoadInBackgroundSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerURLHandleClass:@
registerURLHandleClassSelector :: Selector '[Class] ()
registerURLHandleClassSelector = mkSelector "registerURLHandleClass:"

-- | @Selector@ for @URLHandleClassForURL:@
urlHandleClassForURLSelector :: Selector '[Id NSURL] Class
urlHandleClassForURLSelector = mkSelector "URLHandleClassForURL:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] NSURLHandleStatus
statusSelector = mkSelector "status"

-- | @Selector@ for @failureReason@
failureReasonSelector :: Selector '[] (Id NSString)
failureReasonSelector = mkSelector "failureReason"

-- | @Selector@ for @addClient:@
addClientSelector :: Selector '[RawId] ()
addClientSelector = mkSelector "addClient:"

-- | @Selector@ for @removeClient:@
removeClientSelector :: Selector '[RawId] ()
removeClientSelector = mkSelector "removeClient:"

-- | @Selector@ for @loadInBackground@
loadInBackgroundSelector :: Selector '[] ()
loadInBackgroundSelector = mkSelector "loadInBackground"

-- | @Selector@ for @cancelLoadInBackground@
cancelLoadInBackgroundSelector :: Selector '[] ()
cancelLoadInBackgroundSelector = mkSelector "cancelLoadInBackground"

-- | @Selector@ for @resourceData@
resourceDataSelector :: Selector '[] (Id NSData)
resourceDataSelector = mkSelector "resourceData"

-- | @Selector@ for @availableResourceData@
availableResourceDataSelector :: Selector '[] (Id NSData)
availableResourceDataSelector = mkSelector "availableResourceData"

-- | @Selector@ for @expectedResourceDataSize@
expectedResourceDataSizeSelector :: Selector '[] CLong
expectedResourceDataSizeSelector = mkSelector "expectedResourceDataSize"

-- | @Selector@ for @flushCachedData@
flushCachedDataSelector :: Selector '[] ()
flushCachedDataSelector = mkSelector "flushCachedData"

-- | @Selector@ for @backgroundLoadDidFailWithReason:@
backgroundLoadDidFailWithReasonSelector :: Selector '[Id NSString] ()
backgroundLoadDidFailWithReasonSelector = mkSelector "backgroundLoadDidFailWithReason:"

-- | @Selector@ for @didLoadBytes:loadComplete:@
didLoadBytes_loadCompleteSelector :: Selector '[Id NSData, Bool] ()
didLoadBytes_loadCompleteSelector = mkSelector "didLoadBytes:loadComplete:"

-- | @Selector@ for @canInitWithURL:@
canInitWithURLSelector :: Selector '[Id NSURL] Bool
canInitWithURLSelector = mkSelector "canInitWithURL:"

-- | @Selector@ for @cachedHandleForURL:@
cachedHandleForURLSelector :: Selector '[Id NSURL] (Id NSURLHandle)
cachedHandleForURLSelector = mkSelector "cachedHandleForURL:"

-- | @Selector@ for @initWithURL:cached:@
initWithURL_cachedSelector :: Selector '[Id NSURL, Bool] RawId
initWithURL_cachedSelector = mkSelector "initWithURL:cached:"

-- | @Selector@ for @propertyForKey:@
propertyForKeySelector :: Selector '[Id NSString] RawId
propertyForKeySelector = mkSelector "propertyForKey:"

-- | @Selector@ for @propertyForKeyIfAvailable:@
propertyForKeyIfAvailableSelector :: Selector '[Id NSString] RawId
propertyForKeyIfAvailableSelector = mkSelector "propertyForKeyIfAvailable:"

-- | @Selector@ for @writeProperty:forKey:@
writeProperty_forKeySelector :: Selector '[RawId, Id NSString] Bool
writeProperty_forKeySelector = mkSelector "writeProperty:forKey:"

-- | @Selector@ for @writeData:@
writeDataSelector :: Selector '[Id NSData] Bool
writeDataSelector = mkSelector "writeData:"

-- | @Selector@ for @loadInForeground@
loadInForegroundSelector :: Selector '[] (Id NSData)
loadInForegroundSelector = mkSelector "loadInForeground"

-- | @Selector@ for @beginLoadInBackground@
beginLoadInBackgroundSelector :: Selector '[] ()
beginLoadInBackgroundSelector = mkSelector "beginLoadInBackground"

-- | @Selector@ for @endLoadInBackground@
endLoadInBackgroundSelector :: Selector '[] ()
endLoadInBackgroundSelector = mkSelector "endLoadInBackground"

