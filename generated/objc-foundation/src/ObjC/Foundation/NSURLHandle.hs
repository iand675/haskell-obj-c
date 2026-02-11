{-# LANGUAGE PatternSynonyms #-}
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
  , registerURLHandleClassSelector
  , urlHandleClassForURLSelector
  , statusSelector
  , failureReasonSelector
  , addClientSelector
  , removeClientSelector
  , loadInBackgroundSelector
  , cancelLoadInBackgroundSelector
  , resourceDataSelector
  , availableResourceDataSelector
  , expectedResourceDataSizeSelector
  , flushCachedDataSelector
  , backgroundLoadDidFailWithReasonSelector
  , didLoadBytes_loadCompleteSelector
  , canInitWithURLSelector
  , cachedHandleForURLSelector
  , initWithURL_cachedSelector
  , propertyForKeySelector
  , propertyForKeyIfAvailableSelector
  , writeProperty_forKeySelector
  , writeDataSelector
  , loadInForegroundSelector
  , beginLoadInBackgroundSelector
  , endLoadInBackgroundSelector

  -- * Enum types
  , NSURLHandleStatus(NSURLHandleStatus)
  , pattern NSURLHandleNotLoaded
  , pattern NSURLHandleLoadSucceeded
  , pattern NSURLHandleLoadInProgress
  , pattern NSURLHandleLoadFailed

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

-- | @+ registerURLHandleClass:@
registerURLHandleClass :: Class -> IO ()
registerURLHandleClass anURLHandleSubclass =
  do
    cls' <- getRequiredClass "NSURLHandle"
    sendClassMsg cls' (mkSelector "registerURLHandleClass:") retVoid [argPtr (unClass anURLHandleSubclass)]

-- | @+ URLHandleClassForURL:@
urlHandleClassForURL :: IsNSURL anURL => anURL -> IO Class
urlHandleClassForURL anURL =
  do
    cls' <- getRequiredClass "NSURLHandle"
    withObjCPtr anURL $ \raw_anURL ->
      fmap (Class . castPtr) $ sendClassMsg cls' (mkSelector "URLHandleClassForURL:") (retPtr retVoid) [argPtr (castPtr raw_anURL :: Ptr ())]

-- | @- status@
status :: IsNSURLHandle nsurlHandle => nsurlHandle -> IO NSURLHandleStatus
status nsurlHandle  =
  fmap (coerce :: CULong -> NSURLHandleStatus) $ sendMsg nsurlHandle (mkSelector "status") retCULong []

-- | @- failureReason@
failureReason :: IsNSURLHandle nsurlHandle => nsurlHandle -> IO (Id NSString)
failureReason nsurlHandle  =
  sendMsg nsurlHandle (mkSelector "failureReason") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- addClient:@
addClient :: IsNSURLHandle nsurlHandle => nsurlHandle -> RawId -> IO ()
addClient nsurlHandle  client =
  sendMsg nsurlHandle (mkSelector "addClient:") retVoid [argPtr (castPtr (unRawId client) :: Ptr ())]

-- | @- removeClient:@
removeClient :: IsNSURLHandle nsurlHandle => nsurlHandle -> RawId -> IO ()
removeClient nsurlHandle  client =
  sendMsg nsurlHandle (mkSelector "removeClient:") retVoid [argPtr (castPtr (unRawId client) :: Ptr ())]

-- | @- loadInBackground@
loadInBackground :: IsNSURLHandle nsurlHandle => nsurlHandle -> IO ()
loadInBackground nsurlHandle  =
  sendMsg nsurlHandle (mkSelector "loadInBackground") retVoid []

-- | @- cancelLoadInBackground@
cancelLoadInBackground :: IsNSURLHandle nsurlHandle => nsurlHandle -> IO ()
cancelLoadInBackground nsurlHandle  =
  sendMsg nsurlHandle (mkSelector "cancelLoadInBackground") retVoid []

-- | @- resourceData@
resourceData :: IsNSURLHandle nsurlHandle => nsurlHandle -> IO (Id NSData)
resourceData nsurlHandle  =
  sendMsg nsurlHandle (mkSelector "resourceData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- availableResourceData@
availableResourceData :: IsNSURLHandle nsurlHandle => nsurlHandle -> IO (Id NSData)
availableResourceData nsurlHandle  =
  sendMsg nsurlHandle (mkSelector "availableResourceData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- expectedResourceDataSize@
expectedResourceDataSize :: IsNSURLHandle nsurlHandle => nsurlHandle -> IO CLong
expectedResourceDataSize nsurlHandle  =
  sendMsg nsurlHandle (mkSelector "expectedResourceDataSize") retCLong []

-- | @- flushCachedData@
flushCachedData :: IsNSURLHandle nsurlHandle => nsurlHandle -> IO ()
flushCachedData nsurlHandle  =
  sendMsg nsurlHandle (mkSelector "flushCachedData") retVoid []

-- | @- backgroundLoadDidFailWithReason:@
backgroundLoadDidFailWithReason :: (IsNSURLHandle nsurlHandle, IsNSString reason) => nsurlHandle -> reason -> IO ()
backgroundLoadDidFailWithReason nsurlHandle  reason =
withObjCPtr reason $ \raw_reason ->
    sendMsg nsurlHandle (mkSelector "backgroundLoadDidFailWithReason:") retVoid [argPtr (castPtr raw_reason :: Ptr ())]

-- | @- didLoadBytes:loadComplete:@
didLoadBytes_loadComplete :: (IsNSURLHandle nsurlHandle, IsNSData newBytes) => nsurlHandle -> newBytes -> Bool -> IO ()
didLoadBytes_loadComplete nsurlHandle  newBytes yorn =
withObjCPtr newBytes $ \raw_newBytes ->
    sendMsg nsurlHandle (mkSelector "didLoadBytes:loadComplete:") retVoid [argPtr (castPtr raw_newBytes :: Ptr ()), argCULong (if yorn then 1 else 0)]

-- | @+ canInitWithURL:@
canInitWithURL :: IsNSURL anURL => anURL -> IO Bool
canInitWithURL anURL =
  do
    cls' <- getRequiredClass "NSURLHandle"
    withObjCPtr anURL $ \raw_anURL ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "canInitWithURL:") retCULong [argPtr (castPtr raw_anURL :: Ptr ())]

-- | @+ cachedHandleForURL:@
cachedHandleForURL :: IsNSURL anURL => anURL -> IO (Id NSURLHandle)
cachedHandleForURL anURL =
  do
    cls' <- getRequiredClass "NSURLHandle"
    withObjCPtr anURL $ \raw_anURL ->
      sendClassMsg cls' (mkSelector "cachedHandleForURL:") (retPtr retVoid) [argPtr (castPtr raw_anURL :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithURL:cached:@
initWithURL_cached :: (IsNSURLHandle nsurlHandle, IsNSURL anURL) => nsurlHandle -> anURL -> Bool -> IO RawId
initWithURL_cached nsurlHandle  anURL willCache =
withObjCPtr anURL $ \raw_anURL ->
    fmap (RawId . castPtr) $ sendMsg nsurlHandle (mkSelector "initWithURL:cached:") (retPtr retVoid) [argPtr (castPtr raw_anURL :: Ptr ()), argCULong (if willCache then 1 else 0)]

-- | @- propertyForKey:@
propertyForKey :: (IsNSURLHandle nsurlHandle, IsNSString propertyKey) => nsurlHandle -> propertyKey -> IO RawId
propertyForKey nsurlHandle  propertyKey =
withObjCPtr propertyKey $ \raw_propertyKey ->
    fmap (RawId . castPtr) $ sendMsg nsurlHandle (mkSelector "propertyForKey:") (retPtr retVoid) [argPtr (castPtr raw_propertyKey :: Ptr ())]

-- | @- propertyForKeyIfAvailable:@
propertyForKeyIfAvailable :: (IsNSURLHandle nsurlHandle, IsNSString propertyKey) => nsurlHandle -> propertyKey -> IO RawId
propertyForKeyIfAvailable nsurlHandle  propertyKey =
withObjCPtr propertyKey $ \raw_propertyKey ->
    fmap (RawId . castPtr) $ sendMsg nsurlHandle (mkSelector "propertyForKeyIfAvailable:") (retPtr retVoid) [argPtr (castPtr raw_propertyKey :: Ptr ())]

-- | @- writeProperty:forKey:@
writeProperty_forKey :: (IsNSURLHandle nsurlHandle, IsNSString propertyKey) => nsurlHandle -> RawId -> propertyKey -> IO Bool
writeProperty_forKey nsurlHandle  propertyValue propertyKey =
withObjCPtr propertyKey $ \raw_propertyKey ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlHandle (mkSelector "writeProperty:forKey:") retCULong [argPtr (castPtr (unRawId propertyValue) :: Ptr ()), argPtr (castPtr raw_propertyKey :: Ptr ())]

-- | @- writeData:@
writeData :: (IsNSURLHandle nsurlHandle, IsNSData data_) => nsurlHandle -> data_ -> IO Bool
writeData nsurlHandle  data_ =
withObjCPtr data_ $ \raw_data_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsurlHandle (mkSelector "writeData:") retCULong [argPtr (castPtr raw_data_ :: Ptr ())]

-- | @- loadInForeground@
loadInForeground :: IsNSURLHandle nsurlHandle => nsurlHandle -> IO (Id NSData)
loadInForeground nsurlHandle  =
  sendMsg nsurlHandle (mkSelector "loadInForeground") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- beginLoadInBackground@
beginLoadInBackground :: IsNSURLHandle nsurlHandle => nsurlHandle -> IO ()
beginLoadInBackground nsurlHandle  =
  sendMsg nsurlHandle (mkSelector "beginLoadInBackground") retVoid []

-- | @- endLoadInBackground@
endLoadInBackground :: IsNSURLHandle nsurlHandle => nsurlHandle -> IO ()
endLoadInBackground nsurlHandle  =
  sendMsg nsurlHandle (mkSelector "endLoadInBackground") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerURLHandleClass:@
registerURLHandleClassSelector :: Selector
registerURLHandleClassSelector = mkSelector "registerURLHandleClass:"

-- | @Selector@ for @URLHandleClassForURL:@
urlHandleClassForURLSelector :: Selector
urlHandleClassForURLSelector = mkSelector "URLHandleClassForURL:"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @failureReason@
failureReasonSelector :: Selector
failureReasonSelector = mkSelector "failureReason"

-- | @Selector@ for @addClient:@
addClientSelector :: Selector
addClientSelector = mkSelector "addClient:"

-- | @Selector@ for @removeClient:@
removeClientSelector :: Selector
removeClientSelector = mkSelector "removeClient:"

-- | @Selector@ for @loadInBackground@
loadInBackgroundSelector :: Selector
loadInBackgroundSelector = mkSelector "loadInBackground"

-- | @Selector@ for @cancelLoadInBackground@
cancelLoadInBackgroundSelector :: Selector
cancelLoadInBackgroundSelector = mkSelector "cancelLoadInBackground"

-- | @Selector@ for @resourceData@
resourceDataSelector :: Selector
resourceDataSelector = mkSelector "resourceData"

-- | @Selector@ for @availableResourceData@
availableResourceDataSelector :: Selector
availableResourceDataSelector = mkSelector "availableResourceData"

-- | @Selector@ for @expectedResourceDataSize@
expectedResourceDataSizeSelector :: Selector
expectedResourceDataSizeSelector = mkSelector "expectedResourceDataSize"

-- | @Selector@ for @flushCachedData@
flushCachedDataSelector :: Selector
flushCachedDataSelector = mkSelector "flushCachedData"

-- | @Selector@ for @backgroundLoadDidFailWithReason:@
backgroundLoadDidFailWithReasonSelector :: Selector
backgroundLoadDidFailWithReasonSelector = mkSelector "backgroundLoadDidFailWithReason:"

-- | @Selector@ for @didLoadBytes:loadComplete:@
didLoadBytes_loadCompleteSelector :: Selector
didLoadBytes_loadCompleteSelector = mkSelector "didLoadBytes:loadComplete:"

-- | @Selector@ for @canInitWithURL:@
canInitWithURLSelector :: Selector
canInitWithURLSelector = mkSelector "canInitWithURL:"

-- | @Selector@ for @cachedHandleForURL:@
cachedHandleForURLSelector :: Selector
cachedHandleForURLSelector = mkSelector "cachedHandleForURL:"

-- | @Selector@ for @initWithURL:cached:@
initWithURL_cachedSelector :: Selector
initWithURL_cachedSelector = mkSelector "initWithURL:cached:"

-- | @Selector@ for @propertyForKey:@
propertyForKeySelector :: Selector
propertyForKeySelector = mkSelector "propertyForKey:"

-- | @Selector@ for @propertyForKeyIfAvailable:@
propertyForKeyIfAvailableSelector :: Selector
propertyForKeyIfAvailableSelector = mkSelector "propertyForKeyIfAvailable:"

-- | @Selector@ for @writeProperty:forKey:@
writeProperty_forKeySelector :: Selector
writeProperty_forKeySelector = mkSelector "writeProperty:forKey:"

-- | @Selector@ for @writeData:@
writeDataSelector :: Selector
writeDataSelector = mkSelector "writeData:"

-- | @Selector@ for @loadInForeground@
loadInForegroundSelector :: Selector
loadInForegroundSelector = mkSelector "loadInForeground"

-- | @Selector@ for @beginLoadInBackground@
beginLoadInBackgroundSelector :: Selector
beginLoadInBackgroundSelector = mkSelector "beginLoadInBackground"

-- | @Selector@ for @endLoadInBackground@
endLoadInBackgroundSelector :: Selector
endLoadInBackgroundSelector = mkSelector "endLoadInBackground"

