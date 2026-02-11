{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSNetService@.
module ObjC.Foundation.NSNetService
  ( NSNetService
  , IsNSNetService(..)
  , initWithDomain_type_name_port
  , initWithDomain_type_name
  , scheduleInRunLoop_forMode
  , removeFromRunLoop_forMode
  , publish
  , publishWithOptions
  , resolve
  , stop
  , dictionaryFromTXTRecordData
  , dataFromTXTRecordDictionary
  , resolveWithTimeout
  , getInputStream_outputStream
  , setTXTRecordData
  , txtRecordData
  , startMonitoring
  , stopMonitoring
  , delegate
  , setDelegate
  , includesPeerToPeer
  , setIncludesPeerToPeer
  , name
  , type_
  , domain
  , hostName
  , addresses
  , port
  , initWithDomain_type_name_portSelector
  , initWithDomain_type_nameSelector
  , scheduleInRunLoop_forModeSelector
  , removeFromRunLoop_forModeSelector
  , publishSelector
  , publishWithOptionsSelector
  , resolveSelector
  , stopSelector
  , dictionaryFromTXTRecordDataSelector
  , dataFromTXTRecordDictionarySelector
  , resolveWithTimeoutSelector
  , getInputStream_outputStreamSelector
  , setTXTRecordDataSelector
  , txtRecordDataSelector
  , startMonitoringSelector
  , stopMonitoringSelector
  , delegateSelector
  , setDelegateSelector
  , includesPeerToPeerSelector
  , setIncludesPeerToPeerSelector
  , nameSelector
  , typeSelector
  , domainSelector
  , hostNameSelector
  , addressesSelector
  , portSelector

  -- * Enum types
  , NSNetServiceOptions(NSNetServiceOptions)
  , pattern NSNetServiceNoAutoRename
  , pattern NSNetServiceListenForConnections

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

-- | @- initWithDomain:type:name:port:@
initWithDomain_type_name_port :: (IsNSNetService nsNetService, IsNSString domain, IsNSString type_, IsNSString name) => nsNetService -> domain -> type_ -> name -> CInt -> IO (Id NSNetService)
initWithDomain_type_name_port nsNetService  domain type_ name port =
  withObjCPtr domain $ \raw_domain ->
    withObjCPtr type_ $ \raw_type_ ->
      withObjCPtr name $ \raw_name ->
          sendMsg nsNetService (mkSelector "initWithDomain:type:name:port:") (retPtr retVoid) [argPtr (castPtr raw_domain :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argCInt port] >>= ownedObject . castPtr

-- | @- initWithDomain:type:name:@
initWithDomain_type_name :: (IsNSNetService nsNetService, IsNSString domain, IsNSString type_, IsNSString name) => nsNetService -> domain -> type_ -> name -> IO (Id NSNetService)
initWithDomain_type_name nsNetService  domain type_ name =
  withObjCPtr domain $ \raw_domain ->
    withObjCPtr type_ $ \raw_type_ ->
      withObjCPtr name $ \raw_name ->
          sendMsg nsNetService (mkSelector "initWithDomain:type:name:") (retPtr retVoid) [argPtr (castPtr raw_domain :: Ptr ()), argPtr (castPtr raw_type_ :: Ptr ()), argPtr (castPtr raw_name :: Ptr ())] >>= ownedObject . castPtr

-- | @- scheduleInRunLoop:forMode:@
scheduleInRunLoop_forMode :: (IsNSNetService nsNetService, IsNSRunLoop aRunLoop, IsNSString mode) => nsNetService -> aRunLoop -> mode -> IO ()
scheduleInRunLoop_forMode nsNetService  aRunLoop mode =
  withObjCPtr aRunLoop $ \raw_aRunLoop ->
    withObjCPtr mode $ \raw_mode ->
        sendMsg nsNetService (mkSelector "scheduleInRunLoop:forMode:") retVoid [argPtr (castPtr raw_aRunLoop :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ())]

-- | @- removeFromRunLoop:forMode:@
removeFromRunLoop_forMode :: (IsNSNetService nsNetService, IsNSRunLoop aRunLoop, IsNSString mode) => nsNetService -> aRunLoop -> mode -> IO ()
removeFromRunLoop_forMode nsNetService  aRunLoop mode =
  withObjCPtr aRunLoop $ \raw_aRunLoop ->
    withObjCPtr mode $ \raw_mode ->
        sendMsg nsNetService (mkSelector "removeFromRunLoop:forMode:") retVoid [argPtr (castPtr raw_aRunLoop :: Ptr ()), argPtr (castPtr raw_mode :: Ptr ())]

-- | @- publish@
publish :: IsNSNetService nsNetService => nsNetService -> IO ()
publish nsNetService  =
    sendMsg nsNetService (mkSelector "publish") retVoid []

-- | @- publishWithOptions:@
publishWithOptions :: IsNSNetService nsNetService => nsNetService -> NSNetServiceOptions -> IO ()
publishWithOptions nsNetService  options =
    sendMsg nsNetService (mkSelector "publishWithOptions:") retVoid [argCULong (coerce options)]

-- | @- resolve@
resolve :: IsNSNetService nsNetService => nsNetService -> IO ()
resolve nsNetService  =
    sendMsg nsNetService (mkSelector "resolve") retVoid []

-- | @- stop@
stop :: IsNSNetService nsNetService => nsNetService -> IO ()
stop nsNetService  =
    sendMsg nsNetService (mkSelector "stop") retVoid []

-- | @+ dictionaryFromTXTRecordData:@
dictionaryFromTXTRecordData :: IsNSData txtData => txtData -> IO (Id NSDictionary)
dictionaryFromTXTRecordData txtData =
  do
    cls' <- getRequiredClass "NSNetService"
    withObjCPtr txtData $ \raw_txtData ->
      sendClassMsg cls' (mkSelector "dictionaryFromTXTRecordData:") (retPtr retVoid) [argPtr (castPtr raw_txtData :: Ptr ())] >>= retainedObject . castPtr

-- | @+ dataFromTXTRecordDictionary:@
dataFromTXTRecordDictionary :: IsNSDictionary txtDictionary => txtDictionary -> IO (Id NSData)
dataFromTXTRecordDictionary txtDictionary =
  do
    cls' <- getRequiredClass "NSNetService"
    withObjCPtr txtDictionary $ \raw_txtDictionary ->
      sendClassMsg cls' (mkSelector "dataFromTXTRecordDictionary:") (retPtr retVoid) [argPtr (castPtr raw_txtDictionary :: Ptr ())] >>= retainedObject . castPtr

-- | @- resolveWithTimeout:@
resolveWithTimeout :: IsNSNetService nsNetService => nsNetService -> CDouble -> IO ()
resolveWithTimeout nsNetService  timeout =
    sendMsg nsNetService (mkSelector "resolveWithTimeout:") retVoid [argCDouble timeout]

-- | @- getInputStream:outputStream:@
getInputStream_outputStream :: (IsNSNetService nsNetService, IsNSInputStream inputStream, IsNSOutputStream outputStream) => nsNetService -> inputStream -> outputStream -> IO Bool
getInputStream_outputStream nsNetService  inputStream outputStream =
  withObjCPtr inputStream $ \raw_inputStream ->
    withObjCPtr outputStream $ \raw_outputStream ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsNetService (mkSelector "getInputStream:outputStream:") retCULong [argPtr (castPtr raw_inputStream :: Ptr ()), argPtr (castPtr raw_outputStream :: Ptr ())]

-- | @- setTXTRecordData:@
setTXTRecordData :: (IsNSNetService nsNetService, IsNSData recordData) => nsNetService -> recordData -> IO Bool
setTXTRecordData nsNetService  recordData =
  withObjCPtr recordData $ \raw_recordData ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsNetService (mkSelector "setTXTRecordData:") retCULong [argPtr (castPtr raw_recordData :: Ptr ())]

-- | @- TXTRecordData@
txtRecordData :: IsNSNetService nsNetService => nsNetService -> IO (Id NSData)
txtRecordData nsNetService  =
    sendMsg nsNetService (mkSelector "TXTRecordData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- startMonitoring@
startMonitoring :: IsNSNetService nsNetService => nsNetService -> IO ()
startMonitoring nsNetService  =
    sendMsg nsNetService (mkSelector "startMonitoring") retVoid []

-- | @- stopMonitoring@
stopMonitoring :: IsNSNetService nsNetService => nsNetService -> IO ()
stopMonitoring nsNetService  =
    sendMsg nsNetService (mkSelector "stopMonitoring") retVoid []

-- | @- delegate@
delegate :: IsNSNetService nsNetService => nsNetService -> IO RawId
delegate nsNetService  =
    fmap (RawId . castPtr) $ sendMsg nsNetService (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSNetService nsNetService => nsNetService -> RawId -> IO ()
setDelegate nsNetService  value =
    sendMsg nsNetService (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- includesPeerToPeer@
includesPeerToPeer :: IsNSNetService nsNetService => nsNetService -> IO Bool
includesPeerToPeer nsNetService  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsNetService (mkSelector "includesPeerToPeer") retCULong []

-- | @- setIncludesPeerToPeer:@
setIncludesPeerToPeer :: IsNSNetService nsNetService => nsNetService -> Bool -> IO ()
setIncludesPeerToPeer nsNetService  value =
    sendMsg nsNetService (mkSelector "setIncludesPeerToPeer:") retVoid [argCULong (if value then 1 else 0)]

-- | @- name@
name :: IsNSNetService nsNetService => nsNetService -> IO (Id NSString)
name nsNetService  =
    sendMsg nsNetService (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- type@
type_ :: IsNSNetService nsNetService => nsNetService -> IO (Id NSString)
type_ nsNetService  =
    sendMsg nsNetService (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- domain@
domain :: IsNSNetService nsNetService => nsNetService -> IO (Id NSString)
domain nsNetService  =
    sendMsg nsNetService (mkSelector "domain") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- hostName@
hostName :: IsNSNetService nsNetService => nsNetService -> IO (Id NSString)
hostName nsNetService  =
    sendMsg nsNetService (mkSelector "hostName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- addresses@
addresses :: IsNSNetService nsNetService => nsNetService -> IO (Id NSArray)
addresses nsNetService  =
    sendMsg nsNetService (mkSelector "addresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- port@
port :: IsNSNetService nsNetService => nsNetService -> IO CLong
port nsNetService  =
    sendMsg nsNetService (mkSelector "port") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDomain:type:name:port:@
initWithDomain_type_name_portSelector :: Selector
initWithDomain_type_name_portSelector = mkSelector "initWithDomain:type:name:port:"

-- | @Selector@ for @initWithDomain:type:name:@
initWithDomain_type_nameSelector :: Selector
initWithDomain_type_nameSelector = mkSelector "initWithDomain:type:name:"

-- | @Selector@ for @scheduleInRunLoop:forMode:@
scheduleInRunLoop_forModeSelector :: Selector
scheduleInRunLoop_forModeSelector = mkSelector "scheduleInRunLoop:forMode:"

-- | @Selector@ for @removeFromRunLoop:forMode:@
removeFromRunLoop_forModeSelector :: Selector
removeFromRunLoop_forModeSelector = mkSelector "removeFromRunLoop:forMode:"

-- | @Selector@ for @publish@
publishSelector :: Selector
publishSelector = mkSelector "publish"

-- | @Selector@ for @publishWithOptions:@
publishWithOptionsSelector :: Selector
publishWithOptionsSelector = mkSelector "publishWithOptions:"

-- | @Selector@ for @resolve@
resolveSelector :: Selector
resolveSelector = mkSelector "resolve"

-- | @Selector@ for @stop@
stopSelector :: Selector
stopSelector = mkSelector "stop"

-- | @Selector@ for @dictionaryFromTXTRecordData:@
dictionaryFromTXTRecordDataSelector :: Selector
dictionaryFromTXTRecordDataSelector = mkSelector "dictionaryFromTXTRecordData:"

-- | @Selector@ for @dataFromTXTRecordDictionary:@
dataFromTXTRecordDictionarySelector :: Selector
dataFromTXTRecordDictionarySelector = mkSelector "dataFromTXTRecordDictionary:"

-- | @Selector@ for @resolveWithTimeout:@
resolveWithTimeoutSelector :: Selector
resolveWithTimeoutSelector = mkSelector "resolveWithTimeout:"

-- | @Selector@ for @getInputStream:outputStream:@
getInputStream_outputStreamSelector :: Selector
getInputStream_outputStreamSelector = mkSelector "getInputStream:outputStream:"

-- | @Selector@ for @setTXTRecordData:@
setTXTRecordDataSelector :: Selector
setTXTRecordDataSelector = mkSelector "setTXTRecordData:"

-- | @Selector@ for @TXTRecordData@
txtRecordDataSelector :: Selector
txtRecordDataSelector = mkSelector "TXTRecordData"

-- | @Selector@ for @startMonitoring@
startMonitoringSelector :: Selector
startMonitoringSelector = mkSelector "startMonitoring"

-- | @Selector@ for @stopMonitoring@
stopMonitoringSelector :: Selector
stopMonitoringSelector = mkSelector "stopMonitoring"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @includesPeerToPeer@
includesPeerToPeerSelector :: Selector
includesPeerToPeerSelector = mkSelector "includesPeerToPeer"

-- | @Selector@ for @setIncludesPeerToPeer:@
setIncludesPeerToPeerSelector :: Selector
setIncludesPeerToPeerSelector = mkSelector "setIncludesPeerToPeer:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @domain@
domainSelector :: Selector
domainSelector = mkSelector "domain"

-- | @Selector@ for @hostName@
hostNameSelector :: Selector
hostNameSelector = mkSelector "hostName"

-- | @Selector@ for @addresses@
addressesSelector :: Selector
addressesSelector = mkSelector "addresses"

-- | @Selector@ for @port@
portSelector :: Selector
portSelector = mkSelector "port"

