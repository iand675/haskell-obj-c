{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , addressesSelector
  , dataFromTXTRecordDictionarySelector
  , delegateSelector
  , dictionaryFromTXTRecordDataSelector
  , domainSelector
  , getInputStream_outputStreamSelector
  , hostNameSelector
  , includesPeerToPeerSelector
  , initWithDomain_type_nameSelector
  , initWithDomain_type_name_portSelector
  , nameSelector
  , portSelector
  , publishSelector
  , publishWithOptionsSelector
  , removeFromRunLoop_forModeSelector
  , resolveSelector
  , resolveWithTimeoutSelector
  , scheduleInRunLoop_forModeSelector
  , setDelegateSelector
  , setIncludesPeerToPeerSelector
  , setTXTRecordDataSelector
  , startMonitoringSelector
  , stopMonitoringSelector
  , stopSelector
  , txtRecordDataSelector
  , typeSelector

  -- * Enum types
  , NSNetServiceOptions(NSNetServiceOptions)
  , pattern NSNetServiceNoAutoRename
  , pattern NSNetServiceListenForConnections

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- initWithDomain:type:name:port:@
initWithDomain_type_name_port :: (IsNSNetService nsNetService, IsNSString domain, IsNSString type_, IsNSString name) => nsNetService -> domain -> type_ -> name -> CInt -> IO (Id NSNetService)
initWithDomain_type_name_port nsNetService domain type_ name port =
  sendOwnedMessage nsNetService initWithDomain_type_name_portSelector (toNSString domain) (toNSString type_) (toNSString name) port

-- | @- initWithDomain:type:name:@
initWithDomain_type_name :: (IsNSNetService nsNetService, IsNSString domain, IsNSString type_, IsNSString name) => nsNetService -> domain -> type_ -> name -> IO (Id NSNetService)
initWithDomain_type_name nsNetService domain type_ name =
  sendOwnedMessage nsNetService initWithDomain_type_nameSelector (toNSString domain) (toNSString type_) (toNSString name)

-- | @- scheduleInRunLoop:forMode:@
scheduleInRunLoop_forMode :: (IsNSNetService nsNetService, IsNSRunLoop aRunLoop, IsNSString mode) => nsNetService -> aRunLoop -> mode -> IO ()
scheduleInRunLoop_forMode nsNetService aRunLoop mode =
  sendMessage nsNetService scheduleInRunLoop_forModeSelector (toNSRunLoop aRunLoop) (toNSString mode)

-- | @- removeFromRunLoop:forMode:@
removeFromRunLoop_forMode :: (IsNSNetService nsNetService, IsNSRunLoop aRunLoop, IsNSString mode) => nsNetService -> aRunLoop -> mode -> IO ()
removeFromRunLoop_forMode nsNetService aRunLoop mode =
  sendMessage nsNetService removeFromRunLoop_forModeSelector (toNSRunLoop aRunLoop) (toNSString mode)

-- | @- publish@
publish :: IsNSNetService nsNetService => nsNetService -> IO ()
publish nsNetService =
  sendMessage nsNetService publishSelector

-- | @- publishWithOptions:@
publishWithOptions :: IsNSNetService nsNetService => nsNetService -> NSNetServiceOptions -> IO ()
publishWithOptions nsNetService options =
  sendMessage nsNetService publishWithOptionsSelector options

-- | @- resolve@
resolve :: IsNSNetService nsNetService => nsNetService -> IO ()
resolve nsNetService =
  sendMessage nsNetService resolveSelector

-- | @- stop@
stop :: IsNSNetService nsNetService => nsNetService -> IO ()
stop nsNetService =
  sendMessage nsNetService stopSelector

-- | @+ dictionaryFromTXTRecordData:@
dictionaryFromTXTRecordData :: IsNSData txtData => txtData -> IO (Id NSDictionary)
dictionaryFromTXTRecordData txtData =
  do
    cls' <- getRequiredClass "NSNetService"
    sendClassMessage cls' dictionaryFromTXTRecordDataSelector (toNSData txtData)

-- | @+ dataFromTXTRecordDictionary:@
dataFromTXTRecordDictionary :: IsNSDictionary txtDictionary => txtDictionary -> IO (Id NSData)
dataFromTXTRecordDictionary txtDictionary =
  do
    cls' <- getRequiredClass "NSNetService"
    sendClassMessage cls' dataFromTXTRecordDictionarySelector (toNSDictionary txtDictionary)

-- | @- resolveWithTimeout:@
resolveWithTimeout :: IsNSNetService nsNetService => nsNetService -> CDouble -> IO ()
resolveWithTimeout nsNetService timeout =
  sendMessage nsNetService resolveWithTimeoutSelector timeout

-- | @- getInputStream:outputStream:@
getInputStream_outputStream :: (IsNSNetService nsNetService, IsNSInputStream inputStream, IsNSOutputStream outputStream) => nsNetService -> inputStream -> outputStream -> IO Bool
getInputStream_outputStream nsNetService inputStream outputStream =
  sendMessage nsNetService getInputStream_outputStreamSelector (toNSInputStream inputStream) (toNSOutputStream outputStream)

-- | @- setTXTRecordData:@
setTXTRecordData :: (IsNSNetService nsNetService, IsNSData recordData) => nsNetService -> recordData -> IO Bool
setTXTRecordData nsNetService recordData =
  sendMessage nsNetService setTXTRecordDataSelector (toNSData recordData)

-- | @- TXTRecordData@
txtRecordData :: IsNSNetService nsNetService => nsNetService -> IO (Id NSData)
txtRecordData nsNetService =
  sendMessage nsNetService txtRecordDataSelector

-- | @- startMonitoring@
startMonitoring :: IsNSNetService nsNetService => nsNetService -> IO ()
startMonitoring nsNetService =
  sendMessage nsNetService startMonitoringSelector

-- | @- stopMonitoring@
stopMonitoring :: IsNSNetService nsNetService => nsNetService -> IO ()
stopMonitoring nsNetService =
  sendMessage nsNetService stopMonitoringSelector

-- | @- delegate@
delegate :: IsNSNetService nsNetService => nsNetService -> IO RawId
delegate nsNetService =
  sendMessage nsNetService delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSNetService nsNetService => nsNetService -> RawId -> IO ()
setDelegate nsNetService value =
  sendMessage nsNetService setDelegateSelector value

-- | @- includesPeerToPeer@
includesPeerToPeer :: IsNSNetService nsNetService => nsNetService -> IO Bool
includesPeerToPeer nsNetService =
  sendMessage nsNetService includesPeerToPeerSelector

-- | @- setIncludesPeerToPeer:@
setIncludesPeerToPeer :: IsNSNetService nsNetService => nsNetService -> Bool -> IO ()
setIncludesPeerToPeer nsNetService value =
  sendMessage nsNetService setIncludesPeerToPeerSelector value

-- | @- name@
name :: IsNSNetService nsNetService => nsNetService -> IO (Id NSString)
name nsNetService =
  sendMessage nsNetService nameSelector

-- | @- type@
type_ :: IsNSNetService nsNetService => nsNetService -> IO (Id NSString)
type_ nsNetService =
  sendMessage nsNetService typeSelector

-- | @- domain@
domain :: IsNSNetService nsNetService => nsNetService -> IO (Id NSString)
domain nsNetService =
  sendMessage nsNetService domainSelector

-- | @- hostName@
hostName :: IsNSNetService nsNetService => nsNetService -> IO (Id NSString)
hostName nsNetService =
  sendMessage nsNetService hostNameSelector

-- | @- addresses@
addresses :: IsNSNetService nsNetService => nsNetService -> IO (Id NSArray)
addresses nsNetService =
  sendMessage nsNetService addressesSelector

-- | @- port@
port :: IsNSNetService nsNetService => nsNetService -> IO CLong
port nsNetService =
  sendMessage nsNetService portSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDomain:type:name:port:@
initWithDomain_type_name_portSelector :: Selector '[Id NSString, Id NSString, Id NSString, CInt] (Id NSNetService)
initWithDomain_type_name_portSelector = mkSelector "initWithDomain:type:name:port:"

-- | @Selector@ for @initWithDomain:type:name:@
initWithDomain_type_nameSelector :: Selector '[Id NSString, Id NSString, Id NSString] (Id NSNetService)
initWithDomain_type_nameSelector = mkSelector "initWithDomain:type:name:"

-- | @Selector@ for @scheduleInRunLoop:forMode:@
scheduleInRunLoop_forModeSelector :: Selector '[Id NSRunLoop, Id NSString] ()
scheduleInRunLoop_forModeSelector = mkSelector "scheduleInRunLoop:forMode:"

-- | @Selector@ for @removeFromRunLoop:forMode:@
removeFromRunLoop_forModeSelector :: Selector '[Id NSRunLoop, Id NSString] ()
removeFromRunLoop_forModeSelector = mkSelector "removeFromRunLoop:forMode:"

-- | @Selector@ for @publish@
publishSelector :: Selector '[] ()
publishSelector = mkSelector "publish"

-- | @Selector@ for @publishWithOptions:@
publishWithOptionsSelector :: Selector '[NSNetServiceOptions] ()
publishWithOptionsSelector = mkSelector "publishWithOptions:"

-- | @Selector@ for @resolve@
resolveSelector :: Selector '[] ()
resolveSelector = mkSelector "resolve"

-- | @Selector@ for @stop@
stopSelector :: Selector '[] ()
stopSelector = mkSelector "stop"

-- | @Selector@ for @dictionaryFromTXTRecordData:@
dictionaryFromTXTRecordDataSelector :: Selector '[Id NSData] (Id NSDictionary)
dictionaryFromTXTRecordDataSelector = mkSelector "dictionaryFromTXTRecordData:"

-- | @Selector@ for @dataFromTXTRecordDictionary:@
dataFromTXTRecordDictionarySelector :: Selector '[Id NSDictionary] (Id NSData)
dataFromTXTRecordDictionarySelector = mkSelector "dataFromTXTRecordDictionary:"

-- | @Selector@ for @resolveWithTimeout:@
resolveWithTimeoutSelector :: Selector '[CDouble] ()
resolveWithTimeoutSelector = mkSelector "resolveWithTimeout:"

-- | @Selector@ for @getInputStream:outputStream:@
getInputStream_outputStreamSelector :: Selector '[Id NSInputStream, Id NSOutputStream] Bool
getInputStream_outputStreamSelector = mkSelector "getInputStream:outputStream:"

-- | @Selector@ for @setTXTRecordData:@
setTXTRecordDataSelector :: Selector '[Id NSData] Bool
setTXTRecordDataSelector = mkSelector "setTXTRecordData:"

-- | @Selector@ for @TXTRecordData@
txtRecordDataSelector :: Selector '[] (Id NSData)
txtRecordDataSelector = mkSelector "TXTRecordData"

-- | @Selector@ for @startMonitoring@
startMonitoringSelector :: Selector '[] ()
startMonitoringSelector = mkSelector "startMonitoring"

-- | @Selector@ for @stopMonitoring@
stopMonitoringSelector :: Selector '[] ()
stopMonitoringSelector = mkSelector "stopMonitoring"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @includesPeerToPeer@
includesPeerToPeerSelector :: Selector '[] Bool
includesPeerToPeerSelector = mkSelector "includesPeerToPeer"

-- | @Selector@ for @setIncludesPeerToPeer:@
setIncludesPeerToPeerSelector :: Selector '[Bool] ()
setIncludesPeerToPeerSelector = mkSelector "setIncludesPeerToPeer:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @domain@
domainSelector :: Selector '[] (Id NSString)
domainSelector = mkSelector "domain"

-- | @Selector@ for @hostName@
hostNameSelector :: Selector '[] (Id NSString)
hostNameSelector = mkSelector "hostName"

-- | @Selector@ for @addresses@
addressesSelector :: Selector '[] (Id NSArray)
addressesSelector = mkSelector "addresses"

-- | @Selector@ for @port@
portSelector :: Selector '[] CLong
portSelector = mkSelector "port"

