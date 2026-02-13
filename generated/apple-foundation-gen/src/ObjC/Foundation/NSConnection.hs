{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSConnection@.
module ObjC.Foundation.NSConnection
  ( NSConnection
  , IsNSConnection(..)
  , allConnections
  , defaultConnection
  , connectionWithRegisteredName_host
  , connectionWithRegisteredName_host_usingNameServer
  , rootProxyForConnectionWithRegisteredName_host
  , rootProxyForConnectionWithRegisteredName_host_usingNameServer
  , serviceConnectionWithName_rootObject_usingNameServer
  , serviceConnectionWithName_rootObject
  , invalidate
  , addRequestMode
  , removeRequestMode
  , registerName
  , registerName_withNameServer
  , connectionWithReceivePort_sendPort
  , currentConversation
  , initWithReceivePort_sendPort
  , enableMultipleThreads
  , addRunLoop
  , removeRunLoop
  , runInNewThread
  , dispatchWithComponents
  , statistics
  , requestTimeout
  , setRequestTimeout
  , replyTimeout
  , setReplyTimeout
  , rootObject
  , setRootObject
  , delegate
  , setDelegate
  , independentConversationQueueing
  , setIndependentConversationQueueing
  , valid
  , rootProxy
  , requestModes
  , sendPort
  , receivePort
  , multipleThreadsEnabled
  , remoteObjects
  , localObjects
  , addRequestModeSelector
  , addRunLoopSelector
  , allConnectionsSelector
  , connectionWithReceivePort_sendPortSelector
  , connectionWithRegisteredName_hostSelector
  , connectionWithRegisteredName_host_usingNameServerSelector
  , currentConversationSelector
  , defaultConnectionSelector
  , delegateSelector
  , dispatchWithComponentsSelector
  , enableMultipleThreadsSelector
  , independentConversationQueueingSelector
  , initWithReceivePort_sendPortSelector
  , invalidateSelector
  , localObjectsSelector
  , multipleThreadsEnabledSelector
  , receivePortSelector
  , registerNameSelector
  , registerName_withNameServerSelector
  , remoteObjectsSelector
  , removeRequestModeSelector
  , removeRunLoopSelector
  , replyTimeoutSelector
  , requestModesSelector
  , requestTimeoutSelector
  , rootObjectSelector
  , rootProxyForConnectionWithRegisteredName_hostSelector
  , rootProxyForConnectionWithRegisteredName_host_usingNameServerSelector
  , rootProxySelector
  , runInNewThreadSelector
  , sendPortSelector
  , serviceConnectionWithName_rootObjectSelector
  , serviceConnectionWithName_rootObject_usingNameServerSelector
  , setDelegateSelector
  , setIndependentConversationQueueingSelector
  , setReplyTimeoutSelector
  , setRequestTimeoutSelector
  , setRootObjectSelector
  , statisticsSelector
  , validSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ allConnections@
allConnections :: IO (Id NSArray)
allConnections  =
  do
    cls' <- getRequiredClass "NSConnection"
    sendClassMessage cls' allConnectionsSelector

-- | @+ defaultConnection@
defaultConnection :: IO (Id NSConnection)
defaultConnection  =
  do
    cls' <- getRequiredClass "NSConnection"
    sendClassMessage cls' defaultConnectionSelector

-- | @+ connectionWithRegisteredName:host:@
connectionWithRegisteredName_host :: (IsNSString name, IsNSString hostName) => name -> hostName -> IO (Id NSConnection)
connectionWithRegisteredName_host name hostName =
  do
    cls' <- getRequiredClass "NSConnection"
    sendClassMessage cls' connectionWithRegisteredName_hostSelector (toNSString name) (toNSString hostName)

-- | @+ connectionWithRegisteredName:host:usingNameServer:@
connectionWithRegisteredName_host_usingNameServer :: (IsNSString name, IsNSString hostName, IsNSPortNameServer server) => name -> hostName -> server -> IO (Id NSConnection)
connectionWithRegisteredName_host_usingNameServer name hostName server =
  do
    cls' <- getRequiredClass "NSConnection"
    sendClassMessage cls' connectionWithRegisteredName_host_usingNameServerSelector (toNSString name) (toNSString hostName) (toNSPortNameServer server)

-- | @+ rootProxyForConnectionWithRegisteredName:host:@
rootProxyForConnectionWithRegisteredName_host :: (IsNSString name, IsNSString hostName) => name -> hostName -> IO (Id NSDistantObject)
rootProxyForConnectionWithRegisteredName_host name hostName =
  do
    cls' <- getRequiredClass "NSConnection"
    sendClassMessage cls' rootProxyForConnectionWithRegisteredName_hostSelector (toNSString name) (toNSString hostName)

-- | @+ rootProxyForConnectionWithRegisteredName:host:usingNameServer:@
rootProxyForConnectionWithRegisteredName_host_usingNameServer :: (IsNSString name, IsNSString hostName, IsNSPortNameServer server) => name -> hostName -> server -> IO (Id NSDistantObject)
rootProxyForConnectionWithRegisteredName_host_usingNameServer name hostName server =
  do
    cls' <- getRequiredClass "NSConnection"
    sendClassMessage cls' rootProxyForConnectionWithRegisteredName_host_usingNameServerSelector (toNSString name) (toNSString hostName) (toNSPortNameServer server)

-- | @+ serviceConnectionWithName:rootObject:usingNameServer:@
serviceConnectionWithName_rootObject_usingNameServer :: (IsNSString name, IsNSPortNameServer server) => name -> RawId -> server -> IO (Id NSConnection)
serviceConnectionWithName_rootObject_usingNameServer name root server =
  do
    cls' <- getRequiredClass "NSConnection"
    sendClassMessage cls' serviceConnectionWithName_rootObject_usingNameServerSelector (toNSString name) root (toNSPortNameServer server)

-- | @+ serviceConnectionWithName:rootObject:@
serviceConnectionWithName_rootObject :: IsNSString name => name -> RawId -> IO (Id NSConnection)
serviceConnectionWithName_rootObject name root =
  do
    cls' <- getRequiredClass "NSConnection"
    sendClassMessage cls' serviceConnectionWithName_rootObjectSelector (toNSString name) root

-- | @- invalidate@
invalidate :: IsNSConnection nsConnection => nsConnection -> IO ()
invalidate nsConnection =
  sendMessage nsConnection invalidateSelector

-- | @- addRequestMode:@
addRequestMode :: (IsNSConnection nsConnection, IsNSString rmode) => nsConnection -> rmode -> IO ()
addRequestMode nsConnection rmode =
  sendMessage nsConnection addRequestModeSelector (toNSString rmode)

-- | @- removeRequestMode:@
removeRequestMode :: (IsNSConnection nsConnection, IsNSString rmode) => nsConnection -> rmode -> IO ()
removeRequestMode nsConnection rmode =
  sendMessage nsConnection removeRequestModeSelector (toNSString rmode)

-- | @- registerName:@
registerName :: (IsNSConnection nsConnection, IsNSString name) => nsConnection -> name -> IO Bool
registerName nsConnection name =
  sendMessage nsConnection registerNameSelector (toNSString name)

-- | @- registerName:withNameServer:@
registerName_withNameServer :: (IsNSConnection nsConnection, IsNSString name, IsNSPortNameServer server) => nsConnection -> name -> server -> IO Bool
registerName_withNameServer nsConnection name server =
  sendMessage nsConnection registerName_withNameServerSelector (toNSString name) (toNSPortNameServer server)

-- | @+ connectionWithReceivePort:sendPort:@
connectionWithReceivePort_sendPort :: (IsNSPort receivePort, IsNSPort sendPort) => receivePort -> sendPort -> IO (Id NSConnection)
connectionWithReceivePort_sendPort receivePort sendPort =
  do
    cls' <- getRequiredClass "NSConnection"
    sendClassMessage cls' connectionWithReceivePort_sendPortSelector (toNSPort receivePort) (toNSPort sendPort)

-- | @+ currentConversation@
currentConversation :: IO RawId
currentConversation  =
  do
    cls' <- getRequiredClass "NSConnection"
    sendClassMessage cls' currentConversationSelector

-- | @- initWithReceivePort:sendPort:@
initWithReceivePort_sendPort :: (IsNSConnection nsConnection, IsNSPort receivePort, IsNSPort sendPort) => nsConnection -> receivePort -> sendPort -> IO (Id NSConnection)
initWithReceivePort_sendPort nsConnection receivePort sendPort =
  sendOwnedMessage nsConnection initWithReceivePort_sendPortSelector (toNSPort receivePort) (toNSPort sendPort)

-- | @- enableMultipleThreads@
enableMultipleThreads :: IsNSConnection nsConnection => nsConnection -> IO ()
enableMultipleThreads nsConnection =
  sendMessage nsConnection enableMultipleThreadsSelector

-- | @- addRunLoop:@
addRunLoop :: (IsNSConnection nsConnection, IsNSRunLoop runloop) => nsConnection -> runloop -> IO ()
addRunLoop nsConnection runloop =
  sendMessage nsConnection addRunLoopSelector (toNSRunLoop runloop)

-- | @- removeRunLoop:@
removeRunLoop :: (IsNSConnection nsConnection, IsNSRunLoop runloop) => nsConnection -> runloop -> IO ()
removeRunLoop nsConnection runloop =
  sendMessage nsConnection removeRunLoopSelector (toNSRunLoop runloop)

-- | @- runInNewThread@
runInNewThread :: IsNSConnection nsConnection => nsConnection -> IO ()
runInNewThread nsConnection =
  sendMessage nsConnection runInNewThreadSelector

-- | @- dispatchWithComponents:@
dispatchWithComponents :: (IsNSConnection nsConnection, IsNSArray components) => nsConnection -> components -> IO ()
dispatchWithComponents nsConnection components =
  sendMessage nsConnection dispatchWithComponentsSelector (toNSArray components)

-- | @- statistics@
statistics :: IsNSConnection nsConnection => nsConnection -> IO (Id NSDictionary)
statistics nsConnection =
  sendMessage nsConnection statisticsSelector

-- | @- requestTimeout@
requestTimeout :: IsNSConnection nsConnection => nsConnection -> IO CDouble
requestTimeout nsConnection =
  sendMessage nsConnection requestTimeoutSelector

-- | @- setRequestTimeout:@
setRequestTimeout :: IsNSConnection nsConnection => nsConnection -> CDouble -> IO ()
setRequestTimeout nsConnection value =
  sendMessage nsConnection setRequestTimeoutSelector value

-- | @- replyTimeout@
replyTimeout :: IsNSConnection nsConnection => nsConnection -> IO CDouble
replyTimeout nsConnection =
  sendMessage nsConnection replyTimeoutSelector

-- | @- setReplyTimeout:@
setReplyTimeout :: IsNSConnection nsConnection => nsConnection -> CDouble -> IO ()
setReplyTimeout nsConnection value =
  sendMessage nsConnection setReplyTimeoutSelector value

-- | @- rootObject@
rootObject :: IsNSConnection nsConnection => nsConnection -> IO RawId
rootObject nsConnection =
  sendMessage nsConnection rootObjectSelector

-- | @- setRootObject:@
setRootObject :: IsNSConnection nsConnection => nsConnection -> RawId -> IO ()
setRootObject nsConnection value =
  sendMessage nsConnection setRootObjectSelector value

-- | @- delegate@
delegate :: IsNSConnection nsConnection => nsConnection -> IO RawId
delegate nsConnection =
  sendMessage nsConnection delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSConnection nsConnection => nsConnection -> RawId -> IO ()
setDelegate nsConnection value =
  sendMessage nsConnection setDelegateSelector value

-- | @- independentConversationQueueing@
independentConversationQueueing :: IsNSConnection nsConnection => nsConnection -> IO Bool
independentConversationQueueing nsConnection =
  sendMessage nsConnection independentConversationQueueingSelector

-- | @- setIndependentConversationQueueing:@
setIndependentConversationQueueing :: IsNSConnection nsConnection => nsConnection -> Bool -> IO ()
setIndependentConversationQueueing nsConnection value =
  sendMessage nsConnection setIndependentConversationQueueingSelector value

-- | @- valid@
valid :: IsNSConnection nsConnection => nsConnection -> IO Bool
valid nsConnection =
  sendMessage nsConnection validSelector

-- | @- rootProxy@
rootProxy :: IsNSConnection nsConnection => nsConnection -> IO (Id NSDistantObject)
rootProxy nsConnection =
  sendMessage nsConnection rootProxySelector

-- | @- requestModes@
requestModes :: IsNSConnection nsConnection => nsConnection -> IO (Id NSArray)
requestModes nsConnection =
  sendMessage nsConnection requestModesSelector

-- | @- sendPort@
sendPort :: IsNSConnection nsConnection => nsConnection -> IO (Id NSPort)
sendPort nsConnection =
  sendMessage nsConnection sendPortSelector

-- | @- receivePort@
receivePort :: IsNSConnection nsConnection => nsConnection -> IO (Id NSPort)
receivePort nsConnection =
  sendMessage nsConnection receivePortSelector

-- | @- multipleThreadsEnabled@
multipleThreadsEnabled :: IsNSConnection nsConnection => nsConnection -> IO Bool
multipleThreadsEnabled nsConnection =
  sendMessage nsConnection multipleThreadsEnabledSelector

-- | @- remoteObjects@
remoteObjects :: IsNSConnection nsConnection => nsConnection -> IO (Id NSArray)
remoteObjects nsConnection =
  sendMessage nsConnection remoteObjectsSelector

-- | @- localObjects@
localObjects :: IsNSConnection nsConnection => nsConnection -> IO (Id NSArray)
localObjects nsConnection =
  sendMessage nsConnection localObjectsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allConnections@
allConnectionsSelector :: Selector '[] (Id NSArray)
allConnectionsSelector = mkSelector "allConnections"

-- | @Selector@ for @defaultConnection@
defaultConnectionSelector :: Selector '[] (Id NSConnection)
defaultConnectionSelector = mkSelector "defaultConnection"

-- | @Selector@ for @connectionWithRegisteredName:host:@
connectionWithRegisteredName_hostSelector :: Selector '[Id NSString, Id NSString] (Id NSConnection)
connectionWithRegisteredName_hostSelector = mkSelector "connectionWithRegisteredName:host:"

-- | @Selector@ for @connectionWithRegisteredName:host:usingNameServer:@
connectionWithRegisteredName_host_usingNameServerSelector :: Selector '[Id NSString, Id NSString, Id NSPortNameServer] (Id NSConnection)
connectionWithRegisteredName_host_usingNameServerSelector = mkSelector "connectionWithRegisteredName:host:usingNameServer:"

-- | @Selector@ for @rootProxyForConnectionWithRegisteredName:host:@
rootProxyForConnectionWithRegisteredName_hostSelector :: Selector '[Id NSString, Id NSString] (Id NSDistantObject)
rootProxyForConnectionWithRegisteredName_hostSelector = mkSelector "rootProxyForConnectionWithRegisteredName:host:"

-- | @Selector@ for @rootProxyForConnectionWithRegisteredName:host:usingNameServer:@
rootProxyForConnectionWithRegisteredName_host_usingNameServerSelector :: Selector '[Id NSString, Id NSString, Id NSPortNameServer] (Id NSDistantObject)
rootProxyForConnectionWithRegisteredName_host_usingNameServerSelector = mkSelector "rootProxyForConnectionWithRegisteredName:host:usingNameServer:"

-- | @Selector@ for @serviceConnectionWithName:rootObject:usingNameServer:@
serviceConnectionWithName_rootObject_usingNameServerSelector :: Selector '[Id NSString, RawId, Id NSPortNameServer] (Id NSConnection)
serviceConnectionWithName_rootObject_usingNameServerSelector = mkSelector "serviceConnectionWithName:rootObject:usingNameServer:"

-- | @Selector@ for @serviceConnectionWithName:rootObject:@
serviceConnectionWithName_rootObjectSelector :: Selector '[Id NSString, RawId] (Id NSConnection)
serviceConnectionWithName_rootObjectSelector = mkSelector "serviceConnectionWithName:rootObject:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector '[] ()
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @addRequestMode:@
addRequestModeSelector :: Selector '[Id NSString] ()
addRequestModeSelector = mkSelector "addRequestMode:"

-- | @Selector@ for @removeRequestMode:@
removeRequestModeSelector :: Selector '[Id NSString] ()
removeRequestModeSelector = mkSelector "removeRequestMode:"

-- | @Selector@ for @registerName:@
registerNameSelector :: Selector '[Id NSString] Bool
registerNameSelector = mkSelector "registerName:"

-- | @Selector@ for @registerName:withNameServer:@
registerName_withNameServerSelector :: Selector '[Id NSString, Id NSPortNameServer] Bool
registerName_withNameServerSelector = mkSelector "registerName:withNameServer:"

-- | @Selector@ for @connectionWithReceivePort:sendPort:@
connectionWithReceivePort_sendPortSelector :: Selector '[Id NSPort, Id NSPort] (Id NSConnection)
connectionWithReceivePort_sendPortSelector = mkSelector "connectionWithReceivePort:sendPort:"

-- | @Selector@ for @currentConversation@
currentConversationSelector :: Selector '[] RawId
currentConversationSelector = mkSelector "currentConversation"

-- | @Selector@ for @initWithReceivePort:sendPort:@
initWithReceivePort_sendPortSelector :: Selector '[Id NSPort, Id NSPort] (Id NSConnection)
initWithReceivePort_sendPortSelector = mkSelector "initWithReceivePort:sendPort:"

-- | @Selector@ for @enableMultipleThreads@
enableMultipleThreadsSelector :: Selector '[] ()
enableMultipleThreadsSelector = mkSelector "enableMultipleThreads"

-- | @Selector@ for @addRunLoop:@
addRunLoopSelector :: Selector '[Id NSRunLoop] ()
addRunLoopSelector = mkSelector "addRunLoop:"

-- | @Selector@ for @removeRunLoop:@
removeRunLoopSelector :: Selector '[Id NSRunLoop] ()
removeRunLoopSelector = mkSelector "removeRunLoop:"

-- | @Selector@ for @runInNewThread@
runInNewThreadSelector :: Selector '[] ()
runInNewThreadSelector = mkSelector "runInNewThread"

-- | @Selector@ for @dispatchWithComponents:@
dispatchWithComponentsSelector :: Selector '[Id NSArray] ()
dispatchWithComponentsSelector = mkSelector "dispatchWithComponents:"

-- | @Selector@ for @statistics@
statisticsSelector :: Selector '[] (Id NSDictionary)
statisticsSelector = mkSelector "statistics"

-- | @Selector@ for @requestTimeout@
requestTimeoutSelector :: Selector '[] CDouble
requestTimeoutSelector = mkSelector "requestTimeout"

-- | @Selector@ for @setRequestTimeout:@
setRequestTimeoutSelector :: Selector '[CDouble] ()
setRequestTimeoutSelector = mkSelector "setRequestTimeout:"

-- | @Selector@ for @replyTimeout@
replyTimeoutSelector :: Selector '[] CDouble
replyTimeoutSelector = mkSelector "replyTimeout"

-- | @Selector@ for @setReplyTimeout:@
setReplyTimeoutSelector :: Selector '[CDouble] ()
setReplyTimeoutSelector = mkSelector "setReplyTimeout:"

-- | @Selector@ for @rootObject@
rootObjectSelector :: Selector '[] RawId
rootObjectSelector = mkSelector "rootObject"

-- | @Selector@ for @setRootObject:@
setRootObjectSelector :: Selector '[RawId] ()
setRootObjectSelector = mkSelector "setRootObject:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @independentConversationQueueing@
independentConversationQueueingSelector :: Selector '[] Bool
independentConversationQueueingSelector = mkSelector "independentConversationQueueing"

-- | @Selector@ for @setIndependentConversationQueueing:@
setIndependentConversationQueueingSelector :: Selector '[Bool] ()
setIndependentConversationQueueingSelector = mkSelector "setIndependentConversationQueueing:"

-- | @Selector@ for @valid@
validSelector :: Selector '[] Bool
validSelector = mkSelector "valid"

-- | @Selector@ for @rootProxy@
rootProxySelector :: Selector '[] (Id NSDistantObject)
rootProxySelector = mkSelector "rootProxy"

-- | @Selector@ for @requestModes@
requestModesSelector :: Selector '[] (Id NSArray)
requestModesSelector = mkSelector "requestModes"

-- | @Selector@ for @sendPort@
sendPortSelector :: Selector '[] (Id NSPort)
sendPortSelector = mkSelector "sendPort"

-- | @Selector@ for @receivePort@
receivePortSelector :: Selector '[] (Id NSPort)
receivePortSelector = mkSelector "receivePort"

-- | @Selector@ for @multipleThreadsEnabled@
multipleThreadsEnabledSelector :: Selector '[] Bool
multipleThreadsEnabledSelector = mkSelector "multipleThreadsEnabled"

-- | @Selector@ for @remoteObjects@
remoteObjectsSelector :: Selector '[] (Id NSArray)
remoteObjectsSelector = mkSelector "remoteObjects"

-- | @Selector@ for @localObjects@
localObjectsSelector :: Selector '[] (Id NSArray)
localObjectsSelector = mkSelector "localObjects"

