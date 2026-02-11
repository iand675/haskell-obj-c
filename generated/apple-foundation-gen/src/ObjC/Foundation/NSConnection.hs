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
  , allConnectionsSelector
  , defaultConnectionSelector
  , connectionWithRegisteredName_hostSelector
  , connectionWithRegisteredName_host_usingNameServerSelector
  , rootProxyForConnectionWithRegisteredName_hostSelector
  , rootProxyForConnectionWithRegisteredName_host_usingNameServerSelector
  , serviceConnectionWithName_rootObject_usingNameServerSelector
  , serviceConnectionWithName_rootObjectSelector
  , invalidateSelector
  , addRequestModeSelector
  , removeRequestModeSelector
  , registerNameSelector
  , registerName_withNameServerSelector
  , connectionWithReceivePort_sendPortSelector
  , currentConversationSelector
  , initWithReceivePort_sendPortSelector
  , enableMultipleThreadsSelector
  , addRunLoopSelector
  , removeRunLoopSelector
  , runInNewThreadSelector
  , dispatchWithComponentsSelector
  , statisticsSelector
  , requestTimeoutSelector
  , setRequestTimeoutSelector
  , replyTimeoutSelector
  , setReplyTimeoutSelector
  , rootObjectSelector
  , setRootObjectSelector
  , delegateSelector
  , setDelegateSelector
  , independentConversationQueueingSelector
  , setIndependentConversationQueueingSelector
  , validSelector
  , rootProxySelector
  , requestModesSelector
  , sendPortSelector
  , receivePortSelector
  , multipleThreadsEnabledSelector
  , remoteObjectsSelector
  , localObjectsSelector


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

-- | @+ allConnections@
allConnections :: IO (Id NSArray)
allConnections  =
  do
    cls' <- getRequiredClass "NSConnection"
    sendClassMsg cls' (mkSelector "allConnections") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ defaultConnection@
defaultConnection :: IO (Id NSConnection)
defaultConnection  =
  do
    cls' <- getRequiredClass "NSConnection"
    sendClassMsg cls' (mkSelector "defaultConnection") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ connectionWithRegisteredName:host:@
connectionWithRegisteredName_host :: (IsNSString name, IsNSString hostName) => name -> hostName -> IO (Id NSConnection)
connectionWithRegisteredName_host name hostName =
  do
    cls' <- getRequiredClass "NSConnection"
    withObjCPtr name $ \raw_name ->
      withObjCPtr hostName $ \raw_hostName ->
        sendClassMsg cls' (mkSelector "connectionWithRegisteredName:host:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_hostName :: Ptr ())] >>= retainedObject . castPtr

-- | @+ connectionWithRegisteredName:host:usingNameServer:@
connectionWithRegisteredName_host_usingNameServer :: (IsNSString name, IsNSString hostName, IsNSPortNameServer server) => name -> hostName -> server -> IO (Id NSConnection)
connectionWithRegisteredName_host_usingNameServer name hostName server =
  do
    cls' <- getRequiredClass "NSConnection"
    withObjCPtr name $ \raw_name ->
      withObjCPtr hostName $ \raw_hostName ->
        withObjCPtr server $ \raw_server ->
          sendClassMsg cls' (mkSelector "connectionWithRegisteredName:host:usingNameServer:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_hostName :: Ptr ()), argPtr (castPtr raw_server :: Ptr ())] >>= retainedObject . castPtr

-- | @+ rootProxyForConnectionWithRegisteredName:host:@
rootProxyForConnectionWithRegisteredName_host :: (IsNSString name, IsNSString hostName) => name -> hostName -> IO (Id NSDistantObject)
rootProxyForConnectionWithRegisteredName_host name hostName =
  do
    cls' <- getRequiredClass "NSConnection"
    withObjCPtr name $ \raw_name ->
      withObjCPtr hostName $ \raw_hostName ->
        sendClassMsg cls' (mkSelector "rootProxyForConnectionWithRegisteredName:host:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_hostName :: Ptr ())] >>= retainedObject . castPtr

-- | @+ rootProxyForConnectionWithRegisteredName:host:usingNameServer:@
rootProxyForConnectionWithRegisteredName_host_usingNameServer :: (IsNSString name, IsNSString hostName, IsNSPortNameServer server) => name -> hostName -> server -> IO (Id NSDistantObject)
rootProxyForConnectionWithRegisteredName_host_usingNameServer name hostName server =
  do
    cls' <- getRequiredClass "NSConnection"
    withObjCPtr name $ \raw_name ->
      withObjCPtr hostName $ \raw_hostName ->
        withObjCPtr server $ \raw_server ->
          sendClassMsg cls' (mkSelector "rootProxyForConnectionWithRegisteredName:host:usingNameServer:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_hostName :: Ptr ()), argPtr (castPtr raw_server :: Ptr ())] >>= retainedObject . castPtr

-- | @+ serviceConnectionWithName:rootObject:usingNameServer:@
serviceConnectionWithName_rootObject_usingNameServer :: (IsNSString name, IsNSPortNameServer server) => name -> RawId -> server -> IO (Id NSConnection)
serviceConnectionWithName_rootObject_usingNameServer name root server =
  do
    cls' <- getRequiredClass "NSConnection"
    withObjCPtr name $ \raw_name ->
      withObjCPtr server $ \raw_server ->
        sendClassMsg cls' (mkSelector "serviceConnectionWithName:rootObject:usingNameServer:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr (unRawId root) :: Ptr ()), argPtr (castPtr raw_server :: Ptr ())] >>= retainedObject . castPtr

-- | @+ serviceConnectionWithName:rootObject:@
serviceConnectionWithName_rootObject :: IsNSString name => name -> RawId -> IO (Id NSConnection)
serviceConnectionWithName_rootObject name root =
  do
    cls' <- getRequiredClass "NSConnection"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "serviceConnectionWithName:rootObject:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr (unRawId root) :: Ptr ())] >>= retainedObject . castPtr

-- | @- invalidate@
invalidate :: IsNSConnection nsConnection => nsConnection -> IO ()
invalidate nsConnection  =
    sendMsg nsConnection (mkSelector "invalidate") retVoid []

-- | @- addRequestMode:@
addRequestMode :: (IsNSConnection nsConnection, IsNSString rmode) => nsConnection -> rmode -> IO ()
addRequestMode nsConnection  rmode =
  withObjCPtr rmode $ \raw_rmode ->
      sendMsg nsConnection (mkSelector "addRequestMode:") retVoid [argPtr (castPtr raw_rmode :: Ptr ())]

-- | @- removeRequestMode:@
removeRequestMode :: (IsNSConnection nsConnection, IsNSString rmode) => nsConnection -> rmode -> IO ()
removeRequestMode nsConnection  rmode =
  withObjCPtr rmode $ \raw_rmode ->
      sendMsg nsConnection (mkSelector "removeRequestMode:") retVoid [argPtr (castPtr raw_rmode :: Ptr ())]

-- | @- registerName:@
registerName :: (IsNSConnection nsConnection, IsNSString name) => nsConnection -> name -> IO Bool
registerName nsConnection  name =
  withObjCPtr name $ \raw_name ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsConnection (mkSelector "registerName:") retCULong [argPtr (castPtr raw_name :: Ptr ())]

-- | @- registerName:withNameServer:@
registerName_withNameServer :: (IsNSConnection nsConnection, IsNSString name, IsNSPortNameServer server) => nsConnection -> name -> server -> IO Bool
registerName_withNameServer nsConnection  name server =
  withObjCPtr name $ \raw_name ->
    withObjCPtr server $ \raw_server ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsConnection (mkSelector "registerName:withNameServer:") retCULong [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_server :: Ptr ())]

-- | @+ connectionWithReceivePort:sendPort:@
connectionWithReceivePort_sendPort :: (IsNSPort receivePort, IsNSPort sendPort) => receivePort -> sendPort -> IO (Id NSConnection)
connectionWithReceivePort_sendPort receivePort sendPort =
  do
    cls' <- getRequiredClass "NSConnection"
    withObjCPtr receivePort $ \raw_receivePort ->
      withObjCPtr sendPort $ \raw_sendPort ->
        sendClassMsg cls' (mkSelector "connectionWithReceivePort:sendPort:") (retPtr retVoid) [argPtr (castPtr raw_receivePort :: Ptr ()), argPtr (castPtr raw_sendPort :: Ptr ())] >>= retainedObject . castPtr

-- | @+ currentConversation@
currentConversation :: IO RawId
currentConversation  =
  do
    cls' <- getRequiredClass "NSConnection"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "currentConversation") (retPtr retVoid) []

-- | @- initWithReceivePort:sendPort:@
initWithReceivePort_sendPort :: (IsNSConnection nsConnection, IsNSPort receivePort, IsNSPort sendPort) => nsConnection -> receivePort -> sendPort -> IO (Id NSConnection)
initWithReceivePort_sendPort nsConnection  receivePort sendPort =
  withObjCPtr receivePort $ \raw_receivePort ->
    withObjCPtr sendPort $ \raw_sendPort ->
        sendMsg nsConnection (mkSelector "initWithReceivePort:sendPort:") (retPtr retVoid) [argPtr (castPtr raw_receivePort :: Ptr ()), argPtr (castPtr raw_sendPort :: Ptr ())] >>= ownedObject . castPtr

-- | @- enableMultipleThreads@
enableMultipleThreads :: IsNSConnection nsConnection => nsConnection -> IO ()
enableMultipleThreads nsConnection  =
    sendMsg nsConnection (mkSelector "enableMultipleThreads") retVoid []

-- | @- addRunLoop:@
addRunLoop :: (IsNSConnection nsConnection, IsNSRunLoop runloop) => nsConnection -> runloop -> IO ()
addRunLoop nsConnection  runloop =
  withObjCPtr runloop $ \raw_runloop ->
      sendMsg nsConnection (mkSelector "addRunLoop:") retVoid [argPtr (castPtr raw_runloop :: Ptr ())]

-- | @- removeRunLoop:@
removeRunLoop :: (IsNSConnection nsConnection, IsNSRunLoop runloop) => nsConnection -> runloop -> IO ()
removeRunLoop nsConnection  runloop =
  withObjCPtr runloop $ \raw_runloop ->
      sendMsg nsConnection (mkSelector "removeRunLoop:") retVoid [argPtr (castPtr raw_runloop :: Ptr ())]

-- | @- runInNewThread@
runInNewThread :: IsNSConnection nsConnection => nsConnection -> IO ()
runInNewThread nsConnection  =
    sendMsg nsConnection (mkSelector "runInNewThread") retVoid []

-- | @- dispatchWithComponents:@
dispatchWithComponents :: (IsNSConnection nsConnection, IsNSArray components) => nsConnection -> components -> IO ()
dispatchWithComponents nsConnection  components =
  withObjCPtr components $ \raw_components ->
      sendMsg nsConnection (mkSelector "dispatchWithComponents:") retVoid [argPtr (castPtr raw_components :: Ptr ())]

-- | @- statistics@
statistics :: IsNSConnection nsConnection => nsConnection -> IO (Id NSDictionary)
statistics nsConnection  =
    sendMsg nsConnection (mkSelector "statistics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- requestTimeout@
requestTimeout :: IsNSConnection nsConnection => nsConnection -> IO CDouble
requestTimeout nsConnection  =
    sendMsg nsConnection (mkSelector "requestTimeout") retCDouble []

-- | @- setRequestTimeout:@
setRequestTimeout :: IsNSConnection nsConnection => nsConnection -> CDouble -> IO ()
setRequestTimeout nsConnection  value =
    sendMsg nsConnection (mkSelector "setRequestTimeout:") retVoid [argCDouble value]

-- | @- replyTimeout@
replyTimeout :: IsNSConnection nsConnection => nsConnection -> IO CDouble
replyTimeout nsConnection  =
    sendMsg nsConnection (mkSelector "replyTimeout") retCDouble []

-- | @- setReplyTimeout:@
setReplyTimeout :: IsNSConnection nsConnection => nsConnection -> CDouble -> IO ()
setReplyTimeout nsConnection  value =
    sendMsg nsConnection (mkSelector "setReplyTimeout:") retVoid [argCDouble value]

-- | @- rootObject@
rootObject :: IsNSConnection nsConnection => nsConnection -> IO RawId
rootObject nsConnection  =
    fmap (RawId . castPtr) $ sendMsg nsConnection (mkSelector "rootObject") (retPtr retVoid) []

-- | @- setRootObject:@
setRootObject :: IsNSConnection nsConnection => nsConnection -> RawId -> IO ()
setRootObject nsConnection  value =
    sendMsg nsConnection (mkSelector "setRootObject:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- delegate@
delegate :: IsNSConnection nsConnection => nsConnection -> IO RawId
delegate nsConnection  =
    fmap (RawId . castPtr) $ sendMsg nsConnection (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSConnection nsConnection => nsConnection -> RawId -> IO ()
setDelegate nsConnection  value =
    sendMsg nsConnection (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- independentConversationQueueing@
independentConversationQueueing :: IsNSConnection nsConnection => nsConnection -> IO Bool
independentConversationQueueing nsConnection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsConnection (mkSelector "independentConversationQueueing") retCULong []

-- | @- setIndependentConversationQueueing:@
setIndependentConversationQueueing :: IsNSConnection nsConnection => nsConnection -> Bool -> IO ()
setIndependentConversationQueueing nsConnection  value =
    sendMsg nsConnection (mkSelector "setIndependentConversationQueueing:") retVoid [argCULong (if value then 1 else 0)]

-- | @- valid@
valid :: IsNSConnection nsConnection => nsConnection -> IO Bool
valid nsConnection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsConnection (mkSelector "valid") retCULong []

-- | @- rootProxy@
rootProxy :: IsNSConnection nsConnection => nsConnection -> IO (Id NSDistantObject)
rootProxy nsConnection  =
    sendMsg nsConnection (mkSelector "rootProxy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- requestModes@
requestModes :: IsNSConnection nsConnection => nsConnection -> IO (Id NSArray)
requestModes nsConnection  =
    sendMsg nsConnection (mkSelector "requestModes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sendPort@
sendPort :: IsNSConnection nsConnection => nsConnection -> IO (Id NSPort)
sendPort nsConnection  =
    sendMsg nsConnection (mkSelector "sendPort") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- receivePort@
receivePort :: IsNSConnection nsConnection => nsConnection -> IO (Id NSPort)
receivePort nsConnection  =
    sendMsg nsConnection (mkSelector "receivePort") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- multipleThreadsEnabled@
multipleThreadsEnabled :: IsNSConnection nsConnection => nsConnection -> IO Bool
multipleThreadsEnabled nsConnection  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsConnection (mkSelector "multipleThreadsEnabled") retCULong []

-- | @- remoteObjects@
remoteObjects :: IsNSConnection nsConnection => nsConnection -> IO (Id NSArray)
remoteObjects nsConnection  =
    sendMsg nsConnection (mkSelector "remoteObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localObjects@
localObjects :: IsNSConnection nsConnection => nsConnection -> IO (Id NSArray)
localObjects nsConnection  =
    sendMsg nsConnection (mkSelector "localObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allConnections@
allConnectionsSelector :: Selector
allConnectionsSelector = mkSelector "allConnections"

-- | @Selector@ for @defaultConnection@
defaultConnectionSelector :: Selector
defaultConnectionSelector = mkSelector "defaultConnection"

-- | @Selector@ for @connectionWithRegisteredName:host:@
connectionWithRegisteredName_hostSelector :: Selector
connectionWithRegisteredName_hostSelector = mkSelector "connectionWithRegisteredName:host:"

-- | @Selector@ for @connectionWithRegisteredName:host:usingNameServer:@
connectionWithRegisteredName_host_usingNameServerSelector :: Selector
connectionWithRegisteredName_host_usingNameServerSelector = mkSelector "connectionWithRegisteredName:host:usingNameServer:"

-- | @Selector@ for @rootProxyForConnectionWithRegisteredName:host:@
rootProxyForConnectionWithRegisteredName_hostSelector :: Selector
rootProxyForConnectionWithRegisteredName_hostSelector = mkSelector "rootProxyForConnectionWithRegisteredName:host:"

-- | @Selector@ for @rootProxyForConnectionWithRegisteredName:host:usingNameServer:@
rootProxyForConnectionWithRegisteredName_host_usingNameServerSelector :: Selector
rootProxyForConnectionWithRegisteredName_host_usingNameServerSelector = mkSelector "rootProxyForConnectionWithRegisteredName:host:usingNameServer:"

-- | @Selector@ for @serviceConnectionWithName:rootObject:usingNameServer:@
serviceConnectionWithName_rootObject_usingNameServerSelector :: Selector
serviceConnectionWithName_rootObject_usingNameServerSelector = mkSelector "serviceConnectionWithName:rootObject:usingNameServer:"

-- | @Selector@ for @serviceConnectionWithName:rootObject:@
serviceConnectionWithName_rootObjectSelector :: Selector
serviceConnectionWithName_rootObjectSelector = mkSelector "serviceConnectionWithName:rootObject:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @addRequestMode:@
addRequestModeSelector :: Selector
addRequestModeSelector = mkSelector "addRequestMode:"

-- | @Selector@ for @removeRequestMode:@
removeRequestModeSelector :: Selector
removeRequestModeSelector = mkSelector "removeRequestMode:"

-- | @Selector@ for @registerName:@
registerNameSelector :: Selector
registerNameSelector = mkSelector "registerName:"

-- | @Selector@ for @registerName:withNameServer:@
registerName_withNameServerSelector :: Selector
registerName_withNameServerSelector = mkSelector "registerName:withNameServer:"

-- | @Selector@ for @connectionWithReceivePort:sendPort:@
connectionWithReceivePort_sendPortSelector :: Selector
connectionWithReceivePort_sendPortSelector = mkSelector "connectionWithReceivePort:sendPort:"

-- | @Selector@ for @currentConversation@
currentConversationSelector :: Selector
currentConversationSelector = mkSelector "currentConversation"

-- | @Selector@ for @initWithReceivePort:sendPort:@
initWithReceivePort_sendPortSelector :: Selector
initWithReceivePort_sendPortSelector = mkSelector "initWithReceivePort:sendPort:"

-- | @Selector@ for @enableMultipleThreads@
enableMultipleThreadsSelector :: Selector
enableMultipleThreadsSelector = mkSelector "enableMultipleThreads"

-- | @Selector@ for @addRunLoop:@
addRunLoopSelector :: Selector
addRunLoopSelector = mkSelector "addRunLoop:"

-- | @Selector@ for @removeRunLoop:@
removeRunLoopSelector :: Selector
removeRunLoopSelector = mkSelector "removeRunLoop:"

-- | @Selector@ for @runInNewThread@
runInNewThreadSelector :: Selector
runInNewThreadSelector = mkSelector "runInNewThread"

-- | @Selector@ for @dispatchWithComponents:@
dispatchWithComponentsSelector :: Selector
dispatchWithComponentsSelector = mkSelector "dispatchWithComponents:"

-- | @Selector@ for @statistics@
statisticsSelector :: Selector
statisticsSelector = mkSelector "statistics"

-- | @Selector@ for @requestTimeout@
requestTimeoutSelector :: Selector
requestTimeoutSelector = mkSelector "requestTimeout"

-- | @Selector@ for @setRequestTimeout:@
setRequestTimeoutSelector :: Selector
setRequestTimeoutSelector = mkSelector "setRequestTimeout:"

-- | @Selector@ for @replyTimeout@
replyTimeoutSelector :: Selector
replyTimeoutSelector = mkSelector "replyTimeout"

-- | @Selector@ for @setReplyTimeout:@
setReplyTimeoutSelector :: Selector
setReplyTimeoutSelector = mkSelector "setReplyTimeout:"

-- | @Selector@ for @rootObject@
rootObjectSelector :: Selector
rootObjectSelector = mkSelector "rootObject"

-- | @Selector@ for @setRootObject:@
setRootObjectSelector :: Selector
setRootObjectSelector = mkSelector "setRootObject:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @independentConversationQueueing@
independentConversationQueueingSelector :: Selector
independentConversationQueueingSelector = mkSelector "independentConversationQueueing"

-- | @Selector@ for @setIndependentConversationQueueing:@
setIndependentConversationQueueingSelector :: Selector
setIndependentConversationQueueingSelector = mkSelector "setIndependentConversationQueueing:"

-- | @Selector@ for @valid@
validSelector :: Selector
validSelector = mkSelector "valid"

-- | @Selector@ for @rootProxy@
rootProxySelector :: Selector
rootProxySelector = mkSelector "rootProxy"

-- | @Selector@ for @requestModes@
requestModesSelector :: Selector
requestModesSelector = mkSelector "requestModes"

-- | @Selector@ for @sendPort@
sendPortSelector :: Selector
sendPortSelector = mkSelector "sendPort"

-- | @Selector@ for @receivePort@
receivePortSelector :: Selector
receivePortSelector = mkSelector "receivePort"

-- | @Selector@ for @multipleThreadsEnabled@
multipleThreadsEnabledSelector :: Selector
multipleThreadsEnabledSelector = mkSelector "multipleThreadsEnabled"

-- | @Selector@ for @remoteObjects@
remoteObjectsSelector :: Selector
remoteObjectsSelector = mkSelector "remoteObjects"

-- | @Selector@ for @localObjects@
localObjectsSelector :: Selector
localObjectsSelector = mkSelector "localObjects"

