{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDistantObject@.
module ObjC.Foundation.NSDistantObject
  ( NSDistantObject
  , IsNSDistantObject(..)
  , proxyWithTarget_connection
  , initWithTarget_connection
  , proxyWithLocal_connection
  , initWithLocal_connection
  , initWithCoder
  , setProtocolForProxy
  , connectionForProxy
  , connectionForProxySelector
  , initWithCoderSelector
  , initWithLocal_connectionSelector
  , initWithTarget_connectionSelector
  , proxyWithLocal_connectionSelector
  , proxyWithTarget_connectionSelector
  , setProtocolForProxySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ proxyWithTarget:connection:@
proxyWithTarget_connection :: IsNSConnection connection => RawId -> connection -> IO RawId
proxyWithTarget_connection target connection =
  do
    cls' <- getRequiredClass "NSDistantObject"
    sendClassMessage cls' proxyWithTarget_connectionSelector target (toNSConnection connection)

-- | @- initWithTarget:connection:@
initWithTarget_connection :: (IsNSDistantObject nsDistantObject, IsNSConnection connection) => nsDistantObject -> RawId -> connection -> IO (Id NSDistantObject)
initWithTarget_connection nsDistantObject target connection =
  sendOwnedMessage nsDistantObject initWithTarget_connectionSelector target (toNSConnection connection)

-- | @+ proxyWithLocal:connection:@
proxyWithLocal_connection :: IsNSConnection connection => RawId -> connection -> IO RawId
proxyWithLocal_connection target connection =
  do
    cls' <- getRequiredClass "NSDistantObject"
    sendClassMessage cls' proxyWithLocal_connectionSelector target (toNSConnection connection)

-- | @- initWithLocal:connection:@
initWithLocal_connection :: (IsNSDistantObject nsDistantObject, IsNSConnection connection) => nsDistantObject -> RawId -> connection -> IO (Id NSDistantObject)
initWithLocal_connection nsDistantObject target connection =
  sendOwnedMessage nsDistantObject initWithLocal_connectionSelector target (toNSConnection connection)

-- | @- initWithCoder:@
initWithCoder :: (IsNSDistantObject nsDistantObject, IsNSCoder inCoder) => nsDistantObject -> inCoder -> IO (Id NSDistantObject)
initWithCoder nsDistantObject inCoder =
  sendOwnedMessage nsDistantObject initWithCoderSelector (toNSCoder inCoder)

-- | @- setProtocolForProxy:@
setProtocolForProxy :: IsNSDistantObject nsDistantObject => nsDistantObject -> RawId -> IO ()
setProtocolForProxy nsDistantObject proto =
  sendMessage nsDistantObject setProtocolForProxySelector proto

-- | @- connectionForProxy@
connectionForProxy :: IsNSDistantObject nsDistantObject => nsDistantObject -> IO (Id NSConnection)
connectionForProxy nsDistantObject =
  sendMessage nsDistantObject connectionForProxySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @proxyWithTarget:connection:@
proxyWithTarget_connectionSelector :: Selector '[RawId, Id NSConnection] RawId
proxyWithTarget_connectionSelector = mkSelector "proxyWithTarget:connection:"

-- | @Selector@ for @initWithTarget:connection:@
initWithTarget_connectionSelector :: Selector '[RawId, Id NSConnection] (Id NSDistantObject)
initWithTarget_connectionSelector = mkSelector "initWithTarget:connection:"

-- | @Selector@ for @proxyWithLocal:connection:@
proxyWithLocal_connectionSelector :: Selector '[RawId, Id NSConnection] RawId
proxyWithLocal_connectionSelector = mkSelector "proxyWithLocal:connection:"

-- | @Selector@ for @initWithLocal:connection:@
initWithLocal_connectionSelector :: Selector '[RawId, Id NSConnection] (Id NSDistantObject)
initWithLocal_connectionSelector = mkSelector "initWithLocal:connection:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSDistantObject)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @setProtocolForProxy:@
setProtocolForProxySelector :: Selector '[RawId] ()
setProtocolForProxySelector = mkSelector "setProtocolForProxy:"

-- | @Selector@ for @connectionForProxy@
connectionForProxySelector :: Selector '[] (Id NSConnection)
connectionForProxySelector = mkSelector "connectionForProxy"

