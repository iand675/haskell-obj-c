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
  , proxyWithTarget_connectionSelector
  , initWithTarget_connectionSelector
  , proxyWithLocal_connectionSelector
  , initWithLocal_connectionSelector
  , initWithCoderSelector
  , setProtocolForProxySelector
  , connectionForProxySelector


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

-- | @+ proxyWithTarget:connection:@
proxyWithTarget_connection :: IsNSConnection connection => RawId -> connection -> IO RawId
proxyWithTarget_connection target connection =
  do
    cls' <- getRequiredClass "NSDistantObject"
    withObjCPtr connection $ \raw_connection ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "proxyWithTarget:connection:") (retPtr retVoid) [argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (castPtr raw_connection :: Ptr ())]

-- | @- initWithTarget:connection:@
initWithTarget_connection :: (IsNSDistantObject nsDistantObject, IsNSConnection connection) => nsDistantObject -> RawId -> connection -> IO (Id NSDistantObject)
initWithTarget_connection nsDistantObject  target connection =
withObjCPtr connection $ \raw_connection ->
    sendMsg nsDistantObject (mkSelector "initWithTarget:connection:") (retPtr retVoid) [argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (castPtr raw_connection :: Ptr ())] >>= ownedObject . castPtr

-- | @+ proxyWithLocal:connection:@
proxyWithLocal_connection :: IsNSConnection connection => RawId -> connection -> IO RawId
proxyWithLocal_connection target connection =
  do
    cls' <- getRequiredClass "NSDistantObject"
    withObjCPtr connection $ \raw_connection ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "proxyWithLocal:connection:") (retPtr retVoid) [argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (castPtr raw_connection :: Ptr ())]

-- | @- initWithLocal:connection:@
initWithLocal_connection :: (IsNSDistantObject nsDistantObject, IsNSConnection connection) => nsDistantObject -> RawId -> connection -> IO (Id NSDistantObject)
initWithLocal_connection nsDistantObject  target connection =
withObjCPtr connection $ \raw_connection ->
    sendMsg nsDistantObject (mkSelector "initWithLocal:connection:") (retPtr retVoid) [argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (castPtr raw_connection :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSDistantObject nsDistantObject, IsNSCoder inCoder) => nsDistantObject -> inCoder -> IO (Id NSDistantObject)
initWithCoder nsDistantObject  inCoder =
withObjCPtr inCoder $ \raw_inCoder ->
    sendMsg nsDistantObject (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_inCoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- setProtocolForProxy:@
setProtocolForProxy :: IsNSDistantObject nsDistantObject => nsDistantObject -> RawId -> IO ()
setProtocolForProxy nsDistantObject  proto =
  sendMsg nsDistantObject (mkSelector "setProtocolForProxy:") retVoid [argPtr (castPtr (unRawId proto) :: Ptr ())]

-- | @- connectionForProxy@
connectionForProxy :: IsNSDistantObject nsDistantObject => nsDistantObject -> IO (Id NSConnection)
connectionForProxy nsDistantObject  =
  sendMsg nsDistantObject (mkSelector "connectionForProxy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @proxyWithTarget:connection:@
proxyWithTarget_connectionSelector :: Selector
proxyWithTarget_connectionSelector = mkSelector "proxyWithTarget:connection:"

-- | @Selector@ for @initWithTarget:connection:@
initWithTarget_connectionSelector :: Selector
initWithTarget_connectionSelector = mkSelector "initWithTarget:connection:"

-- | @Selector@ for @proxyWithLocal:connection:@
proxyWithLocal_connectionSelector :: Selector
proxyWithLocal_connectionSelector = mkSelector "proxyWithLocal:connection:"

-- | @Selector@ for @initWithLocal:connection:@
initWithLocal_connectionSelector :: Selector
initWithLocal_connectionSelector = mkSelector "initWithLocal:connection:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @setProtocolForProxy:@
setProtocolForProxySelector :: Selector
setProtocolForProxySelector = mkSelector "setProtocolForProxy:"

-- | @Selector@ for @connectionForProxy@
connectionForProxySelector :: Selector
connectionForProxySelector = mkSelector "connectionForProxy"

