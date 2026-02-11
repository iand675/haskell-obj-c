{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSocketPortNameServer@.
module ObjC.Foundation.NSSocketPortNameServer
  ( NSSocketPortNameServer
  , IsNSSocketPortNameServer(..)
  , sharedInstance
  , portForName
  , portForName_host
  , registerPort_name
  , removePortForName
  , portForName_host_nameServerPortNumber
  , registerPort_name_nameServerPortNumber
  , defaultNameServerPortNumber
  , setDefaultNameServerPortNumber
  , sharedInstanceSelector
  , portForNameSelector
  , portForName_hostSelector
  , registerPort_nameSelector
  , removePortForNameSelector
  , portForName_host_nameServerPortNumberSelector
  , registerPort_name_nameServerPortNumberSelector
  , defaultNameServerPortNumberSelector
  , setDefaultNameServerPortNumberSelector


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

-- | @+ sharedInstance@
sharedInstance :: IO RawId
sharedInstance  =
  do
    cls' <- getRequiredClass "NSSocketPortNameServer"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "sharedInstance") (retPtr retVoid) []

-- | @- portForName:@
portForName :: (IsNSSocketPortNameServer nsSocketPortNameServer, IsNSString name) => nsSocketPortNameServer -> name -> IO (Id NSPort)
portForName nsSocketPortNameServer  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsSocketPortNameServer (mkSelector "portForName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- portForName:host:@
portForName_host :: (IsNSSocketPortNameServer nsSocketPortNameServer, IsNSString name, IsNSString host) => nsSocketPortNameServer -> name -> host -> IO (Id NSPort)
portForName_host nsSocketPortNameServer  name host =
withObjCPtr name $ \raw_name ->
  withObjCPtr host $ \raw_host ->
      sendMsg nsSocketPortNameServer (mkSelector "portForName:host:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_host :: Ptr ())] >>= retainedObject . castPtr

-- | @- registerPort:name:@
registerPort_name :: (IsNSSocketPortNameServer nsSocketPortNameServer, IsNSPort port, IsNSString name) => nsSocketPortNameServer -> port -> name -> IO Bool
registerPort_name nsSocketPortNameServer  port name =
withObjCPtr port $ \raw_port ->
  withObjCPtr name $ \raw_name ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSocketPortNameServer (mkSelector "registerPort:name:") retCULong [argPtr (castPtr raw_port :: Ptr ()), argPtr (castPtr raw_name :: Ptr ())]

-- | @- removePortForName:@
removePortForName :: (IsNSSocketPortNameServer nsSocketPortNameServer, IsNSString name) => nsSocketPortNameServer -> name -> IO Bool
removePortForName nsSocketPortNameServer  name =
withObjCPtr name $ \raw_name ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSocketPortNameServer (mkSelector "removePortForName:") retCULong [argPtr (castPtr raw_name :: Ptr ())]

-- | @- portForName:host:nameServerPortNumber:@
portForName_host_nameServerPortNumber :: (IsNSSocketPortNameServer nsSocketPortNameServer, IsNSString name, IsNSString host) => nsSocketPortNameServer -> name -> host -> CUShort -> IO (Id NSPort)
portForName_host_nameServerPortNumber nsSocketPortNameServer  name host portNumber =
withObjCPtr name $ \raw_name ->
  withObjCPtr host $ \raw_host ->
      sendMsg nsSocketPortNameServer (mkSelector "portForName:host:nameServerPortNumber:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_host :: Ptr ()), argCUInt (fromIntegral portNumber)] >>= retainedObject . castPtr

-- | @- registerPort:name:nameServerPortNumber:@
registerPort_name_nameServerPortNumber :: (IsNSSocketPortNameServer nsSocketPortNameServer, IsNSPort port, IsNSString name) => nsSocketPortNameServer -> port -> name -> CUShort -> IO Bool
registerPort_name_nameServerPortNumber nsSocketPortNameServer  port name portNumber =
withObjCPtr port $ \raw_port ->
  withObjCPtr name $ \raw_name ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSocketPortNameServer (mkSelector "registerPort:name:nameServerPortNumber:") retCULong [argPtr (castPtr raw_port :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argCUInt (fromIntegral portNumber)]

-- | @- defaultNameServerPortNumber@
defaultNameServerPortNumber :: IsNSSocketPortNameServer nsSocketPortNameServer => nsSocketPortNameServer -> IO CUShort
defaultNameServerPortNumber nsSocketPortNameServer  =
  fmap fromIntegral $ sendMsg nsSocketPortNameServer (mkSelector "defaultNameServerPortNumber") retCUInt []

-- | @- setDefaultNameServerPortNumber:@
setDefaultNameServerPortNumber :: IsNSSocketPortNameServer nsSocketPortNameServer => nsSocketPortNameServer -> CUShort -> IO ()
setDefaultNameServerPortNumber nsSocketPortNameServer  value =
  sendMsg nsSocketPortNameServer (mkSelector "setDefaultNameServerPortNumber:") retVoid [argCUInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedInstance@
sharedInstanceSelector :: Selector
sharedInstanceSelector = mkSelector "sharedInstance"

-- | @Selector@ for @portForName:@
portForNameSelector :: Selector
portForNameSelector = mkSelector "portForName:"

-- | @Selector@ for @portForName:host:@
portForName_hostSelector :: Selector
portForName_hostSelector = mkSelector "portForName:host:"

-- | @Selector@ for @registerPort:name:@
registerPort_nameSelector :: Selector
registerPort_nameSelector = mkSelector "registerPort:name:"

-- | @Selector@ for @removePortForName:@
removePortForNameSelector :: Selector
removePortForNameSelector = mkSelector "removePortForName:"

-- | @Selector@ for @portForName:host:nameServerPortNumber:@
portForName_host_nameServerPortNumberSelector :: Selector
portForName_host_nameServerPortNumberSelector = mkSelector "portForName:host:nameServerPortNumber:"

-- | @Selector@ for @registerPort:name:nameServerPortNumber:@
registerPort_name_nameServerPortNumberSelector :: Selector
registerPort_name_nameServerPortNumberSelector = mkSelector "registerPort:name:nameServerPortNumber:"

-- | @Selector@ for @defaultNameServerPortNumber@
defaultNameServerPortNumberSelector :: Selector
defaultNameServerPortNumberSelector = mkSelector "defaultNameServerPortNumber"

-- | @Selector@ for @setDefaultNameServerPortNumber:@
setDefaultNameServerPortNumberSelector :: Selector
setDefaultNameServerPortNumberSelector = mkSelector "setDefaultNameServerPortNumber:"

