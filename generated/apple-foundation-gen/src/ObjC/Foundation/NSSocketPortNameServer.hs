{-# LANGUAGE DataKinds #-}
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
  , defaultNameServerPortNumberSelector
  , portForNameSelector
  , portForName_hostSelector
  , portForName_host_nameServerPortNumberSelector
  , registerPort_nameSelector
  , registerPort_name_nameServerPortNumberSelector
  , removePortForNameSelector
  , setDefaultNameServerPortNumberSelector
  , sharedInstanceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ sharedInstance@
sharedInstance :: IO RawId
sharedInstance  =
  do
    cls' <- getRequiredClass "NSSocketPortNameServer"
    sendClassMessage cls' sharedInstanceSelector

-- | @- portForName:@
portForName :: (IsNSSocketPortNameServer nsSocketPortNameServer, IsNSString name) => nsSocketPortNameServer -> name -> IO (Id NSPort)
portForName nsSocketPortNameServer name =
  sendMessage nsSocketPortNameServer portForNameSelector (toNSString name)

-- | @- portForName:host:@
portForName_host :: (IsNSSocketPortNameServer nsSocketPortNameServer, IsNSString name, IsNSString host) => nsSocketPortNameServer -> name -> host -> IO (Id NSPort)
portForName_host nsSocketPortNameServer name host =
  sendMessage nsSocketPortNameServer portForName_hostSelector (toNSString name) (toNSString host)

-- | @- registerPort:name:@
registerPort_name :: (IsNSSocketPortNameServer nsSocketPortNameServer, IsNSPort port, IsNSString name) => nsSocketPortNameServer -> port -> name -> IO Bool
registerPort_name nsSocketPortNameServer port name =
  sendMessage nsSocketPortNameServer registerPort_nameSelector (toNSPort port) (toNSString name)

-- | @- removePortForName:@
removePortForName :: (IsNSSocketPortNameServer nsSocketPortNameServer, IsNSString name) => nsSocketPortNameServer -> name -> IO Bool
removePortForName nsSocketPortNameServer name =
  sendMessage nsSocketPortNameServer removePortForNameSelector (toNSString name)

-- | @- portForName:host:nameServerPortNumber:@
portForName_host_nameServerPortNumber :: (IsNSSocketPortNameServer nsSocketPortNameServer, IsNSString name, IsNSString host) => nsSocketPortNameServer -> name -> host -> CUShort -> IO (Id NSPort)
portForName_host_nameServerPortNumber nsSocketPortNameServer name host portNumber =
  sendMessage nsSocketPortNameServer portForName_host_nameServerPortNumberSelector (toNSString name) (toNSString host) portNumber

-- | @- registerPort:name:nameServerPortNumber:@
registerPort_name_nameServerPortNumber :: (IsNSSocketPortNameServer nsSocketPortNameServer, IsNSPort port, IsNSString name) => nsSocketPortNameServer -> port -> name -> CUShort -> IO Bool
registerPort_name_nameServerPortNumber nsSocketPortNameServer port name portNumber =
  sendMessage nsSocketPortNameServer registerPort_name_nameServerPortNumberSelector (toNSPort port) (toNSString name) portNumber

-- | @- defaultNameServerPortNumber@
defaultNameServerPortNumber :: IsNSSocketPortNameServer nsSocketPortNameServer => nsSocketPortNameServer -> IO CUShort
defaultNameServerPortNumber nsSocketPortNameServer =
  sendMessage nsSocketPortNameServer defaultNameServerPortNumberSelector

-- | @- setDefaultNameServerPortNumber:@
setDefaultNameServerPortNumber :: IsNSSocketPortNameServer nsSocketPortNameServer => nsSocketPortNameServer -> CUShort -> IO ()
setDefaultNameServerPortNumber nsSocketPortNameServer value =
  sendMessage nsSocketPortNameServer setDefaultNameServerPortNumberSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedInstance@
sharedInstanceSelector :: Selector '[] RawId
sharedInstanceSelector = mkSelector "sharedInstance"

-- | @Selector@ for @portForName:@
portForNameSelector :: Selector '[Id NSString] (Id NSPort)
portForNameSelector = mkSelector "portForName:"

-- | @Selector@ for @portForName:host:@
portForName_hostSelector :: Selector '[Id NSString, Id NSString] (Id NSPort)
portForName_hostSelector = mkSelector "portForName:host:"

-- | @Selector@ for @registerPort:name:@
registerPort_nameSelector :: Selector '[Id NSPort, Id NSString] Bool
registerPort_nameSelector = mkSelector "registerPort:name:"

-- | @Selector@ for @removePortForName:@
removePortForNameSelector :: Selector '[Id NSString] Bool
removePortForNameSelector = mkSelector "removePortForName:"

-- | @Selector@ for @portForName:host:nameServerPortNumber:@
portForName_host_nameServerPortNumberSelector :: Selector '[Id NSString, Id NSString, CUShort] (Id NSPort)
portForName_host_nameServerPortNumberSelector = mkSelector "portForName:host:nameServerPortNumber:"

-- | @Selector@ for @registerPort:name:nameServerPortNumber:@
registerPort_name_nameServerPortNumberSelector :: Selector '[Id NSPort, Id NSString, CUShort] Bool
registerPort_name_nameServerPortNumberSelector = mkSelector "registerPort:name:nameServerPortNumber:"

-- | @Selector@ for @defaultNameServerPortNumber@
defaultNameServerPortNumberSelector :: Selector '[] CUShort
defaultNameServerPortNumberSelector = mkSelector "defaultNameServerPortNumber"

-- | @Selector@ for @setDefaultNameServerPortNumber:@
setDefaultNameServerPortNumberSelector :: Selector '[CUShort] ()
setDefaultNameServerPortNumberSelector = mkSelector "setDefaultNameServerPortNumber:"

