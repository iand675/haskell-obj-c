{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPortNameServer@.
module ObjC.Foundation.NSPortNameServer
  ( NSPortNameServer
  , IsNSPortNameServer(..)
  , systemDefaultPortNameServer
  , portForName
  , portForName_host
  , registerPort_name
  , removePortForName
  , portForNameSelector
  , portForName_hostSelector
  , registerPort_nameSelector
  , removePortForNameSelector
  , systemDefaultPortNameServerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ systemDefaultPortNameServer@
systemDefaultPortNameServer :: IO (Id NSPortNameServer)
systemDefaultPortNameServer  =
  do
    cls' <- getRequiredClass "NSPortNameServer"
    sendClassMessage cls' systemDefaultPortNameServerSelector

-- | @- portForName:@
portForName :: (IsNSPortNameServer nsPortNameServer, IsNSString name) => nsPortNameServer -> name -> IO (Id NSPort)
portForName nsPortNameServer name =
  sendMessage nsPortNameServer portForNameSelector (toNSString name)

-- | @- portForName:host:@
portForName_host :: (IsNSPortNameServer nsPortNameServer, IsNSString name, IsNSString host) => nsPortNameServer -> name -> host -> IO (Id NSPort)
portForName_host nsPortNameServer name host =
  sendMessage nsPortNameServer portForName_hostSelector (toNSString name) (toNSString host)

-- | @- registerPort:name:@
registerPort_name :: (IsNSPortNameServer nsPortNameServer, IsNSPort port, IsNSString name) => nsPortNameServer -> port -> name -> IO Bool
registerPort_name nsPortNameServer port name =
  sendMessage nsPortNameServer registerPort_nameSelector (toNSPort port) (toNSString name)

-- | @- removePortForName:@
removePortForName :: (IsNSPortNameServer nsPortNameServer, IsNSString name) => nsPortNameServer -> name -> IO Bool
removePortForName nsPortNameServer name =
  sendMessage nsPortNameServer removePortForNameSelector (toNSString name)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @systemDefaultPortNameServer@
systemDefaultPortNameServerSelector :: Selector '[] (Id NSPortNameServer)
systemDefaultPortNameServerSelector = mkSelector "systemDefaultPortNameServer"

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

