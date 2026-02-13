{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMachBootstrapServer@.
module ObjC.Foundation.NSMachBootstrapServer
  ( NSMachBootstrapServer
  , IsNSMachBootstrapServer(..)
  , sharedInstance
  , portForName
  , portForName_host
  , registerPort_name
  , servicePortWithName
  , portForNameSelector
  , portForName_hostSelector
  , registerPort_nameSelector
  , servicePortWithNameSelector
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
    cls' <- getRequiredClass "NSMachBootstrapServer"
    sendClassMessage cls' sharedInstanceSelector

-- | @- portForName:@
portForName :: (IsNSMachBootstrapServer nsMachBootstrapServer, IsNSString name) => nsMachBootstrapServer -> name -> IO (Id NSPort)
portForName nsMachBootstrapServer name =
  sendMessage nsMachBootstrapServer portForNameSelector (toNSString name)

-- | @- portForName:host:@
portForName_host :: (IsNSMachBootstrapServer nsMachBootstrapServer, IsNSString name, IsNSString host) => nsMachBootstrapServer -> name -> host -> IO (Id NSPort)
portForName_host nsMachBootstrapServer name host =
  sendMessage nsMachBootstrapServer portForName_hostSelector (toNSString name) (toNSString host)

-- | @- registerPort:name:@
registerPort_name :: (IsNSMachBootstrapServer nsMachBootstrapServer, IsNSPort port, IsNSString name) => nsMachBootstrapServer -> port -> name -> IO Bool
registerPort_name nsMachBootstrapServer port name =
  sendMessage nsMachBootstrapServer registerPort_nameSelector (toNSPort port) (toNSString name)

-- | @- servicePortWithName:@
servicePortWithName :: (IsNSMachBootstrapServer nsMachBootstrapServer, IsNSString name) => nsMachBootstrapServer -> name -> IO (Id NSPort)
servicePortWithName nsMachBootstrapServer name =
  sendMessage nsMachBootstrapServer servicePortWithNameSelector (toNSString name)

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

-- | @Selector@ for @servicePortWithName:@
servicePortWithNameSelector :: Selector '[Id NSString] (Id NSPort)
servicePortWithNameSelector = mkSelector "servicePortWithName:"

