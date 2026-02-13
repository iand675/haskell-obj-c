{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMessagePortNameServer@.
module ObjC.Foundation.NSMessagePortNameServer
  ( NSMessagePortNameServer
  , IsNSMessagePortNameServer(..)
  , sharedInstance
  , portForName
  , portForName_host
  , portForNameSelector
  , portForName_hostSelector
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
    cls' <- getRequiredClass "NSMessagePortNameServer"
    sendClassMessage cls' sharedInstanceSelector

-- | @- portForName:@
portForName :: (IsNSMessagePortNameServer nsMessagePortNameServer, IsNSString name) => nsMessagePortNameServer -> name -> IO (Id NSPort)
portForName nsMessagePortNameServer name =
  sendMessage nsMessagePortNameServer portForNameSelector (toNSString name)

-- | @- portForName:host:@
portForName_host :: (IsNSMessagePortNameServer nsMessagePortNameServer, IsNSString name, IsNSString host) => nsMessagePortNameServer -> name -> host -> IO (Id NSPort)
portForName_host nsMessagePortNameServer name host =
  sendMessage nsMessagePortNameServer portForName_hostSelector (toNSString name) (toNSString host)

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

