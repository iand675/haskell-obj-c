{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MCAdvertiserAssistant@.
module ObjC.MultipeerConnectivity.MCAdvertiserAssistant
  ( MCAdvertiserAssistant
  , IsMCAdvertiserAssistant(..)
  , initWithServiceType_discoveryInfo_session
  , start
  , stop
  , delegate
  , setDelegate
  , session
  , discoveryInfo
  , serviceType
  , delegateSelector
  , discoveryInfoSelector
  , initWithServiceType_discoveryInfo_sessionSelector
  , serviceTypeSelector
  , sessionSelector
  , setDelegateSelector
  , startSelector
  , stopSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MultipeerConnectivity.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithServiceType:discoveryInfo:session:@
initWithServiceType_discoveryInfo_session :: (IsMCAdvertiserAssistant mcAdvertiserAssistant, IsNSString serviceType, IsNSDictionary info, IsMCSession session) => mcAdvertiserAssistant -> serviceType -> info -> session -> IO (Id MCAdvertiserAssistant)
initWithServiceType_discoveryInfo_session mcAdvertiserAssistant serviceType info session =
  sendOwnedMessage mcAdvertiserAssistant initWithServiceType_discoveryInfo_sessionSelector (toNSString serviceType) (toNSDictionary info) (toMCSession session)

-- | @- start@
start :: IsMCAdvertiserAssistant mcAdvertiserAssistant => mcAdvertiserAssistant -> IO ()
start mcAdvertiserAssistant =
  sendMessage mcAdvertiserAssistant startSelector

-- | @- stop@
stop :: IsMCAdvertiserAssistant mcAdvertiserAssistant => mcAdvertiserAssistant -> IO ()
stop mcAdvertiserAssistant =
  sendMessage mcAdvertiserAssistant stopSelector

-- | @- delegate@
delegate :: IsMCAdvertiserAssistant mcAdvertiserAssistant => mcAdvertiserAssistant -> IO RawId
delegate mcAdvertiserAssistant =
  sendMessage mcAdvertiserAssistant delegateSelector

-- | @- setDelegate:@
setDelegate :: IsMCAdvertiserAssistant mcAdvertiserAssistant => mcAdvertiserAssistant -> RawId -> IO ()
setDelegate mcAdvertiserAssistant value =
  sendMessage mcAdvertiserAssistant setDelegateSelector value

-- | @- session@
session :: IsMCAdvertiserAssistant mcAdvertiserAssistant => mcAdvertiserAssistant -> IO (Id MCSession)
session mcAdvertiserAssistant =
  sendMessage mcAdvertiserAssistant sessionSelector

-- | @- discoveryInfo@
discoveryInfo :: IsMCAdvertiserAssistant mcAdvertiserAssistant => mcAdvertiserAssistant -> IO (Id NSDictionary)
discoveryInfo mcAdvertiserAssistant =
  sendMessage mcAdvertiserAssistant discoveryInfoSelector

-- | @- serviceType@
serviceType :: IsMCAdvertiserAssistant mcAdvertiserAssistant => mcAdvertiserAssistant -> IO (Id NSString)
serviceType mcAdvertiserAssistant =
  sendMessage mcAdvertiserAssistant serviceTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithServiceType:discoveryInfo:session:@
initWithServiceType_discoveryInfo_sessionSelector :: Selector '[Id NSString, Id NSDictionary, Id MCSession] (Id MCAdvertiserAssistant)
initWithServiceType_discoveryInfo_sessionSelector = mkSelector "initWithServiceType:discoveryInfo:session:"

-- | @Selector@ for @start@
startSelector :: Selector '[] ()
startSelector = mkSelector "start"

-- | @Selector@ for @stop@
stopSelector :: Selector '[] ()
stopSelector = mkSelector "stop"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @session@
sessionSelector :: Selector '[] (Id MCSession)
sessionSelector = mkSelector "session"

-- | @Selector@ for @discoveryInfo@
discoveryInfoSelector :: Selector '[] (Id NSDictionary)
discoveryInfoSelector = mkSelector "discoveryInfo"

-- | @Selector@ for @serviceType@
serviceTypeSelector :: Selector '[] (Id NSString)
serviceTypeSelector = mkSelector "serviceType"

