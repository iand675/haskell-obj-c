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
  , session
  , discoveryInfo
  , serviceType
  , initWithServiceType_discoveryInfo_sessionSelector
  , startSelector
  , stopSelector
  , sessionSelector
  , discoveryInfoSelector
  , serviceTypeSelector


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

import ObjC.MultipeerConnectivity.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithServiceType:discoveryInfo:session:@
initWithServiceType_discoveryInfo_session :: (IsMCAdvertiserAssistant mcAdvertiserAssistant, IsNSString serviceType, IsNSDictionary info, IsMCSession session) => mcAdvertiserAssistant -> serviceType -> info -> session -> IO (Id MCAdvertiserAssistant)
initWithServiceType_discoveryInfo_session mcAdvertiserAssistant  serviceType info session =
withObjCPtr serviceType $ \raw_serviceType ->
  withObjCPtr info $ \raw_info ->
    withObjCPtr session $ \raw_session ->
        sendMsg mcAdvertiserAssistant (mkSelector "initWithServiceType:discoveryInfo:session:") (retPtr retVoid) [argPtr (castPtr raw_serviceType :: Ptr ()), argPtr (castPtr raw_info :: Ptr ()), argPtr (castPtr raw_session :: Ptr ())] >>= ownedObject . castPtr

-- | @- start@
start :: IsMCAdvertiserAssistant mcAdvertiserAssistant => mcAdvertiserAssistant -> IO ()
start mcAdvertiserAssistant  =
  sendMsg mcAdvertiserAssistant (mkSelector "start") retVoid []

-- | @- stop@
stop :: IsMCAdvertiserAssistant mcAdvertiserAssistant => mcAdvertiserAssistant -> IO ()
stop mcAdvertiserAssistant  =
  sendMsg mcAdvertiserAssistant (mkSelector "stop") retVoid []

-- | @- session@
session :: IsMCAdvertiserAssistant mcAdvertiserAssistant => mcAdvertiserAssistant -> IO (Id MCSession)
session mcAdvertiserAssistant  =
  sendMsg mcAdvertiserAssistant (mkSelector "session") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- discoveryInfo@
discoveryInfo :: IsMCAdvertiserAssistant mcAdvertiserAssistant => mcAdvertiserAssistant -> IO (Id NSDictionary)
discoveryInfo mcAdvertiserAssistant  =
  sendMsg mcAdvertiserAssistant (mkSelector "discoveryInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- serviceType@
serviceType :: IsMCAdvertiserAssistant mcAdvertiserAssistant => mcAdvertiserAssistant -> IO (Id NSString)
serviceType mcAdvertiserAssistant  =
  sendMsg mcAdvertiserAssistant (mkSelector "serviceType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithServiceType:discoveryInfo:session:@
initWithServiceType_discoveryInfo_sessionSelector :: Selector
initWithServiceType_discoveryInfo_sessionSelector = mkSelector "initWithServiceType:discoveryInfo:session:"

-- | @Selector@ for @start@
startSelector :: Selector
startSelector = mkSelector "start"

-- | @Selector@ for @stop@
stopSelector :: Selector
stopSelector = mkSelector "stop"

-- | @Selector@ for @session@
sessionSelector :: Selector
sessionSelector = mkSelector "session"

-- | @Selector@ for @discoveryInfo@
discoveryInfoSelector :: Selector
discoveryInfoSelector = mkSelector "discoveryInfo"

-- | @Selector@ for @serviceType@
serviceTypeSelector :: Selector
serviceTypeSelector = mkSelector "serviceType"

