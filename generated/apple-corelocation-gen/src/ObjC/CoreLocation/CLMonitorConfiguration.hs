{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLMonitorConfiguration@.
module ObjC.CoreLocation.CLMonitorConfiguration
  ( CLMonitorConfiguration
  , IsCLMonitorConfiguration(..)
  , configWithMonitorName_queue_eventHandler
  , name
  , queue
  , eventHandler
  , configWithMonitorName_queue_eventHandlerSelector
  , eventHandlerSelector
  , nameSelector
  , queueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ configWithMonitorName:queue:eventHandler:@
configWithMonitorName_queue_eventHandler :: (IsNSString name, IsNSObject queue) => name -> queue -> Ptr () -> IO (Id CLMonitorConfiguration)
configWithMonitorName_queue_eventHandler name queue eventHandler =
  do
    cls' <- getRequiredClass "CLMonitorConfiguration"
    sendClassMessage cls' configWithMonitorName_queue_eventHandlerSelector (toNSString name) (toNSObject queue) eventHandler

-- | @- name@
name :: IsCLMonitorConfiguration clMonitorConfiguration => clMonitorConfiguration -> IO (Id NSString)
name clMonitorConfiguration =
  sendMessage clMonitorConfiguration nameSelector

-- | @- queue@
queue :: IsCLMonitorConfiguration clMonitorConfiguration => clMonitorConfiguration -> IO (Id NSObject)
queue clMonitorConfiguration =
  sendMessage clMonitorConfiguration queueSelector

-- | @- eventHandler@
eventHandler :: IsCLMonitorConfiguration clMonitorConfiguration => clMonitorConfiguration -> IO (Ptr ())
eventHandler clMonitorConfiguration =
  sendMessage clMonitorConfiguration eventHandlerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @configWithMonitorName:queue:eventHandler:@
configWithMonitorName_queue_eventHandlerSelector :: Selector '[Id NSString, Id NSObject, Ptr ()] (Id CLMonitorConfiguration)
configWithMonitorName_queue_eventHandlerSelector = mkSelector "configWithMonitorName:queue:eventHandler:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @queue@
queueSelector :: Selector '[] (Id NSObject)
queueSelector = mkSelector "queue"

-- | @Selector@ for @eventHandler@
eventHandlerSelector :: Selector '[] (Ptr ())
eventHandlerSelector = mkSelector "eventHandler"

