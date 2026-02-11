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
  , nameSelector
  , queueSelector
  , eventHandlerSelector


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

import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ configWithMonitorName:queue:eventHandler:@
configWithMonitorName_queue_eventHandler :: (IsNSString name, IsNSObject queue) => name -> queue -> Ptr () -> IO (Id CLMonitorConfiguration)
configWithMonitorName_queue_eventHandler name queue eventHandler =
  do
    cls' <- getRequiredClass "CLMonitorConfiguration"
    withObjCPtr name $ \raw_name ->
      withObjCPtr queue $ \raw_queue ->
        sendClassMsg cls' (mkSelector "configWithMonitorName:queue:eventHandler:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr eventHandler :: Ptr ())] >>= retainedObject . castPtr

-- | @- name@
name :: IsCLMonitorConfiguration clMonitorConfiguration => clMonitorConfiguration -> IO (Id NSString)
name clMonitorConfiguration  =
  sendMsg clMonitorConfiguration (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- queue@
queue :: IsCLMonitorConfiguration clMonitorConfiguration => clMonitorConfiguration -> IO (Id NSObject)
queue clMonitorConfiguration  =
  sendMsg clMonitorConfiguration (mkSelector "queue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- eventHandler@
eventHandler :: IsCLMonitorConfiguration clMonitorConfiguration => clMonitorConfiguration -> IO (Ptr ())
eventHandler clMonitorConfiguration  =
  fmap castPtr $ sendMsg clMonitorConfiguration (mkSelector "eventHandler") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @configWithMonitorName:queue:eventHandler:@
configWithMonitorName_queue_eventHandlerSelector :: Selector
configWithMonitorName_queue_eventHandlerSelector = mkSelector "configWithMonitorName:queue:eventHandler:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @queue@
queueSelector :: Selector
queueSelector = mkSelector "queue"

-- | @Selector@ for @eventHandler@
eventHandlerSelector :: Selector
eventHandlerSelector = mkSelector "eventHandler"

