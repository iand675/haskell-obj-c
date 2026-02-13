{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRValveConfigurationAndControlClusterValveStateChangedEvent@.
module ObjC.Matter.MTRValveConfigurationAndControlClusterValveStateChangedEvent
  ( MTRValveConfigurationAndControlClusterValveStateChangedEvent
  , IsMTRValveConfigurationAndControlClusterValveStateChangedEvent(..)
  , valveState
  , setValveState
  , valveLevel
  , setValveLevel
  , setValveLevelSelector
  , setValveStateSelector
  , valveLevelSelector
  , valveStateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- valveState@
valveState :: IsMTRValveConfigurationAndControlClusterValveStateChangedEvent mtrValveConfigurationAndControlClusterValveStateChangedEvent => mtrValveConfigurationAndControlClusterValveStateChangedEvent -> IO (Id NSNumber)
valveState mtrValveConfigurationAndControlClusterValveStateChangedEvent =
  sendMessage mtrValveConfigurationAndControlClusterValveStateChangedEvent valveStateSelector

-- | @- setValveState:@
setValveState :: (IsMTRValveConfigurationAndControlClusterValveStateChangedEvent mtrValveConfigurationAndControlClusterValveStateChangedEvent, IsNSNumber value) => mtrValveConfigurationAndControlClusterValveStateChangedEvent -> value -> IO ()
setValveState mtrValveConfigurationAndControlClusterValveStateChangedEvent value =
  sendMessage mtrValveConfigurationAndControlClusterValveStateChangedEvent setValveStateSelector (toNSNumber value)

-- | @- valveLevel@
valveLevel :: IsMTRValveConfigurationAndControlClusterValveStateChangedEvent mtrValveConfigurationAndControlClusterValveStateChangedEvent => mtrValveConfigurationAndControlClusterValveStateChangedEvent -> IO (Id NSNumber)
valveLevel mtrValveConfigurationAndControlClusterValveStateChangedEvent =
  sendMessage mtrValveConfigurationAndControlClusterValveStateChangedEvent valveLevelSelector

-- | @- setValveLevel:@
setValveLevel :: (IsMTRValveConfigurationAndControlClusterValveStateChangedEvent mtrValveConfigurationAndControlClusterValveStateChangedEvent, IsNSNumber value) => mtrValveConfigurationAndControlClusterValveStateChangedEvent -> value -> IO ()
setValveLevel mtrValveConfigurationAndControlClusterValveStateChangedEvent value =
  sendMessage mtrValveConfigurationAndControlClusterValveStateChangedEvent setValveLevelSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valveState@
valveStateSelector :: Selector '[] (Id NSNumber)
valveStateSelector = mkSelector "valveState"

-- | @Selector@ for @setValveState:@
setValveStateSelector :: Selector '[Id NSNumber] ()
setValveStateSelector = mkSelector "setValveState:"

-- | @Selector@ for @valveLevel@
valveLevelSelector :: Selector '[] (Id NSNumber)
valveLevelSelector = mkSelector "valveLevel"

-- | @Selector@ for @setValveLevel:@
setValveLevelSelector :: Selector '[Id NSNumber] ()
setValveLevelSelector = mkSelector "setValveLevel:"

