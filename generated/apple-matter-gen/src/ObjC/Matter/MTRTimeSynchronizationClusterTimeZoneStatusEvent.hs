{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTimeSynchronizationClusterTimeZoneStatusEvent@.
module ObjC.Matter.MTRTimeSynchronizationClusterTimeZoneStatusEvent
  ( MTRTimeSynchronizationClusterTimeZoneStatusEvent
  , IsMTRTimeSynchronizationClusterTimeZoneStatusEvent(..)
  , offset
  , setOffset
  , name
  , setName
  , nameSelector
  , offsetSelector
  , setNameSelector
  , setOffsetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- offset@
offset :: IsMTRTimeSynchronizationClusterTimeZoneStatusEvent mtrTimeSynchronizationClusterTimeZoneStatusEvent => mtrTimeSynchronizationClusterTimeZoneStatusEvent -> IO (Id NSNumber)
offset mtrTimeSynchronizationClusterTimeZoneStatusEvent =
  sendMessage mtrTimeSynchronizationClusterTimeZoneStatusEvent offsetSelector

-- | @- setOffset:@
setOffset :: (IsMTRTimeSynchronizationClusterTimeZoneStatusEvent mtrTimeSynchronizationClusterTimeZoneStatusEvent, IsNSNumber value) => mtrTimeSynchronizationClusterTimeZoneStatusEvent -> value -> IO ()
setOffset mtrTimeSynchronizationClusterTimeZoneStatusEvent value =
  sendMessage mtrTimeSynchronizationClusterTimeZoneStatusEvent setOffsetSelector (toNSNumber value)

-- | @- name@
name :: IsMTRTimeSynchronizationClusterTimeZoneStatusEvent mtrTimeSynchronizationClusterTimeZoneStatusEvent => mtrTimeSynchronizationClusterTimeZoneStatusEvent -> IO (Id NSString)
name mtrTimeSynchronizationClusterTimeZoneStatusEvent =
  sendMessage mtrTimeSynchronizationClusterTimeZoneStatusEvent nameSelector

-- | @- setName:@
setName :: (IsMTRTimeSynchronizationClusterTimeZoneStatusEvent mtrTimeSynchronizationClusterTimeZoneStatusEvent, IsNSString value) => mtrTimeSynchronizationClusterTimeZoneStatusEvent -> value -> IO ()
setName mtrTimeSynchronizationClusterTimeZoneStatusEvent value =
  sendMessage mtrTimeSynchronizationClusterTimeZoneStatusEvent setNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @offset@
offsetSelector :: Selector '[] (Id NSNumber)
offsetSelector = mkSelector "offset"

-- | @Selector@ for @setOffset:@
setOffsetSelector :: Selector '[Id NSNumber] ()
setOffsetSelector = mkSelector "setOffset:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

