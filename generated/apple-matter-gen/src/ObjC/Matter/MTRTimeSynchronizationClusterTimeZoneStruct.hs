{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTimeSynchronizationClusterTimeZoneStruct@.
module ObjC.Matter.MTRTimeSynchronizationClusterTimeZoneStruct
  ( MTRTimeSynchronizationClusterTimeZoneStruct
  , IsMTRTimeSynchronizationClusterTimeZoneStruct(..)
  , offset
  , setOffset
  , validAt
  , setValidAt
  , name
  , setName
  , nameSelector
  , offsetSelector
  , setNameSelector
  , setOffsetSelector
  , setValidAtSelector
  , validAtSelector


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
offset :: IsMTRTimeSynchronizationClusterTimeZoneStruct mtrTimeSynchronizationClusterTimeZoneStruct => mtrTimeSynchronizationClusterTimeZoneStruct -> IO (Id NSNumber)
offset mtrTimeSynchronizationClusterTimeZoneStruct =
  sendMessage mtrTimeSynchronizationClusterTimeZoneStruct offsetSelector

-- | @- setOffset:@
setOffset :: (IsMTRTimeSynchronizationClusterTimeZoneStruct mtrTimeSynchronizationClusterTimeZoneStruct, IsNSNumber value) => mtrTimeSynchronizationClusterTimeZoneStruct -> value -> IO ()
setOffset mtrTimeSynchronizationClusterTimeZoneStruct value =
  sendMessage mtrTimeSynchronizationClusterTimeZoneStruct setOffsetSelector (toNSNumber value)

-- | @- validAt@
validAt :: IsMTRTimeSynchronizationClusterTimeZoneStruct mtrTimeSynchronizationClusterTimeZoneStruct => mtrTimeSynchronizationClusterTimeZoneStruct -> IO (Id NSNumber)
validAt mtrTimeSynchronizationClusterTimeZoneStruct =
  sendMessage mtrTimeSynchronizationClusterTimeZoneStruct validAtSelector

-- | @- setValidAt:@
setValidAt :: (IsMTRTimeSynchronizationClusterTimeZoneStruct mtrTimeSynchronizationClusterTimeZoneStruct, IsNSNumber value) => mtrTimeSynchronizationClusterTimeZoneStruct -> value -> IO ()
setValidAt mtrTimeSynchronizationClusterTimeZoneStruct value =
  sendMessage mtrTimeSynchronizationClusterTimeZoneStruct setValidAtSelector (toNSNumber value)

-- | @- name@
name :: IsMTRTimeSynchronizationClusterTimeZoneStruct mtrTimeSynchronizationClusterTimeZoneStruct => mtrTimeSynchronizationClusterTimeZoneStruct -> IO (Id NSString)
name mtrTimeSynchronizationClusterTimeZoneStruct =
  sendMessage mtrTimeSynchronizationClusterTimeZoneStruct nameSelector

-- | @- setName:@
setName :: (IsMTRTimeSynchronizationClusterTimeZoneStruct mtrTimeSynchronizationClusterTimeZoneStruct, IsNSString value) => mtrTimeSynchronizationClusterTimeZoneStruct -> value -> IO ()
setName mtrTimeSynchronizationClusterTimeZoneStruct value =
  sendMessage mtrTimeSynchronizationClusterTimeZoneStruct setNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @offset@
offsetSelector :: Selector '[] (Id NSNumber)
offsetSelector = mkSelector "offset"

-- | @Selector@ for @setOffset:@
setOffsetSelector :: Selector '[Id NSNumber] ()
setOffsetSelector = mkSelector "setOffset:"

-- | @Selector@ for @validAt@
validAtSelector :: Selector '[] (Id NSNumber)
validAtSelector = mkSelector "validAt"

-- | @Selector@ for @setValidAt:@
setValidAtSelector :: Selector '[Id NSNumber] ()
setValidAtSelector = mkSelector "setValidAt:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

