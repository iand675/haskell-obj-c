{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCondition@.
module ObjC.Foundation.NSCondition
  ( NSCondition
  , IsNSCondition(..)
  , wait
  , waitUntilDate
  , signal
  , broadcast
  , name
  , setName
  , broadcastSelector
  , nameSelector
  , setNameSelector
  , signalSelector
  , waitSelector
  , waitUntilDateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- wait@
wait :: IsNSCondition nsCondition => nsCondition -> IO ()
wait nsCondition =
  sendMessage nsCondition waitSelector

-- | @- waitUntilDate:@
waitUntilDate :: (IsNSCondition nsCondition, IsNSDate limit) => nsCondition -> limit -> IO Bool
waitUntilDate nsCondition limit =
  sendMessage nsCondition waitUntilDateSelector (toNSDate limit)

-- | @- signal@
signal :: IsNSCondition nsCondition => nsCondition -> IO ()
signal nsCondition =
  sendMessage nsCondition signalSelector

-- | @- broadcast@
broadcast :: IsNSCondition nsCondition => nsCondition -> IO ()
broadcast nsCondition =
  sendMessage nsCondition broadcastSelector

-- | @- name@
name :: IsNSCondition nsCondition => nsCondition -> IO (Id NSString)
name nsCondition =
  sendMessage nsCondition nameSelector

-- | @- setName:@
setName :: (IsNSCondition nsCondition, IsNSString value) => nsCondition -> value -> IO ()
setName nsCondition value =
  sendMessage nsCondition setNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @wait@
waitSelector :: Selector '[] ()
waitSelector = mkSelector "wait"

-- | @Selector@ for @waitUntilDate:@
waitUntilDateSelector :: Selector '[Id NSDate] Bool
waitUntilDateSelector = mkSelector "waitUntilDate:"

-- | @Selector@ for @signal@
signalSelector :: Selector '[] ()
signalSelector = mkSelector "signal"

-- | @Selector@ for @broadcast@
broadcastSelector :: Selector '[] ()
broadcastSelector = mkSelector "broadcast"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

