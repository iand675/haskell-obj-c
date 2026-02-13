{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSConditionLock@.
module ObjC.Foundation.NSConditionLock
  ( NSConditionLock
  , IsNSConditionLock(..)
  , initWithCondition
  , lockWhenCondition
  , tryLock
  , tryLockWhenCondition
  , unlockWithCondition
  , lockBeforeDate
  , lockWhenCondition_beforeDate
  , condition
  , name
  , setName
  , conditionSelector
  , initWithConditionSelector
  , lockBeforeDateSelector
  , lockWhenConditionSelector
  , lockWhenCondition_beforeDateSelector
  , nameSelector
  , setNameSelector
  , tryLockSelector
  , tryLockWhenConditionSelector
  , unlockWithConditionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithCondition:@
initWithCondition :: IsNSConditionLock nsConditionLock => nsConditionLock -> CLong -> IO (Id NSConditionLock)
initWithCondition nsConditionLock condition =
  sendOwnedMessage nsConditionLock initWithConditionSelector condition

-- | @- lockWhenCondition:@
lockWhenCondition :: IsNSConditionLock nsConditionLock => nsConditionLock -> CLong -> IO ()
lockWhenCondition nsConditionLock condition =
  sendMessage nsConditionLock lockWhenConditionSelector condition

-- | @- tryLock@
tryLock :: IsNSConditionLock nsConditionLock => nsConditionLock -> IO Bool
tryLock nsConditionLock =
  sendMessage nsConditionLock tryLockSelector

-- | @- tryLockWhenCondition:@
tryLockWhenCondition :: IsNSConditionLock nsConditionLock => nsConditionLock -> CLong -> IO Bool
tryLockWhenCondition nsConditionLock condition =
  sendMessage nsConditionLock tryLockWhenConditionSelector condition

-- | @- unlockWithCondition:@
unlockWithCondition :: IsNSConditionLock nsConditionLock => nsConditionLock -> CLong -> IO ()
unlockWithCondition nsConditionLock condition =
  sendMessage nsConditionLock unlockWithConditionSelector condition

-- | @- lockBeforeDate:@
lockBeforeDate :: (IsNSConditionLock nsConditionLock, IsNSDate limit) => nsConditionLock -> limit -> IO Bool
lockBeforeDate nsConditionLock limit =
  sendMessage nsConditionLock lockBeforeDateSelector (toNSDate limit)

-- | @- lockWhenCondition:beforeDate:@
lockWhenCondition_beforeDate :: (IsNSConditionLock nsConditionLock, IsNSDate limit) => nsConditionLock -> CLong -> limit -> IO Bool
lockWhenCondition_beforeDate nsConditionLock condition limit =
  sendMessage nsConditionLock lockWhenCondition_beforeDateSelector condition (toNSDate limit)

-- | @- condition@
condition :: IsNSConditionLock nsConditionLock => nsConditionLock -> IO CLong
condition nsConditionLock =
  sendMessage nsConditionLock conditionSelector

-- | @- name@
name :: IsNSConditionLock nsConditionLock => nsConditionLock -> IO (Id NSString)
name nsConditionLock =
  sendMessage nsConditionLock nameSelector

-- | @- setName:@
setName :: (IsNSConditionLock nsConditionLock, IsNSString value) => nsConditionLock -> value -> IO ()
setName nsConditionLock value =
  sendMessage nsConditionLock setNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCondition:@
initWithConditionSelector :: Selector '[CLong] (Id NSConditionLock)
initWithConditionSelector = mkSelector "initWithCondition:"

-- | @Selector@ for @lockWhenCondition:@
lockWhenConditionSelector :: Selector '[CLong] ()
lockWhenConditionSelector = mkSelector "lockWhenCondition:"

-- | @Selector@ for @tryLock@
tryLockSelector :: Selector '[] Bool
tryLockSelector = mkSelector "tryLock"

-- | @Selector@ for @tryLockWhenCondition:@
tryLockWhenConditionSelector :: Selector '[CLong] Bool
tryLockWhenConditionSelector = mkSelector "tryLockWhenCondition:"

-- | @Selector@ for @unlockWithCondition:@
unlockWithConditionSelector :: Selector '[CLong] ()
unlockWithConditionSelector = mkSelector "unlockWithCondition:"

-- | @Selector@ for @lockBeforeDate:@
lockBeforeDateSelector :: Selector '[Id NSDate] Bool
lockBeforeDateSelector = mkSelector "lockBeforeDate:"

-- | @Selector@ for @lockWhenCondition:beforeDate:@
lockWhenCondition_beforeDateSelector :: Selector '[CLong, Id NSDate] Bool
lockWhenCondition_beforeDateSelector = mkSelector "lockWhenCondition:beforeDate:"

-- | @Selector@ for @condition@
conditionSelector :: Selector '[] CLong
conditionSelector = mkSelector "condition"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

