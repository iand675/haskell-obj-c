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
  , initWithConditionSelector
  , lockWhenConditionSelector
  , tryLockSelector
  , tryLockWhenConditionSelector
  , unlockWithConditionSelector
  , lockBeforeDateSelector
  , lockWhenCondition_beforeDateSelector
  , conditionSelector
  , nameSelector
  , setNameSelector


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

import ObjC.Foundation.Internal.Classes

-- | @- initWithCondition:@
initWithCondition :: IsNSConditionLock nsConditionLock => nsConditionLock -> CLong -> IO (Id NSConditionLock)
initWithCondition nsConditionLock  condition =
  sendMsg nsConditionLock (mkSelector "initWithCondition:") (retPtr retVoid) [argCLong (fromIntegral condition)] >>= ownedObject . castPtr

-- | @- lockWhenCondition:@
lockWhenCondition :: IsNSConditionLock nsConditionLock => nsConditionLock -> CLong -> IO ()
lockWhenCondition nsConditionLock  condition =
  sendMsg nsConditionLock (mkSelector "lockWhenCondition:") retVoid [argCLong (fromIntegral condition)]

-- | @- tryLock@
tryLock :: IsNSConditionLock nsConditionLock => nsConditionLock -> IO Bool
tryLock nsConditionLock  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsConditionLock (mkSelector "tryLock") retCULong []

-- | @- tryLockWhenCondition:@
tryLockWhenCondition :: IsNSConditionLock nsConditionLock => nsConditionLock -> CLong -> IO Bool
tryLockWhenCondition nsConditionLock  condition =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsConditionLock (mkSelector "tryLockWhenCondition:") retCULong [argCLong (fromIntegral condition)]

-- | @- unlockWithCondition:@
unlockWithCondition :: IsNSConditionLock nsConditionLock => nsConditionLock -> CLong -> IO ()
unlockWithCondition nsConditionLock  condition =
  sendMsg nsConditionLock (mkSelector "unlockWithCondition:") retVoid [argCLong (fromIntegral condition)]

-- | @- lockBeforeDate:@
lockBeforeDate :: (IsNSConditionLock nsConditionLock, IsNSDate limit) => nsConditionLock -> limit -> IO Bool
lockBeforeDate nsConditionLock  limit =
withObjCPtr limit $ \raw_limit ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsConditionLock (mkSelector "lockBeforeDate:") retCULong [argPtr (castPtr raw_limit :: Ptr ())]

-- | @- lockWhenCondition:beforeDate:@
lockWhenCondition_beforeDate :: (IsNSConditionLock nsConditionLock, IsNSDate limit) => nsConditionLock -> CLong -> limit -> IO Bool
lockWhenCondition_beforeDate nsConditionLock  condition limit =
withObjCPtr limit $ \raw_limit ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsConditionLock (mkSelector "lockWhenCondition:beforeDate:") retCULong [argCLong (fromIntegral condition), argPtr (castPtr raw_limit :: Ptr ())]

-- | @- condition@
condition :: IsNSConditionLock nsConditionLock => nsConditionLock -> IO CLong
condition nsConditionLock  =
  sendMsg nsConditionLock (mkSelector "condition") retCLong []

-- | @- name@
name :: IsNSConditionLock nsConditionLock => nsConditionLock -> IO (Id NSString)
name nsConditionLock  =
  sendMsg nsConditionLock (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsNSConditionLock nsConditionLock, IsNSString value) => nsConditionLock -> value -> IO ()
setName nsConditionLock  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsConditionLock (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCondition:@
initWithConditionSelector :: Selector
initWithConditionSelector = mkSelector "initWithCondition:"

-- | @Selector@ for @lockWhenCondition:@
lockWhenConditionSelector :: Selector
lockWhenConditionSelector = mkSelector "lockWhenCondition:"

-- | @Selector@ for @tryLock@
tryLockSelector :: Selector
tryLockSelector = mkSelector "tryLock"

-- | @Selector@ for @tryLockWhenCondition:@
tryLockWhenConditionSelector :: Selector
tryLockWhenConditionSelector = mkSelector "tryLockWhenCondition:"

-- | @Selector@ for @unlockWithCondition:@
unlockWithConditionSelector :: Selector
unlockWithConditionSelector = mkSelector "unlockWithCondition:"

-- | @Selector@ for @lockBeforeDate:@
lockBeforeDateSelector :: Selector
lockBeforeDateSelector = mkSelector "lockBeforeDate:"

-- | @Selector@ for @lockWhenCondition:beforeDate:@
lockWhenCondition_beforeDateSelector :: Selector
lockWhenCondition_beforeDateSelector = mkSelector "lockWhenCondition:beforeDate:"

-- | @Selector@ for @condition@
conditionSelector :: Selector
conditionSelector = mkSelector "condition"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

