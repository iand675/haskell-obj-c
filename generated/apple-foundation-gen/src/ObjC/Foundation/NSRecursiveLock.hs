{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSRecursiveLock@.
module ObjC.Foundation.NSRecursiveLock
  ( NSRecursiveLock
  , IsNSRecursiveLock(..)
  , tryLock
  , lockBeforeDate
  , name
  , setName
  , lockBeforeDateSelector
  , nameSelector
  , setNameSelector
  , tryLockSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- tryLock@
tryLock :: IsNSRecursiveLock nsRecursiveLock => nsRecursiveLock -> IO Bool
tryLock nsRecursiveLock =
  sendMessage nsRecursiveLock tryLockSelector

-- | @- lockBeforeDate:@
lockBeforeDate :: (IsNSRecursiveLock nsRecursiveLock, IsNSDate limit) => nsRecursiveLock -> limit -> IO Bool
lockBeforeDate nsRecursiveLock limit =
  sendMessage nsRecursiveLock lockBeforeDateSelector (toNSDate limit)

-- | @- name@
name :: IsNSRecursiveLock nsRecursiveLock => nsRecursiveLock -> IO (Id NSString)
name nsRecursiveLock =
  sendMessage nsRecursiveLock nameSelector

-- | @- setName:@
setName :: (IsNSRecursiveLock nsRecursiveLock, IsNSString value) => nsRecursiveLock -> value -> IO ()
setName nsRecursiveLock value =
  sendMessage nsRecursiveLock setNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tryLock@
tryLockSelector :: Selector '[] Bool
tryLockSelector = mkSelector "tryLock"

-- | @Selector@ for @lockBeforeDate:@
lockBeforeDateSelector :: Selector '[Id NSDate] Bool
lockBeforeDateSelector = mkSelector "lockBeforeDate:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

