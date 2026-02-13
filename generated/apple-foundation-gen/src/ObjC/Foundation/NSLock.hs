{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSLock@.
module ObjC.Foundation.NSLock
  ( NSLock
  , IsNSLock(..)
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
tryLock :: IsNSLock nsLock => nsLock -> IO Bool
tryLock nsLock =
  sendMessage nsLock tryLockSelector

-- | @- lockBeforeDate:@
lockBeforeDate :: (IsNSLock nsLock, IsNSDate limit) => nsLock -> limit -> IO Bool
lockBeforeDate nsLock limit =
  sendMessage nsLock lockBeforeDateSelector (toNSDate limit)

-- | @- name@
name :: IsNSLock nsLock => nsLock -> IO (Id NSString)
name nsLock =
  sendMessage nsLock nameSelector

-- | @- setName:@
setName :: (IsNSLock nsLock, IsNSString value) => nsLock -> value -> IO ()
setName nsLock value =
  sendMessage nsLock setNameSelector (toNSString value)

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

