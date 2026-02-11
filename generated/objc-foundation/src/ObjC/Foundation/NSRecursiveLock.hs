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
  , tryLockSelector
  , lockBeforeDateSelector
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

-- | @- tryLock@
tryLock :: IsNSRecursiveLock nsRecursiveLock => nsRecursiveLock -> IO Bool
tryLock nsRecursiveLock  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRecursiveLock (mkSelector "tryLock") retCULong []

-- | @- lockBeforeDate:@
lockBeforeDate :: (IsNSRecursiveLock nsRecursiveLock, IsNSDate limit) => nsRecursiveLock -> limit -> IO Bool
lockBeforeDate nsRecursiveLock  limit =
withObjCPtr limit $ \raw_limit ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRecursiveLock (mkSelector "lockBeforeDate:") retCULong [argPtr (castPtr raw_limit :: Ptr ())]

-- | @- name@
name :: IsNSRecursiveLock nsRecursiveLock => nsRecursiveLock -> IO (Id NSString)
name nsRecursiveLock  =
  sendMsg nsRecursiveLock (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsNSRecursiveLock nsRecursiveLock, IsNSString value) => nsRecursiveLock -> value -> IO ()
setName nsRecursiveLock  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsRecursiveLock (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @tryLock@
tryLockSelector :: Selector
tryLockSelector = mkSelector "tryLock"

-- | @Selector@ for @lockBeforeDate:@
lockBeforeDateSelector :: Selector
lockBeforeDateSelector = mkSelector "lockBeforeDate:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

