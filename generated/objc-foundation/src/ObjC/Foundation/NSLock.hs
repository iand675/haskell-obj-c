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
tryLock :: IsNSLock nsLock => nsLock -> IO Bool
tryLock nsLock  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLock (mkSelector "tryLock") retCULong []

-- | @- lockBeforeDate:@
lockBeforeDate :: (IsNSLock nsLock, IsNSDate limit) => nsLock -> limit -> IO Bool
lockBeforeDate nsLock  limit =
withObjCPtr limit $ \raw_limit ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLock (mkSelector "lockBeforeDate:") retCULong [argPtr (castPtr raw_limit :: Ptr ())]

-- | @- name@
name :: IsNSLock nsLock => nsLock -> IO (Id NSString)
name nsLock  =
  sendMsg nsLock (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsNSLock nsLock, IsNSString value) => nsLock -> value -> IO ()
setName nsLock  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsLock (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

