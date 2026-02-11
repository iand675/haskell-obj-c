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
  , waitSelector
  , waitUntilDateSelector
  , signalSelector
  , broadcastSelector
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

-- | @- wait@
wait :: IsNSCondition nsCondition => nsCondition -> IO ()
wait nsCondition  =
  sendMsg nsCondition (mkSelector "wait") retVoid []

-- | @- waitUntilDate:@
waitUntilDate :: (IsNSCondition nsCondition, IsNSDate limit) => nsCondition -> limit -> IO Bool
waitUntilDate nsCondition  limit =
withObjCPtr limit $ \raw_limit ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCondition (mkSelector "waitUntilDate:") retCULong [argPtr (castPtr raw_limit :: Ptr ())]

-- | @- signal@
signal :: IsNSCondition nsCondition => nsCondition -> IO ()
signal nsCondition  =
  sendMsg nsCondition (mkSelector "signal") retVoid []

-- | @- broadcast@
broadcast :: IsNSCondition nsCondition => nsCondition -> IO ()
broadcast nsCondition  =
  sendMsg nsCondition (mkSelector "broadcast") retVoid []

-- | @- name@
name :: IsNSCondition nsCondition => nsCondition -> IO (Id NSString)
name nsCondition  =
  sendMsg nsCondition (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsNSCondition nsCondition, IsNSString value) => nsCondition -> value -> IO ()
setName nsCondition  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsCondition (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @wait@
waitSelector :: Selector
waitSelector = mkSelector "wait"

-- | @Selector@ for @waitUntilDate:@
waitUntilDateSelector :: Selector
waitUntilDateSelector = mkSelector "waitUntilDate:"

-- | @Selector@ for @signal@
signalSelector :: Selector
signalSelector = mkSelector "signal"

-- | @Selector@ for @broadcast@
broadcastSelector :: Selector
broadcastSelector = mkSelector "broadcast"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

