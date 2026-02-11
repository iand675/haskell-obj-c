{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A mutable value object representing a group.
--
-- CNMutableGroup is not thread safe.
--
-- Generated bindings for @CNMutableGroup@.
module ObjC.Contacts.CNMutableGroup
  ( CNMutableGroup
  , IsCNMutableGroup(..)
  , name
  , setName
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

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsCNMutableGroup cnMutableGroup => cnMutableGroup -> IO (Id NSString)
name cnMutableGroup  =
  sendMsg cnMutableGroup (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsCNMutableGroup cnMutableGroup, IsNSString value) => cnMutableGroup -> value -> IO ()
setName cnMutableGroup  value =
withObjCPtr value $ \raw_value ->
    sendMsg cnMutableGroup (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

