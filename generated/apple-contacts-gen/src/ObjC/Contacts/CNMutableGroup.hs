{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Contacts.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsCNMutableGroup cnMutableGroup => cnMutableGroup -> IO (Id NSString)
name cnMutableGroup =
  sendMessage cnMutableGroup nameSelector

-- | @- setName:@
setName :: (IsCNMutableGroup cnMutableGroup, IsNSString value) => cnMutableGroup -> value -> IO ()
setName cnMutableGroup value =
  sendMessage cnMutableGroup setNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

