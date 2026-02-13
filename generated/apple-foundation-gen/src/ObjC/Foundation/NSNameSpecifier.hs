{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSNameSpecifier@.
module ObjC.Foundation.NSNameSpecifier
  ( NSNameSpecifier
  , IsNSNameSpecifier(..)
  , initWithCoder
  , initWithContainerClassDescription_containerSpecifier_key_name
  , name
  , setName
  , initWithCoderSelector
  , initWithContainerClassDescription_containerSpecifier_key_nameSelector
  , nameSelector
  , setNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initWithCoder:@
initWithCoder :: (IsNSNameSpecifier nsNameSpecifier, IsNSCoder inCoder) => nsNameSpecifier -> inCoder -> IO (Id NSNameSpecifier)
initWithCoder nsNameSpecifier inCoder =
  sendOwnedMessage nsNameSpecifier initWithCoderSelector (toNSCoder inCoder)

-- | @- initWithContainerClassDescription:containerSpecifier:key:name:@
initWithContainerClassDescription_containerSpecifier_key_name :: (IsNSNameSpecifier nsNameSpecifier, IsNSScriptClassDescription classDesc, IsNSScriptObjectSpecifier container, IsNSString property, IsNSString name) => nsNameSpecifier -> classDesc -> container -> property -> name -> IO (Id NSNameSpecifier)
initWithContainerClassDescription_containerSpecifier_key_name nsNameSpecifier classDesc container property name =
  sendOwnedMessage nsNameSpecifier initWithContainerClassDescription_containerSpecifier_key_nameSelector (toNSScriptClassDescription classDesc) (toNSScriptObjectSpecifier container) (toNSString property) (toNSString name)

-- | @- name@
name :: IsNSNameSpecifier nsNameSpecifier => nsNameSpecifier -> IO (Id NSString)
name nsNameSpecifier =
  sendMessage nsNameSpecifier nameSelector

-- | @- setName:@
setName :: (IsNSNameSpecifier nsNameSpecifier, IsNSString value) => nsNameSpecifier -> value -> IO ()
setName nsNameSpecifier value =
  sendMessage nsNameSpecifier setNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSNameSpecifier)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithContainerClassDescription:containerSpecifier:key:name:@
initWithContainerClassDescription_containerSpecifier_key_nameSelector :: Selector '[Id NSScriptClassDescription, Id NSScriptObjectSpecifier, Id NSString, Id NSString] (Id NSNameSpecifier)
initWithContainerClassDescription_containerSpecifier_key_nameSelector = mkSelector "initWithContainerClassDescription:containerSpecifier:key:name:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

