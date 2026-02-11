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

-- | @- initWithCoder:@
initWithCoder :: (IsNSNameSpecifier nsNameSpecifier, IsNSCoder inCoder) => nsNameSpecifier -> inCoder -> IO (Id NSNameSpecifier)
initWithCoder nsNameSpecifier  inCoder =
withObjCPtr inCoder $ \raw_inCoder ->
    sendMsg nsNameSpecifier (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_inCoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContainerClassDescription:containerSpecifier:key:name:@
initWithContainerClassDescription_containerSpecifier_key_name :: (IsNSNameSpecifier nsNameSpecifier, IsNSScriptClassDescription classDesc, IsNSScriptObjectSpecifier container, IsNSString property, IsNSString name) => nsNameSpecifier -> classDesc -> container -> property -> name -> IO (Id NSNameSpecifier)
initWithContainerClassDescription_containerSpecifier_key_name nsNameSpecifier  classDesc container property name =
withObjCPtr classDesc $ \raw_classDesc ->
  withObjCPtr container $ \raw_container ->
    withObjCPtr property $ \raw_property ->
      withObjCPtr name $ \raw_name ->
          sendMsg nsNameSpecifier (mkSelector "initWithContainerClassDescription:containerSpecifier:key:name:") (retPtr retVoid) [argPtr (castPtr raw_classDesc :: Ptr ()), argPtr (castPtr raw_container :: Ptr ()), argPtr (castPtr raw_property :: Ptr ()), argPtr (castPtr raw_name :: Ptr ())] >>= ownedObject . castPtr

-- | @- name@
name :: IsNSNameSpecifier nsNameSpecifier => nsNameSpecifier -> IO (Id NSString)
name nsNameSpecifier  =
  sendMsg nsNameSpecifier (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsNSNameSpecifier nsNameSpecifier, IsNSString value) => nsNameSpecifier -> value -> IO ()
setName nsNameSpecifier  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsNameSpecifier (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithContainerClassDescription:containerSpecifier:key:name:@
initWithContainerClassDescription_containerSpecifier_key_nameSelector :: Selector
initWithContainerClassDescription_containerSpecifier_key_nameSelector = mkSelector "initWithContainerClassDescription:containerSpecifier:key:name:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

