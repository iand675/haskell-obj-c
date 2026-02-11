{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Groups together properties to drive a static linking process.
--
-- Generated bindings for @MTL4StaticLinkingDescriptor@.
module ObjC.Metal.MTL4StaticLinkingDescriptor
  ( MTL4StaticLinkingDescriptor
  , IsMTL4StaticLinkingDescriptor(..)
  , functionDescriptors
  , setFunctionDescriptors
  , privateFunctionDescriptors
  , setPrivateFunctionDescriptors
  , groups
  , setGroups
  , functionDescriptorsSelector
  , setFunctionDescriptorsSelector
  , privateFunctionDescriptorsSelector
  , setPrivateFunctionDescriptorsSelector
  , groupsSelector
  , setGroupsSelector


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

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Provides an array of functions to link at the Metal IR level.
--
-- ObjC selector: @- functionDescriptors@
functionDescriptors :: IsMTL4StaticLinkingDescriptor mtL4StaticLinkingDescriptor => mtL4StaticLinkingDescriptor -> IO (Id NSArray)
functionDescriptors mtL4StaticLinkingDescriptor  =
  sendMsg mtL4StaticLinkingDescriptor (mkSelector "functionDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides an array of functions to link at the Metal IR level.
--
-- ObjC selector: @- setFunctionDescriptors:@
setFunctionDescriptors :: (IsMTL4StaticLinkingDescriptor mtL4StaticLinkingDescriptor, IsNSArray value) => mtL4StaticLinkingDescriptor -> value -> IO ()
setFunctionDescriptors mtL4StaticLinkingDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4StaticLinkingDescriptor (mkSelector "setFunctionDescriptors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Provides an array of private functions to link at the Metal IR level.
--
-- You specify private functions to link separately from ``functionDescriptors`` because pipelines don't export private functions as ``MTLFunctionHandle`` instances. - Note: You can link private functions even when your ``MTLDevice`` doesn't support function pointers.
--
-- ObjC selector: @- privateFunctionDescriptors@
privateFunctionDescriptors :: IsMTL4StaticLinkingDescriptor mtL4StaticLinkingDescriptor => mtL4StaticLinkingDescriptor -> IO (Id NSArray)
privateFunctionDescriptors mtL4StaticLinkingDescriptor  =
  sendMsg mtL4StaticLinkingDescriptor (mkSelector "privateFunctionDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides an array of private functions to link at the Metal IR level.
--
-- You specify private functions to link separately from ``functionDescriptors`` because pipelines don't export private functions as ``MTLFunctionHandle`` instances. - Note: You can link private functions even when your ``MTLDevice`` doesn't support function pointers.
--
-- ObjC selector: @- setPrivateFunctionDescriptors:@
setPrivateFunctionDescriptors :: (IsMTL4StaticLinkingDescriptor mtL4StaticLinkingDescriptor, IsNSArray value) => mtL4StaticLinkingDescriptor -> value -> IO ()
setPrivateFunctionDescriptors mtL4StaticLinkingDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4StaticLinkingDescriptor (mkSelector "setPrivateFunctionDescriptors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Assigns groups of functions to match call-site attributes in shader code.
--
-- Function groups help the compiler reduce the number of candidate functions it needs to evaluate for shader function calls, potentially increasing runtime performance.
--
-- ObjC selector: @- groups@
groups :: IsMTL4StaticLinkingDescriptor mtL4StaticLinkingDescriptor => mtL4StaticLinkingDescriptor -> IO (Id NSDictionary)
groups mtL4StaticLinkingDescriptor  =
  sendMsg mtL4StaticLinkingDescriptor (mkSelector "groups") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Assigns groups of functions to match call-site attributes in shader code.
--
-- Function groups help the compiler reduce the number of candidate functions it needs to evaluate for shader function calls, potentially increasing runtime performance.
--
-- ObjC selector: @- setGroups:@
setGroups :: (IsMTL4StaticLinkingDescriptor mtL4StaticLinkingDescriptor, IsNSDictionary value) => mtL4StaticLinkingDescriptor -> value -> IO ()
setGroups mtL4StaticLinkingDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4StaticLinkingDescriptor (mkSelector "setGroups:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @functionDescriptors@
functionDescriptorsSelector :: Selector
functionDescriptorsSelector = mkSelector "functionDescriptors"

-- | @Selector@ for @setFunctionDescriptors:@
setFunctionDescriptorsSelector :: Selector
setFunctionDescriptorsSelector = mkSelector "setFunctionDescriptors:"

-- | @Selector@ for @privateFunctionDescriptors@
privateFunctionDescriptorsSelector :: Selector
privateFunctionDescriptorsSelector = mkSelector "privateFunctionDescriptors"

-- | @Selector@ for @setPrivateFunctionDescriptors:@
setPrivateFunctionDescriptorsSelector :: Selector
setPrivateFunctionDescriptorsSelector = mkSelector "setPrivateFunctionDescriptors:"

-- | @Selector@ for @groups@
groupsSelector :: Selector
groupsSelector = mkSelector "groups"

-- | @Selector@ for @setGroups:@
setGroupsSelector :: Selector
setGroupsSelector = mkSelector "setGroups:"

