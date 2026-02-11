{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An instance of JSVirtualMachine represents a single JavaScript "object space" or set of execution resources. Thread safety is supported by locking the virtual machine, with concurrent JavaScript execution supported by allocating separate instances of JSVirtualMachine.
--
-- A virtual machine may need to run deferred tasks on a run loop, such as garbage collection or resolving WebAssembly compilations. By default, a virtual machine will use the run loop of the thread it was initialized on. Currently, there is no API to change a JSVirtualMachine's run loop once it has been initialized.
--
-- Generated bindings for @JSVirtualMachine@.
module ObjC.JavaScriptCore.JSVirtualMachine
  ( JSVirtualMachine
  , IsJSVirtualMachine(..)
  , init_
  , addManagedReference_withOwner
  , removeManagedReference_withOwner
  , initSelector
  , addManagedReference_withOwnerSelector
  , removeManagedReference_withOwnerSelector


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

import ObjC.JavaScriptCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creating New Virtual Machines
--
-- Create a new JSVirtualMachine.
--
-- ObjC selector: @- init@
init_ :: IsJSVirtualMachine jsVirtualMachine => jsVirtualMachine -> IO (Id JSVirtualMachine)
init_ jsVirtualMachine  =
  sendMsg jsVirtualMachine (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Memory Management
--
-- Notify the JSVirtualMachine of an external object relationship.
--
-- Allows clients of JSVirtualMachine to make the JavaScript runtime aware of  arbitrary external Objective-C object graphs. The runtime can then use  this information to retain any JavaScript values that are referenced  from somewhere in said object graph.
--
-- For correct behavior clients must make their external object graphs  reachable from within the JavaScript runtime. If an Objective-C object is  reachable from within the JavaScript runtime, all managed references  transitively reachable from it as recorded using -addManagedReference:withOwner: will be scanned by the garbage collector.
--
-- @object@ — The object that the owner points to.
--
-- @owner@ — The object that owns the pointed to object.
--
-- ObjC selector: @- addManagedReference:withOwner:@
addManagedReference_withOwner :: IsJSVirtualMachine jsVirtualMachine => jsVirtualMachine -> RawId -> RawId -> IO ()
addManagedReference_withOwner jsVirtualMachine  object owner =
  sendMsg jsVirtualMachine (mkSelector "addManagedReference:withOwner:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (castPtr (unRawId owner) :: Ptr ())]

-- | Notify the JSVirtualMachine that a previous object relationship no longer exists.
--
-- The JavaScript runtime will continue to scan any references that were reported to it by -addManagedReference:withOwner: until those references are removed.
--
-- @object@ — The object that was formerly owned.
--
-- @owner@ — The former owner.
--
-- ObjC selector: @- removeManagedReference:withOwner:@
removeManagedReference_withOwner :: IsJSVirtualMachine jsVirtualMachine => jsVirtualMachine -> RawId -> RawId -> IO ()
removeManagedReference_withOwner jsVirtualMachine  object owner =
  sendMsg jsVirtualMachine (mkSelector "removeManagedReference:withOwner:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (castPtr (unRawId owner) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @addManagedReference:withOwner:@
addManagedReference_withOwnerSelector :: Selector
addManagedReference_withOwnerSelector = mkSelector "addManagedReference:withOwner:"

-- | @Selector@ for @removeManagedReference:withOwner:@
removeManagedReference_withOwnerSelector :: Selector
removeManagedReference_withOwnerSelector = mkSelector "removeManagedReference:withOwner:"

