{-# LANGUAGE DataKinds #-}
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
  , addManagedReference_withOwnerSelector
  , initSelector
  , removeManagedReference_withOwnerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
init_ jsVirtualMachine =
  sendOwnedMessage jsVirtualMachine initSelector

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
addManagedReference_withOwner jsVirtualMachine object owner =
  sendMessage jsVirtualMachine addManagedReference_withOwnerSelector object owner

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
removeManagedReference_withOwner jsVirtualMachine object owner =
  sendMessage jsVirtualMachine removeManagedReference_withOwnerSelector object owner

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id JSVirtualMachine)
initSelector = mkSelector "init"

-- | @Selector@ for @addManagedReference:withOwner:@
addManagedReference_withOwnerSelector :: Selector '[RawId, RawId] ()
addManagedReference_withOwnerSelector = mkSelector "addManagedReference:withOwner:"

-- | @Selector@ for @removeManagedReference:withOwner:@
removeManagedReference_withOwnerSelector :: Selector '[RawId, RawId] ()
removeManagedReference_withOwnerSelector = mkSelector "removeManagedReference:withOwner:"

