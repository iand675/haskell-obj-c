{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.JavaScriptCore.Internal.Classes (
    module ObjC.JavaScriptCore.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- JSContext ----------

-- | A JSContext is a JavaScript execution environment. All JavaScript execution takes place within a context, and all JavaScript values are tied to a context.
-- 
-- Phantom type for @JSContext@.
data JSContext

instance IsObjCObject (Id JSContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "JSContext"

class IsNSObject a => IsJSContext a where
  toJSContext :: a -> Id JSContext

instance IsJSContext (Id JSContext) where
  toJSContext = unsafeCastId

instance IsNSObject (Id JSContext) where
  toNSObject = unsafeCastId

-- ---------- JSManagedValue ----------

-- | JSManagedValue represents a "conditionally retained" JSValue.  "Conditionally retained" means that as long as the JSManagedValue's  JSValue is reachable through the JavaScript object graph, or through the Objective-C object graph reported to the JSVirtualMachine using addManagedReference:withOwner:, the corresponding JSValue will  be retained. However, if neither graph reaches the JSManagedValue, the  corresponding JSValue will be released and set to nil.
--
-- The primary use for a JSManagedValue is to store a JSValue in an Objective-Cor Swift object that is exported to JavaScript. It is incorrect to store a JSValuein an object that is exported to JavaScript, since doing so creates a retain cycle.
-- 
-- Phantom type for @JSManagedValue@.
data JSManagedValue

instance IsObjCObject (Id JSManagedValue) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "JSManagedValue"

class IsNSObject a => IsJSManagedValue a where
  toJSManagedValue :: a -> Id JSManagedValue

instance IsJSManagedValue (Id JSManagedValue) where
  toJSManagedValue = unsafeCastId

instance IsNSObject (Id JSManagedValue) where
  toNSObject = unsafeCastId

-- ---------- JSValue ----------

-- | A JSValue is a reference to a JavaScript value. Every JSValue originates from a JSContext and holds a strong reference to it. When a JSValue instance method creates a new JSValue, the new value originates from the same JSContext.
--
-- All JSValues values also originate from a JSVirtualMachine (available indirectly via the context property). It is an error to pass a JSValue to a method or property of a JSValue or JSContext originating from a different JSVirtualMachine. Doing so will raise an Objective-C exception.
-- 
-- Phantom type for @JSValue@.
data JSValue

instance IsObjCObject (Id JSValue) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "JSValue"

class IsNSObject a => IsJSValue a where
  toJSValue :: a -> Id JSValue

instance IsJSValue (Id JSValue) where
  toJSValue = unsafeCastId

instance IsNSObject (Id JSValue) where
  toNSObject = unsafeCastId

-- ---------- JSVirtualMachine ----------

-- | An instance of JSVirtualMachine represents a single JavaScript "object space" or set of execution resources. Thread safety is supported by locking the virtual machine, with concurrent JavaScript execution supported by allocating separate instances of JSVirtualMachine.
--
-- A virtual machine may need to run deferred tasks on a run loop, such as garbage collection or resolving WebAssembly compilations. By default, a virtual machine will use the run loop of the thread it was initialized on. Currently, there is no API to change a JSVirtualMachine's run loop once it has been initialized.
-- 
-- Phantom type for @JSVirtualMachine@.
data JSVirtualMachine

instance IsObjCObject (Id JSVirtualMachine) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "JSVirtualMachine"

class IsNSObject a => IsJSVirtualMachine a where
  toJSVirtualMachine :: a -> Id JSVirtualMachine

instance IsJSVirtualMachine (Id JSVirtualMachine) where
  toJSVirtualMachine = unsafeCastId

instance IsNSObject (Id JSVirtualMachine) where
  toNSObject = unsafeCastId
