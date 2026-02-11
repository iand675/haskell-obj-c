{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A JSValue is a reference to a JavaScript value. Every JSValue originates from a JSContext and holds a strong reference to it. When a JSValue instance method creates a new JSValue, the new value originates from the same JSContext.
--
-- All JSValues values also originate from a JSVirtualMachine (available indirectly via the context property). It is an error to pass a JSValue to a method or property of a JSValue or JSContext originating from a different JSVirtualMachine. Doing so will raise an Objective-C exception.
--
-- Generated bindings for @JSValue@.
module ObjC.JavaScriptCore.JSValue
  ( JSValue
  , IsJSValue(..)
  , valueWithObject_inContext
  , valueWithBool_inContext
  , valueWithDouble_inContext
  , valueWithInt32_inContext
  , valueWithUInt32_inContext
  , valueWithNewObjectInContext
  , valueWithNewArrayInContext
  , valueWithNewRegularExpressionFromPattern_flags_inContext
  , valueWithNewErrorFromMessage_inContext
  , valueWithNewPromiseInContext_fromExecutor
  , valueWithNewPromiseResolvedWithResult_inContext
  , valueWithNewPromiseRejectedWithReason_inContext
  , valueWithNewSymbolFromDescription_inContext
  , valueWithNewBigIntFromString_inContext
  , valueWithNewBigIntFromInt64_inContext
  , valueWithNewBigIntFromUInt64_inContext
  , valueWithNewBigIntFromDouble_inContext
  , valueWithNullInContext
  , valueWithUndefinedInContext
  , toObject
  , toObjectOfClass
  , toBool
  , toDouble
  , toInt32
  , toUInt32
  , toInt64
  , toUInt64
  , toNumber
  , toString
  , toDate
  , toArray
  , toDictionary
  , isInstanceOf
  , isEqualToObject
  , isEqualWithTypeCoercionToObject
  , compareJSValue
  , compareInt64
  , compareUInt64
  , compareDouble
  , callWithArguments
  , constructWithArguments
  , invokeMethod_withArguments
  , valueWithJSValueRef_inContext
  , objectForKeyedSubscript
  , objectAtIndexedSubscript
  , setObject_forKeyedSubscript
  , setObject_atIndexedSubscript
  , valueForProperty
  , setValue_forProperty
  , deleteProperty
  , hasProperty
  , defineProperty_descriptor
  , valueAtIndex
  , setValue_atIndex
  , valueWithRange_inContext
  , toRange
  , context
  , isUndefined
  , isNull
  , isBoolean
  , isNumber
  , isString
  , isObject
  , isArray
  , isDate
  , isSymbol
  , isBigInt
  , jsValueRef
  , valueWithObject_inContextSelector
  , valueWithBool_inContextSelector
  , valueWithDouble_inContextSelector
  , valueWithInt32_inContextSelector
  , valueWithUInt32_inContextSelector
  , valueWithNewObjectInContextSelector
  , valueWithNewArrayInContextSelector
  , valueWithNewRegularExpressionFromPattern_flags_inContextSelector
  , valueWithNewErrorFromMessage_inContextSelector
  , valueWithNewPromiseInContext_fromExecutorSelector
  , valueWithNewPromiseResolvedWithResult_inContextSelector
  , valueWithNewPromiseRejectedWithReason_inContextSelector
  , valueWithNewSymbolFromDescription_inContextSelector
  , valueWithNewBigIntFromString_inContextSelector
  , valueWithNewBigIntFromInt64_inContextSelector
  , valueWithNewBigIntFromUInt64_inContextSelector
  , valueWithNewBigIntFromDouble_inContextSelector
  , valueWithNullInContextSelector
  , valueWithUndefinedInContextSelector
  , toObjectSelector
  , toObjectOfClassSelector
  , toBoolSelector
  , toDoubleSelector
  , toInt32Selector
  , toUInt32Selector
  , toInt64Selector
  , toUInt64Selector
  , toNumberSelector
  , toStringSelector
  , toDateSelector
  , toArraySelector
  , toDictionarySelector
  , isInstanceOfSelector
  , isEqualToObjectSelector
  , isEqualWithTypeCoercionToObjectSelector
  , compareJSValueSelector
  , compareInt64Selector
  , compareUInt64Selector
  , compareDoubleSelector
  , callWithArgumentsSelector
  , constructWithArgumentsSelector
  , invokeMethod_withArgumentsSelector
  , valueWithJSValueRef_inContextSelector
  , objectForKeyedSubscriptSelector
  , objectAtIndexedSubscriptSelector
  , setObject_forKeyedSubscriptSelector
  , setObject_atIndexedSubscriptSelector
  , valueForPropertySelector
  , setValue_forPropertySelector
  , deletePropertySelector
  , hasPropertySelector
  , defineProperty_descriptorSelector
  , valueAtIndexSelector
  , setValue_atIndexSelector
  , valueWithRange_inContextSelector
  , toRangeSelector
  , contextSelector
  , isUndefinedSelector
  , isNullSelector
  , isBooleanSelector
  , isNumberSelector
  , isStringSelector
  , isObjectSelector
  , isArraySelector
  , isDateSelector
  , isSymbolSelector
  , isBigIntSelector
  , jsValueRefSelector

  -- * Enum types
  , JSRelationCondition(JSRelationCondition)
  , pattern KJSRelationConditionUndefined
  , pattern KJSRelationConditionEqual
  , pattern KJSRelationConditionGreaterThan
  , pattern KJSRelationConditionLessThan

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.JavaScriptCore.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.JavaScriptCore.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creating JavaScript Values
--
-- Create a JSValue by converting an Objective-C object.
--
-- The resulting JSValue retains the provided Objective-C object.
--
-- @value@ — The Objective-C object to be converted.
--
-- Returns: The new JSValue.
--
-- ObjC selector: @+ valueWithObject:inContext:@
valueWithObject_inContext :: IsJSContext context => RawId -> context -> IO (Id JSValue)
valueWithObject_inContext value context =
  do
    cls' <- getRequiredClass "JSValue"
    withObjCPtr context $ \raw_context ->
      sendClassMsg cls' (mkSelector "valueWithObject:inContext:") (retPtr retVoid) [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | Create a JavaScript value from a BOOL primitive.
--
-- @context@ — The JSContext in which the resulting JSValue will be created.
--
-- Returns: The new JSValue representing the equivalent boolean value.
--
-- ObjC selector: @+ valueWithBool:inContext:@
valueWithBool_inContext :: IsJSContext context => Bool -> context -> IO (Id JSValue)
valueWithBool_inContext value context =
  do
    cls' <- getRequiredClass "JSValue"
    withObjCPtr context $ \raw_context ->
      sendClassMsg cls' (mkSelector "valueWithBool:inContext:") (retPtr retVoid) [argCULong (if value then 1 else 0), argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | Create a JavaScript value from a double primitive.
--
-- @context@ — The JSContext in which the resulting JSValue will be created.
--
-- Returns: The new JSValue representing the equivalent boolean value.
--
-- ObjC selector: @+ valueWithDouble:inContext:@
valueWithDouble_inContext :: IsJSContext context => CDouble -> context -> IO (Id JSValue)
valueWithDouble_inContext value context =
  do
    cls' <- getRequiredClass "JSValue"
    withObjCPtr context $ \raw_context ->
      sendClassMsg cls' (mkSelector "valueWithDouble:inContext:") (retPtr retVoid) [argCDouble (fromIntegral value), argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | Create a JavaScript value from an int32_t primitive.
--
-- @context@ — The JSContext in which the resulting JSValue will be created.
--
-- Returns: The new JSValue representing the equivalent boolean value.
--
-- ObjC selector: @+ valueWithInt32:inContext:@
valueWithInt32_inContext :: IsJSContext context => CInt -> context -> IO (Id JSValue)
valueWithInt32_inContext value context =
  do
    cls' <- getRequiredClass "JSValue"
    withObjCPtr context $ \raw_context ->
      sendClassMsg cls' (mkSelector "valueWithInt32:inContext:") (retPtr retVoid) [argCInt (fromIntegral value), argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | Create a JavaScript value from a uint32_t primitive.
--
-- @context@ — The JSContext in which the resulting JSValue will be created.
--
-- Returns: The new JSValue representing the equivalent boolean value.
--
-- ObjC selector: @+ valueWithUInt32:inContext:@
valueWithUInt32_inContext :: IsJSContext context => CUInt -> context -> IO (Id JSValue)
valueWithUInt32_inContext value context =
  do
    cls' <- getRequiredClass "JSValue"
    withObjCPtr context $ \raw_context ->
      sendClassMsg cls' (mkSelector "valueWithUInt32:inContext:") (retPtr retVoid) [argCUInt (fromIntegral value), argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | Create a new, empty JavaScript object.
--
-- @context@ — The JSContext in which the resulting object will be created.
--
-- Returns: The new JavaScript object.
--
-- ObjC selector: @+ valueWithNewObjectInContext:@
valueWithNewObjectInContext :: IsJSContext context => context -> IO (Id JSValue)
valueWithNewObjectInContext context =
  do
    cls' <- getRequiredClass "JSValue"
    withObjCPtr context $ \raw_context ->
      sendClassMsg cls' (mkSelector "valueWithNewObjectInContext:") (retPtr retVoid) [argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | Create a new, empty JavaScript array.
--
-- @context@ — The JSContext in which the resulting array will be created.
--
-- Returns: The new JavaScript array.
--
-- ObjC selector: @+ valueWithNewArrayInContext:@
valueWithNewArrayInContext :: IsJSContext context => context -> IO (Id JSValue)
valueWithNewArrayInContext context =
  do
    cls' <- getRequiredClass "JSValue"
    withObjCPtr context $ \raw_context ->
      sendClassMsg cls' (mkSelector "valueWithNewArrayInContext:") (retPtr retVoid) [argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | Create a new JavaScript regular expression object.
--
-- @pattern@ — The regular expression pattern.
--
-- @flags@ — The regular expression flags.
--
-- @context@ — The JSContext in which the resulting regular expression object will be created.
--
-- Returns: The new JavaScript regular expression object.
--
-- ObjC selector: @+ valueWithNewRegularExpressionFromPattern:flags:inContext:@
valueWithNewRegularExpressionFromPattern_flags_inContext :: (IsNSString pattern_, IsNSString flags, IsJSContext context) => pattern_ -> flags -> context -> IO (Id JSValue)
valueWithNewRegularExpressionFromPattern_flags_inContext pattern_ flags context =
  do
    cls' <- getRequiredClass "JSValue"
    withObjCPtr pattern_ $ \raw_pattern_ ->
      withObjCPtr flags $ \raw_flags ->
        withObjCPtr context $ \raw_context ->
          sendClassMsg cls' (mkSelector "valueWithNewRegularExpressionFromPattern:flags:inContext:") (retPtr retVoid) [argPtr (castPtr raw_pattern_ :: Ptr ()), argPtr (castPtr raw_flags :: Ptr ()), argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | Create a new JavaScript error object.
--
-- @message@ — The error message.
--
-- @context@ — The JSContext in which the resulting error object will be created.
--
-- Returns: The new JavaScript error object.
--
-- ObjC selector: @+ valueWithNewErrorFromMessage:inContext:@
valueWithNewErrorFromMessage_inContext :: (IsNSString message, IsJSContext context) => message -> context -> IO (Id JSValue)
valueWithNewErrorFromMessage_inContext message context =
  do
    cls' <- getRequiredClass "JSValue"
    withObjCPtr message $ \raw_message ->
      withObjCPtr context $ \raw_context ->
        sendClassMsg cls' (mkSelector "valueWithNewErrorFromMessage:inContext:") (retPtr retVoid) [argPtr (castPtr raw_message :: Ptr ()), argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | Create a new promise object using the provided executor callback.
--
-- @callback@ — A callback block invoked while the promise object is being initialized. The resolve and reject parameters are functions that can be called to notify any pending reactions about the state of the new promise object.
--
-- @context@ — The JSContext to which the resulting JSValue belongs.
--
-- Returns: The JSValue representing a new promise JavaScript object.
--
-- This method is equivalent to calling the Promise constructor in JavaScript. the resolve and reject callbacks each normally take a single value, which they forward to all relevent pending reactions. While inside the executor callback context will act as if it were in any other callback, except calleeFunction will be nil. This also means means the new promise object may be accessed via [context thisValue].
--
-- ObjC selector: @+ valueWithNewPromiseInContext:fromExecutor:@
valueWithNewPromiseInContext_fromExecutor :: IsJSContext context => context -> Ptr () -> IO (Id JSValue)
valueWithNewPromiseInContext_fromExecutor context callback =
  do
    cls' <- getRequiredClass "JSValue"
    withObjCPtr context $ \raw_context ->
      sendClassMsg cls' (mkSelector "valueWithNewPromiseInContext:fromExecutor:") (retPtr retVoid) [argPtr (castPtr raw_context :: Ptr ()), argPtr (castPtr callback :: Ptr ())] >>= retainedObject . castPtr

-- | Create a new resolved promise object with the provided value.
--
-- @result@ — The result value to be passed to any reactions.
--
-- @context@ — The JSContext to which the resulting JSValue belongs.
--
-- Returns: The JSValue representing a new promise JavaScript object.
--
-- This method is equivalent to calling [JSValue valueWithNewPromiseFromExecutor:^(JSValue *resolve, JSValue *reject) { [resolve callWithArguments:\@[result]]; } inContext:context]
--
-- ObjC selector: @+ valueWithNewPromiseResolvedWithResult:inContext:@
valueWithNewPromiseResolvedWithResult_inContext :: IsJSContext context => RawId -> context -> IO (Id JSValue)
valueWithNewPromiseResolvedWithResult_inContext result context =
  do
    cls' <- getRequiredClass "JSValue"
    withObjCPtr context $ \raw_context ->
      sendClassMsg cls' (mkSelector "valueWithNewPromiseResolvedWithResult:inContext:") (retPtr retVoid) [argPtr (castPtr (unRawId result) :: Ptr ()), argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | Create a new rejected promise object with the provided value.
--
-- @reason@ — The result value to be passed to any reactions.
--
-- @context@ — The JSContext to which the resulting JSValue belongs.
--
-- Returns: The JSValue representing a new promise JavaScript object.
--
-- This method is equivalent to calling [JSValue valueWithNewPromiseFromExecutor:^(JSValue *resolve, JSValue *reject) { [reject callWithArguments:\@[reason]]; } inContext:context]
--
-- ObjC selector: @+ valueWithNewPromiseRejectedWithReason:inContext:@
valueWithNewPromiseRejectedWithReason_inContext :: IsJSContext context => RawId -> context -> IO (Id JSValue)
valueWithNewPromiseRejectedWithReason_inContext reason context =
  do
    cls' <- getRequiredClass "JSValue"
    withObjCPtr context $ \raw_context ->
      sendClassMsg cls' (mkSelector "valueWithNewPromiseRejectedWithReason:inContext:") (retPtr retVoid) [argPtr (castPtr (unRawId reason) :: Ptr ()), argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | Create a new, unique, symbol object.
--
-- @description@ — The description of the symbol object being created.
--
-- @context@ — The JSContext to which the resulting JSValue belongs.
--
-- Returns: The JSValue representing a unique JavaScript value with type symbol.
--
-- ObjC selector: @+ valueWithNewSymbolFromDescription:inContext:@
valueWithNewSymbolFromDescription_inContext :: (IsNSString description, IsJSContext context) => description -> context -> IO (Id JSValue)
valueWithNewSymbolFromDescription_inContext description context =
  do
    cls' <- getRequiredClass "JSValue"
    withObjCPtr description $ \raw_description ->
      withObjCPtr context $ \raw_context ->
        sendClassMsg cls' (mkSelector "valueWithNewSymbolFromDescription:inContext:") (retPtr retVoid) [argPtr (castPtr raw_description :: Ptr ()), argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | Create a new BigInt value from a numeric string.
--
-- @string@ — The string representation of the BigInt JavaScript value being created.
--
-- @context@ — The JSContext to which the resulting JSValue belongs.
--
-- Returns: The JSValue representing a JavaScript value with type BigInt.
--
-- This is equivalent to calling the BigInt constructor from JavaScript with a string argument.
--
-- ObjC selector: @+ valueWithNewBigIntFromString:inContext:@
valueWithNewBigIntFromString_inContext :: (IsNSString string, IsJSContext context) => string -> context -> IO (Id JSValue)
valueWithNewBigIntFromString_inContext string context =
  do
    cls' <- getRequiredClass "JSValue"
    withObjCPtr string $ \raw_string ->
      withObjCPtr context $ \raw_context ->
        sendClassMsg cls' (mkSelector "valueWithNewBigIntFromString:inContext:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | Create a new BigInt value from a int64_t.
--
-- @int64@ — The signed 64-bit integer of the BigInt JavaScript value being created.
--
-- @context@ — The JSContext to which the resulting JSValue belongs.
--
-- Returns: The JSValue representing a JavaScript value with type BigInt.
--
-- ObjC selector: @+ valueWithNewBigIntFromInt64:inContext:@
valueWithNewBigIntFromInt64_inContext :: IsJSContext context => CLong -> context -> IO (Id JSValue)
valueWithNewBigIntFromInt64_inContext int64 context =
  do
    cls' <- getRequiredClass "JSValue"
    withObjCPtr context $ \raw_context ->
      sendClassMsg cls' (mkSelector "valueWithNewBigIntFromInt64:inContext:") (retPtr retVoid) [argCLong (fromIntegral int64), argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | Create a new BigInt value from a uint64_t.
--
-- @uint64@ — The unsigned 64-bit integer of the BigInt JavaScript value being created.
--
-- @context@ — The JSContext to which the resulting JSValue belongs.
--
-- Returns: The JSValue representing a JavaScript value with type BigInt.
--
-- ObjC selector: @+ valueWithNewBigIntFromUInt64:inContext:@
valueWithNewBigIntFromUInt64_inContext :: IsJSContext context => CULong -> context -> IO (Id JSValue)
valueWithNewBigIntFromUInt64_inContext uint64 context =
  do
    cls' <- getRequiredClass "JSValue"
    withObjCPtr context $ \raw_context ->
      sendClassMsg cls' (mkSelector "valueWithNewBigIntFromUInt64:inContext:") (retPtr retVoid) [argCULong (fromIntegral uint64), argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | Create a new BigInt value from a double.
--
-- @value@ — The value of the BigInt JavaScript value being created.
--
-- @context@ — The JSContext to which the resulting JSValue belongs.
--
-- Returns: The JSValue representing a JavaScript value with type BigInt.
--
-- If the value is not an integer, an exception is thrown.
--
-- ObjC selector: @+ valueWithNewBigIntFromDouble:inContext:@
valueWithNewBigIntFromDouble_inContext :: IsJSContext context => CDouble -> context -> IO (Id JSValue)
valueWithNewBigIntFromDouble_inContext value context =
  do
    cls' <- getRequiredClass "JSValue"
    withObjCPtr context $ \raw_context ->
      sendClassMsg cls' (mkSelector "valueWithNewBigIntFromDouble:inContext:") (retPtr retVoid) [argCDouble (fromIntegral value), argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | Create the JavaScript value null.
--
-- @context@ — The JSContext to which the resulting JSValue belongs.
--
-- Returns: The JSValue representing the JavaScript value null.
--
-- ObjC selector: @+ valueWithNullInContext:@
valueWithNullInContext :: IsJSContext context => context -> IO (Id JSValue)
valueWithNullInContext context =
  do
    cls' <- getRequiredClass "JSValue"
    withObjCPtr context $ \raw_context ->
      sendClassMsg cls' (mkSelector "valueWithNullInContext:") (retPtr retVoid) [argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | Create the JavaScript value undefined.
--
-- @context@ — The JSContext to which the resulting JSValue belongs.
--
-- Returns: The JSValue representing the JavaScript value undefined.
--
-- ObjC selector: @+ valueWithUndefinedInContext:@
valueWithUndefinedInContext :: IsJSContext context => context -> IO (Id JSValue)
valueWithUndefinedInContext context =
  do
    cls' <- getRequiredClass "JSValue"
    withObjCPtr context $ \raw_context ->
      sendClassMsg cls' (mkSelector "valueWithUndefinedInContext:") (retPtr retVoid) [argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | Converting to Objective-C Types
--
-- When converting between JavaScript values and Objective-C objects a copy is performed. Values of types listed below are copied to the corresponding types on conversion in each direction. For NSDictionaries, entries in the dictionary that are keyed by strings are copied onto a JavaScript object. For dictionaries and arrays, conversion is recursive, with the same object conversion being applied to all entries in the collection.
--
-- Objective-C type  |   JavaScript type
-- --------------------+---------------------
-- nil         |     undefined
-- NSNull       |        null
-- NSString      |       string
-- NSNumber      |   number, boolean
-- NSDictionary    |   Object object
-- NSArray       |    Array object
-- NSDate       |     Date object
-- NSBlock (1)   |   Function object (1)
-- id (2)     |   Wrapper object (2)
-- Class (3)    | Constructor object (3)
--
-- (1) Instances of NSBlock with supported arguments types will be presented to JavaScript as a callable Function object. For more information on supported argument types see JSExport.h. If a JavaScript Function originating from an Objective-C block is converted back to an Objective-C object the block will be returned. All other JavaScript functions will be converted in the same manner as a JavaScript object of type Object.
--
-- (2) For Objective-C instances that do not derive from the set of types listed above, a wrapper object to provide a retaining handle to the Objective-C instance from JavaScript. For more information on these wrapper objects, see JSExport.h. When a JavaScript wrapper object is converted back to Objective-C the Objective-C instance being retained by the wrapper is returned.
--
-- (3) For Objective-C Class objects a constructor object containing exported class methods will be returned. See JSExport.h for more information on constructor objects.
--
-- For all methods taking arguments of type id, arguments will be converted into a JavaScript value according to the above conversion.
--
-- Convert this JSValue to an Objective-C object.
--
-- The JSValue is converted to an Objective-C object according  to the conversion rules specified above.
--
-- Returns: The Objective-C representation of this JSValue.
--
-- ObjC selector: @- toObject@
toObject :: IsJSValue jsValue => jsValue -> IO RawId
toObject jsValue  =
  fmap (RawId . castPtr) $ sendMsg jsValue (mkSelector "toObject") (retPtr retVoid) []

-- | Convert a JSValue to an Objective-C object of a specific class.
--
-- The JSValue is converted to an Objective-C object of the specified Class.  If the result is not of the specified Class then nil will be returned.
--
-- Returns: An Objective-C object of the specified Class or nil.
--
-- ObjC selector: @- toObjectOfClass:@
toObjectOfClass :: IsJSValue jsValue => jsValue -> Class -> IO RawId
toObjectOfClass jsValue  expectedClass =
  fmap (RawId . castPtr) $ sendMsg jsValue (mkSelector "toObjectOfClass:") (retPtr retVoid) [argPtr (unClass expectedClass)]

-- | Convert a JSValue to a boolean.
--
-- The JSValue is converted to a boolean according to the rules specified  by the JavaScript language.
--
-- Returns: The boolean result of the conversion.
--
-- ObjC selector: @- toBool@
toBool :: IsJSValue jsValue => jsValue -> IO Bool
toBool jsValue  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg jsValue (mkSelector "toBool") retCULong []

-- | Convert a JSValue to a double.
--
-- Returns: The double result of the conversion.
--
-- Convert the JSValue to a number according to the rules specified by the JavaScript language. Unless the JSValue is a BigInt then this is equivalent to Number(value) in JavaScript.
--
-- ObjC selector: @- toDouble@
toDouble :: IsJSValue jsValue => jsValue -> IO CDouble
toDouble jsValue  =
  sendMsg jsValue (mkSelector "toDouble") retCDouble []

-- | Convert a JSValue to an int32_t.
--
-- The JSValue is converted to an integer according to the rules specified by the JavaScript language. If the JSValue is a BigInt, then the value is truncated to an int32_t.
--
-- Returns: The int32_t result of the conversion.
--
-- ObjC selector: @- toInt32@
toInt32 :: IsJSValue jsValue => jsValue -> IO CInt
toInt32 jsValue  =
  sendMsg jsValue (mkSelector "toInt32") retCInt []

-- | Convert a JSValue to a uint32_t.
--
-- The JSValue is converted to an integer according to the rules specified by the JavaScript language. If the JSValue is a BigInt, then the value is truncated to a uint32_t.
--
-- Returns: The uint32_t result of the conversion.
--
-- ObjC selector: @- toUInt32@
toUInt32 :: IsJSValue jsValue => jsValue -> IO CUInt
toUInt32 jsValue  =
  sendMsg jsValue (mkSelector "toUInt32") retCUInt []

-- | Convert a JSValue to a int64_t.
--
-- The JSValue is converted to an integer according to the rules specified by the JavaScript language. If the value is a BigInt, then the value is truncated to an int64_t.
--
-- ObjC selector: @- toInt64@
toInt64 :: IsJSValue jsValue => jsValue -> IO CLong
toInt64 jsValue  =
  sendMsg jsValue (mkSelector "toInt64") retCLong []

-- | Convert a JSValue to a uint64_t.
--
-- The JSValue is converted to an integer according to the rules specified by the JavaScript language. If the value is a BigInt, then the value is truncated to a uint64_t.
--
-- ObjC selector: @- toUInt64@
toUInt64 :: IsJSValue jsValue => jsValue -> IO CULong
toUInt64 jsValue  =
  sendMsg jsValue (mkSelector "toUInt64") retCULong []

-- | Convert a JSValue to a NSNumber.
--
-- If the JSValue represents a boolean, a NSNumber value of YES or NO will be returned. For all other types, the result is equivalent to Number(value) in JavaScript.
--
-- Returns: The NSNumber result of the conversion.
--
-- ObjC selector: @- toNumber@
toNumber :: IsJSValue jsValue => jsValue -> IO (Id NSNumber)
toNumber jsValue  =
  sendMsg jsValue (mkSelector "toNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convert a JSValue to a NSString.
--
-- The JSValue is converted to a string according to the rules specified  by the JavaScript language.
--
-- Returns: The NSString containing the result of the conversion.
--
-- ObjC selector: @- toString@
toString :: IsJSValue jsValue => jsValue -> IO (Id NSString)
toString jsValue  =
  sendMsg jsValue (mkSelector "toString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convert a JSValue to a NSDate.
--
-- The value is converted to a number representing a time interval  since 1970 which is then used to create a new NSDate instance.
--
-- Returns: The NSDate created using the converted time interval.
--
-- ObjC selector: @- toDate@
toDate :: IsJSValue jsValue => jsValue -> IO (Id NSDate)
toDate jsValue  =
  sendMsg jsValue (mkSelector "toDate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convert a JSValue to a NSArray.
--
-- If the value is null or undefined then nil is returned. If the value is not an object then a JavaScript TypeError will be thrown. The property length is read from the object, converted to an unsigned integer, and an NSArray of this size is allocated. Properties corresponding to indices within the array bounds will be copied to the array, with JSValues converted to equivalent Objective-C objects as specified.
--
-- Returns: The NSArray containing the recursively converted contents of the  converted JavaScript array.
--
-- ObjC selector: @- toArray@
toArray :: IsJSValue jsValue => jsValue -> IO (Id NSArray)
toArray jsValue  =
  sendMsg jsValue (mkSelector "toArray") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Convert a JSValue to a NSDictionary.
--
-- If the value is null or undefined then nil is returned. If the value is not an object then a JavaScript TypeError will be thrown. All enumerable properties of the object are copied to the dictionary, with JSValues converted to equivalent Objective-C objects as specified.
--
-- Returns: The NSDictionary containing the recursively converted contents of the converted JavaScript object.
--
-- ObjC selector: @- toDictionary@
toDictionary :: IsJSValue jsValue => jsValue -> IO (Id NSDictionary)
toDictionary jsValue  =
  sendMsg jsValue (mkSelector "toDictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Check if a JSValue is an instance of another object.
--
-- This method has the same function as the JavaScript operator instanceof. If an object other than a JSValue is passed, it will first be converted according to the aforementioned rules.
--
-- ObjC selector: @- isInstanceOf:@
isInstanceOf :: IsJSValue jsValue => jsValue -> RawId -> IO Bool
isInstanceOf jsValue  value =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg jsValue (mkSelector "isInstanceOf:") retCULong [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Compare two JSValues using JavaScript's === operator.
--
-- ObjC selector: @- isEqualToObject:@
isEqualToObject :: IsJSValue jsValue => jsValue -> RawId -> IO Bool
isEqualToObject jsValue  value =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg jsValue (mkSelector "isEqualToObject:") retCULong [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Compare two JSValues using JavaScript's == operator.
--
-- ObjC selector: @- isEqualWithTypeCoercionToObject:@
isEqualWithTypeCoercionToObject :: IsJSValue jsValue => jsValue -> RawId -> IO Bool
isEqualWithTypeCoercionToObject jsValue  value =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg jsValue (mkSelector "isEqualWithTypeCoercionToObject:") retCULong [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Compare two JSValues. The JSValue to compare with.
--
-- Returns: A value of JSRelationCondition, a kJSRelationConditionUndefined is returned if an exception is thrown.
--
-- The result is computed by comparing the results of JavaScript's ==, <, and > operators. If either self or other is (or would coerce to) NaN in JavaScript, then the result is kJSRelationConditionUndefined.
--
-- ObjC selector: @- compareJSValue:@
compareJSValue :: (IsJSValue jsValue, IsJSValue other) => jsValue -> other -> IO JSRelationCondition
compareJSValue jsValue  other =
withObjCPtr other $ \raw_other ->
    fmap (coerce :: CUInt -> JSRelationCondition) $ sendMsg jsValue (mkSelector "compareJSValue:") retCUInt [argPtr (castPtr raw_other :: Ptr ())]

-- | Compare a JSValue with a int64_t. The int64_t to compare with.
--
-- Returns: A value of JSRelationCondition, a kJSRelationConditionUndefined is returned if an exception is thrown.
--
-- The JSValue is converted to an integer according to the rules specified by the JavaScript language then compared with other.
--
-- ObjC selector: @- compareInt64:@
compareInt64 :: IsJSValue jsValue => jsValue -> CLong -> IO JSRelationCondition
compareInt64 jsValue  other =
  fmap (coerce :: CUInt -> JSRelationCondition) $ sendMsg jsValue (mkSelector "compareInt64:") retCUInt [argCLong (fromIntegral other)]

-- | Compare a JSValue with a uint64_t. The uint64_t to compare with.
--
-- Returns: A value of JSRelationCondition, a kJSRelationConditionUndefined is returned if an exception is thrown.
--
-- The JSValue is converted to an integer according to the rules specified by the JavaScript language then compared with other.
--
-- ObjC selector: @- compareUInt64:@
compareUInt64 :: IsJSValue jsValue => jsValue -> CULong -> IO JSRelationCondition
compareUInt64 jsValue  other =
  fmap (coerce :: CUInt -> JSRelationCondition) $ sendMsg jsValue (mkSelector "compareUInt64:") retCUInt [argCULong (fromIntegral other)]

-- | Compare a JSValue with a double. The double to compare with.
--
-- Returns: A value of JSRelationCondition, a kJSRelationConditionUndefined is returned if an exception is thrown.
--
-- The JSValue is converted to a double according to the rules specified by the JavaScript language then compared with other.
--
-- ObjC selector: @- compareDouble:@
compareDouble :: IsJSValue jsValue => jsValue -> CDouble -> IO JSRelationCondition
compareDouble jsValue  other =
  fmap (coerce :: CUInt -> JSRelationCondition) $ sendMsg jsValue (mkSelector "compareDouble:") retCUInt [argCDouble (fromIntegral other)]

-- | Calling Functions and Constructors
--
-- Invoke a JSValue as a function.
--
-- In JavaScript, if a function doesn't explicitly return a value then it implicitly returns the JavaScript value undefined.
--
-- @arguments@ — The arguments to pass to the function.
--
-- Returns: The return value of the function call.
--
-- ObjC selector: @- callWithArguments:@
callWithArguments :: (IsJSValue jsValue, IsNSArray arguments) => jsValue -> arguments -> IO (Id JSValue)
callWithArguments jsValue  arguments =
withObjCPtr arguments $ \raw_arguments ->
    sendMsg jsValue (mkSelector "callWithArguments:") (retPtr retVoid) [argPtr (castPtr raw_arguments :: Ptr ())] >>= retainedObject . castPtr

-- | Invoke a JSValue as a constructor.
--
-- This is equivalent to using the new syntax in JavaScript.
--
-- @arguments@ — The arguments to pass to the constructor.
--
-- Returns: The return value of the constructor call.
--
-- ObjC selector: @- constructWithArguments:@
constructWithArguments :: (IsJSValue jsValue, IsNSArray arguments) => jsValue -> arguments -> IO (Id JSValue)
constructWithArguments jsValue  arguments =
withObjCPtr arguments $ \raw_arguments ->
    sendMsg jsValue (mkSelector "constructWithArguments:") (retPtr retVoid) [argPtr (castPtr raw_arguments :: Ptr ())] >>= retainedObject . castPtr

-- | Invoke a method on a JSValue.
--
-- Accesses the property named method from this value and  calls the resulting value as a function, passing this JSValue as the this value along with the specified arguments.
--
-- @method@ — The name of the method to be invoked.
--
-- @arguments@ — The arguments to pass to the method.
--
-- Returns: The return value of the method call.
--
-- ObjC selector: @- invokeMethod:withArguments:@
invokeMethod_withArguments :: (IsJSValue jsValue, IsNSString method, IsNSArray arguments) => jsValue -> method -> arguments -> IO (Id JSValue)
invokeMethod_withArguments jsValue  method arguments =
withObjCPtr method $ \raw_method ->
  withObjCPtr arguments $ \raw_arguments ->
      sendMsg jsValue (mkSelector "invokeMethod:withArguments:") (retPtr retVoid) [argPtr (castPtr raw_method :: Ptr ()), argPtr (castPtr raw_arguments :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a JSValue, wrapping its C API counterpart.
--
-- Returns: The Objective-C API equivalent of the specified JSValueRef.
--
-- ObjC selector: @+ valueWithJSValueRef:inContext:@
valueWithJSValueRef_inContext :: IsJSContext context => RawId -> context -> IO (Id JSValue)
valueWithJSValueRef_inContext value context =
  do
    cls' <- getRequiredClass "JSValue"
    withObjCPtr context $ \raw_context ->
      sendClassMsg cls' (mkSelector "valueWithJSValueRef:inContext:") (retPtr retVoid) [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | @- objectForKeyedSubscript:@
objectForKeyedSubscript :: IsJSValue jsValue => jsValue -> RawId -> IO (Id JSValue)
objectForKeyedSubscript jsValue  key =
  sendMsg jsValue (mkSelector "objectForKeyedSubscript:") (retPtr retVoid) [argPtr (castPtr (unRawId key) :: Ptr ())] >>= retainedObject . castPtr

-- | @- objectAtIndexedSubscript:@
objectAtIndexedSubscript :: IsJSValue jsValue => jsValue -> CULong -> IO (Id JSValue)
objectAtIndexedSubscript jsValue  index =
  sendMsg jsValue (mkSelector "objectAtIndexedSubscript:") (retPtr retVoid) [argCULong (fromIntegral index)] >>= retainedObject . castPtr

-- | @- setObject:forKeyedSubscript:@
setObject_forKeyedSubscript :: IsJSValue jsValue => jsValue -> RawId -> RawId -> IO ()
setObject_forKeyedSubscript jsValue  object key =
  sendMsg jsValue (mkSelector "setObject:forKeyedSubscript:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (castPtr (unRawId key) :: Ptr ())]

-- | @- setObject:atIndexedSubscript:@
setObject_atIndexedSubscript :: IsJSValue jsValue => jsValue -> RawId -> CULong -> IO ()
setObject_atIndexedSubscript jsValue  object index =
  sendMsg jsValue (mkSelector "setObject:atIndexedSubscript:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ()), argCULong (fromIntegral index)]

-- | Access a property of a JSValue.
--
-- Returns: The JSValue for the requested property or the JSValue undefined if the property does not exist.
--
-- Corresponds to the JavaScript operation object[property]. Starting with macOS 10.15 and iOS 13, 'property' can be any 'id' and will be converted to a JSValue using the conversion rules of valueWithObject:inContext:. Prior to macOS 10.15 and iOS 13, 'property' was expected to be an NSString *.
--
-- ObjC selector: @- valueForProperty:@
valueForProperty :: IsJSValue jsValue => jsValue -> RawId -> IO (Id JSValue)
valueForProperty jsValue  property =
  sendMsg jsValue (mkSelector "valueForProperty:") (retPtr retVoid) [argPtr (castPtr (unRawId property) :: Ptr ())] >>= retainedObject . castPtr

-- | Set a property on a JSValue.
--
-- Corresponds to the JavaScript operation object[property] = value. Starting with macOS 10.15 and iOS 13, 'property' can be any 'id' and will be converted to a JSValue using the conversion rules of valueWithObject:inContext:. Prior to macOS 10.15 and iOS 13, 'property' was expected to be an NSString *.
--
-- ObjC selector: @- setValue:forProperty:@
setValue_forProperty :: IsJSValue jsValue => jsValue -> RawId -> RawId -> IO ()
setValue_forProperty jsValue  value property =
  sendMsg jsValue (mkSelector "setValue:forProperty:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr (unRawId property) :: Ptr ())]

-- | Delete a property from a JSValue.
--
-- Returns: YES if deletion is successful, NO otherwise.
--
-- Corresponds to the JavaScript operation delete object[property]. Starting with macOS 10.15 and iOS 13, 'property' can be any 'id' and will be converted to a JSValue using the conversion rules of valueWithObject:inContext:. Prior to macOS 10.15 and iOS 13, 'property' was expected to be an NSString *.
--
-- ObjC selector: @- deleteProperty:@
deleteProperty :: IsJSValue jsValue => jsValue -> RawId -> IO Bool
deleteProperty jsValue  property =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg jsValue (mkSelector "deleteProperty:") retCULong [argPtr (castPtr (unRawId property) :: Ptr ())]

-- | Check if a JSValue has a property.
--
-- This method has the same function as the JavaScript operator in.
--
-- Returns: Returns YES if property is present on the value.
--
-- Corresponds to the JavaScript operation property in object. Starting with macOS 10.15 and iOS 13, 'property' can be any 'id' and will be converted to a JSValue using the conversion rules of valueWithObject:inContext:. Prior to macOS 10.15 and iOS 13, 'property' was expected to be an NSString *.
--
-- ObjC selector: @- hasProperty:@
hasProperty :: IsJSValue jsValue => jsValue -> RawId -> IO Bool
hasProperty jsValue  property =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg jsValue (mkSelector "hasProperty:") retCULong [argPtr (castPtr (unRawId property) :: Ptr ())]

-- | Define properties with custom descriptors on JSValues.
--
-- This method may be used to create a data or accessor property on an object. This method operates in accordance with the Object.defineProperty method in the JavaScript language. Starting with macOS 10.15 and iOS 13, 'property' can be any 'id' and will be converted to a JSValue using the conversion rules of valueWithObject:inContext:. Prior to macOS 10.15 and iOS 13, 'property' was expected to be an NSString *.
--
-- ObjC selector: @- defineProperty:descriptor:@
defineProperty_descriptor :: IsJSValue jsValue => jsValue -> RawId -> RawId -> IO ()
defineProperty_descriptor jsValue  property descriptor =
  sendMsg jsValue (mkSelector "defineProperty:descriptor:") retVoid [argPtr (castPtr (unRawId property) :: Ptr ()), argPtr (castPtr (unRawId descriptor) :: Ptr ())]

-- | Access an indexed (numerical) property on a JSValue.
--
-- Returns: The JSValue for the property at the specified index. Returns the JavaScript value undefined if no property exists at that index.
--
-- ObjC selector: @- valueAtIndex:@
valueAtIndex :: IsJSValue jsValue => jsValue -> CULong -> IO (Id JSValue)
valueAtIndex jsValue  index =
  sendMsg jsValue (mkSelector "valueAtIndex:") (retPtr retVoid) [argCULong (fromIntegral index)] >>= retainedObject . castPtr

-- | Set an indexed (numerical) property on a JSValue.
--
-- For JSValues that are JavaScript arrays, indices greater than UINT_MAX - 1 will not affect the length of the array.
--
-- ObjC selector: @- setValue:atIndex:@
setValue_atIndex :: IsJSValue jsValue => jsValue -> RawId -> CULong -> IO ()
setValue_atIndex jsValue  value index =
  sendMsg jsValue (mkSelector "setValue:atIndex:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argCULong (fromIntegral index)]

-- | Create a JSValue from a NSRange.
--
-- Returns: A newly allocated JavaScript object containing properties named location and length, with values from the NSRange.
--
-- ObjC selector: @+ valueWithRange:inContext:@
valueWithRange_inContext :: IsJSContext context => NSRange -> context -> IO (Id JSValue)
valueWithRange_inContext range context =
  do
    cls' <- getRequiredClass "JSValue"
    withObjCPtr context $ \raw_context ->
      sendClassMsg cls' (mkSelector "valueWithRange:inContext:") (retPtr retVoid) [argNSRange range, argPtr (castPtr raw_context :: Ptr ())] >>= retainedObject . castPtr

-- | Convert a JSValue to an NSRange.
--
-- Reads the properties named location and length from this JSValue and converts the results to double.
--
-- Returns: The new NSRange.
--
-- ObjC selector: @- toRange@
toRange :: IsJSValue jsValue => jsValue -> IO NSRange
toRange jsValue  =
  sendMsgStret jsValue (mkSelector "toRange") retNSRange []

-- | The JSContext that this value originates from.
--
-- ObjC selector: @- context@
context :: IsJSValue jsValue => jsValue -> IO (Id JSContext)
context jsValue  =
  sendMsg jsValue (mkSelector "context") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Check if a JSValue corresponds to the JavaScript value undefined.
--
-- ObjC selector: @- isUndefined@
isUndefined :: IsJSValue jsValue => jsValue -> IO Bool
isUndefined jsValue  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg jsValue (mkSelector "isUndefined") retCULong []

-- | Check if a JSValue corresponds to the JavaScript value null.
--
-- ObjC selector: @- isNull@
isNull :: IsJSValue jsValue => jsValue -> IO Bool
isNull jsValue  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg jsValue (mkSelector "isNull") retCULong []

-- | Check if a JSValue is a boolean.
--
-- ObjC selector: @- isBoolean@
isBoolean :: IsJSValue jsValue => jsValue -> IO Bool
isBoolean jsValue  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg jsValue (mkSelector "isBoolean") retCULong []

-- | Check if a JSValue is a number.
--
-- In JavaScript, there is no differentiation between types of numbers. Semantically all numbers behave like doubles except in special cases like bit operations.
--
-- ObjC selector: @- isNumber@
isNumber :: IsJSValue jsValue => jsValue -> IO Bool
isNumber jsValue  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg jsValue (mkSelector "isNumber") retCULong []

-- | Check if a JSValue is a string.
--
-- ObjC selector: @- isString@
isString :: IsJSValue jsValue => jsValue -> IO Bool
isString jsValue  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg jsValue (mkSelector "isString") retCULong []

-- | Check if a JSValue is an object.
--
-- ObjC selector: @- isObject@
isObject :: IsJSValue jsValue => jsValue -> IO Bool
isObject jsValue  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg jsValue (mkSelector "isObject") retCULong []

-- | Check if a JSValue is an array.
--
-- ObjC selector: @- isArray@
isArray :: IsJSValue jsValue => jsValue -> IO Bool
isArray jsValue  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg jsValue (mkSelector "isArray") retCULong []

-- | Check if a JSValue is a date.
--
-- ObjC selector: @- isDate@
isDate :: IsJSValue jsValue => jsValue -> IO Bool
isDate jsValue  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg jsValue (mkSelector "isDate") retCULong []

-- | Check if a JSValue is a symbol.
--
-- ObjC selector: @- isSymbol@
isSymbol :: IsJSValue jsValue => jsValue -> IO Bool
isSymbol jsValue  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg jsValue (mkSelector "isSymbol") retCULong []

-- | Check if a JSValue is a BigInt.
--
-- ObjC selector: @- isBigInt@
isBigInt :: IsJSValue jsValue => jsValue -> IO Bool
isBigInt jsValue  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg jsValue (mkSelector "isBigInt") retCULong []

-- | Returns the C API counterpart wrapped by a JSContext.
--
-- Returns: The C API equivalent of this JSValue.
--
-- ObjC selector: @- JSValueRef@
jsValueRef :: IsJSValue jsValue => jsValue -> IO RawId
jsValueRef jsValue  =
  fmap (RawId . castPtr) $ sendMsg jsValue (mkSelector "JSValueRef") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valueWithObject:inContext:@
valueWithObject_inContextSelector :: Selector
valueWithObject_inContextSelector = mkSelector "valueWithObject:inContext:"

-- | @Selector@ for @valueWithBool:inContext:@
valueWithBool_inContextSelector :: Selector
valueWithBool_inContextSelector = mkSelector "valueWithBool:inContext:"

-- | @Selector@ for @valueWithDouble:inContext:@
valueWithDouble_inContextSelector :: Selector
valueWithDouble_inContextSelector = mkSelector "valueWithDouble:inContext:"

-- | @Selector@ for @valueWithInt32:inContext:@
valueWithInt32_inContextSelector :: Selector
valueWithInt32_inContextSelector = mkSelector "valueWithInt32:inContext:"

-- | @Selector@ for @valueWithUInt32:inContext:@
valueWithUInt32_inContextSelector :: Selector
valueWithUInt32_inContextSelector = mkSelector "valueWithUInt32:inContext:"

-- | @Selector@ for @valueWithNewObjectInContext:@
valueWithNewObjectInContextSelector :: Selector
valueWithNewObjectInContextSelector = mkSelector "valueWithNewObjectInContext:"

-- | @Selector@ for @valueWithNewArrayInContext:@
valueWithNewArrayInContextSelector :: Selector
valueWithNewArrayInContextSelector = mkSelector "valueWithNewArrayInContext:"

-- | @Selector@ for @valueWithNewRegularExpressionFromPattern:flags:inContext:@
valueWithNewRegularExpressionFromPattern_flags_inContextSelector :: Selector
valueWithNewRegularExpressionFromPattern_flags_inContextSelector = mkSelector "valueWithNewRegularExpressionFromPattern:flags:inContext:"

-- | @Selector@ for @valueWithNewErrorFromMessage:inContext:@
valueWithNewErrorFromMessage_inContextSelector :: Selector
valueWithNewErrorFromMessage_inContextSelector = mkSelector "valueWithNewErrorFromMessage:inContext:"

-- | @Selector@ for @valueWithNewPromiseInContext:fromExecutor:@
valueWithNewPromiseInContext_fromExecutorSelector :: Selector
valueWithNewPromiseInContext_fromExecutorSelector = mkSelector "valueWithNewPromiseInContext:fromExecutor:"

-- | @Selector@ for @valueWithNewPromiseResolvedWithResult:inContext:@
valueWithNewPromiseResolvedWithResult_inContextSelector :: Selector
valueWithNewPromiseResolvedWithResult_inContextSelector = mkSelector "valueWithNewPromiseResolvedWithResult:inContext:"

-- | @Selector@ for @valueWithNewPromiseRejectedWithReason:inContext:@
valueWithNewPromiseRejectedWithReason_inContextSelector :: Selector
valueWithNewPromiseRejectedWithReason_inContextSelector = mkSelector "valueWithNewPromiseRejectedWithReason:inContext:"

-- | @Selector@ for @valueWithNewSymbolFromDescription:inContext:@
valueWithNewSymbolFromDescription_inContextSelector :: Selector
valueWithNewSymbolFromDescription_inContextSelector = mkSelector "valueWithNewSymbolFromDescription:inContext:"

-- | @Selector@ for @valueWithNewBigIntFromString:inContext:@
valueWithNewBigIntFromString_inContextSelector :: Selector
valueWithNewBigIntFromString_inContextSelector = mkSelector "valueWithNewBigIntFromString:inContext:"

-- | @Selector@ for @valueWithNewBigIntFromInt64:inContext:@
valueWithNewBigIntFromInt64_inContextSelector :: Selector
valueWithNewBigIntFromInt64_inContextSelector = mkSelector "valueWithNewBigIntFromInt64:inContext:"

-- | @Selector@ for @valueWithNewBigIntFromUInt64:inContext:@
valueWithNewBigIntFromUInt64_inContextSelector :: Selector
valueWithNewBigIntFromUInt64_inContextSelector = mkSelector "valueWithNewBigIntFromUInt64:inContext:"

-- | @Selector@ for @valueWithNewBigIntFromDouble:inContext:@
valueWithNewBigIntFromDouble_inContextSelector :: Selector
valueWithNewBigIntFromDouble_inContextSelector = mkSelector "valueWithNewBigIntFromDouble:inContext:"

-- | @Selector@ for @valueWithNullInContext:@
valueWithNullInContextSelector :: Selector
valueWithNullInContextSelector = mkSelector "valueWithNullInContext:"

-- | @Selector@ for @valueWithUndefinedInContext:@
valueWithUndefinedInContextSelector :: Selector
valueWithUndefinedInContextSelector = mkSelector "valueWithUndefinedInContext:"

-- | @Selector@ for @toObject@
toObjectSelector :: Selector
toObjectSelector = mkSelector "toObject"

-- | @Selector@ for @toObjectOfClass:@
toObjectOfClassSelector :: Selector
toObjectOfClassSelector = mkSelector "toObjectOfClass:"

-- | @Selector@ for @toBool@
toBoolSelector :: Selector
toBoolSelector = mkSelector "toBool"

-- | @Selector@ for @toDouble@
toDoubleSelector :: Selector
toDoubleSelector = mkSelector "toDouble"

-- | @Selector@ for @toInt32@
toInt32Selector :: Selector
toInt32Selector = mkSelector "toInt32"

-- | @Selector@ for @toUInt32@
toUInt32Selector :: Selector
toUInt32Selector = mkSelector "toUInt32"

-- | @Selector@ for @toInt64@
toInt64Selector :: Selector
toInt64Selector = mkSelector "toInt64"

-- | @Selector@ for @toUInt64@
toUInt64Selector :: Selector
toUInt64Selector = mkSelector "toUInt64"

-- | @Selector@ for @toNumber@
toNumberSelector :: Selector
toNumberSelector = mkSelector "toNumber"

-- | @Selector@ for @toString@
toStringSelector :: Selector
toStringSelector = mkSelector "toString"

-- | @Selector@ for @toDate@
toDateSelector :: Selector
toDateSelector = mkSelector "toDate"

-- | @Selector@ for @toArray@
toArraySelector :: Selector
toArraySelector = mkSelector "toArray"

-- | @Selector@ for @toDictionary@
toDictionarySelector :: Selector
toDictionarySelector = mkSelector "toDictionary"

-- | @Selector@ for @isInstanceOf:@
isInstanceOfSelector :: Selector
isInstanceOfSelector = mkSelector "isInstanceOf:"

-- | @Selector@ for @isEqualToObject:@
isEqualToObjectSelector :: Selector
isEqualToObjectSelector = mkSelector "isEqualToObject:"

-- | @Selector@ for @isEqualWithTypeCoercionToObject:@
isEqualWithTypeCoercionToObjectSelector :: Selector
isEqualWithTypeCoercionToObjectSelector = mkSelector "isEqualWithTypeCoercionToObject:"

-- | @Selector@ for @compareJSValue:@
compareJSValueSelector :: Selector
compareJSValueSelector = mkSelector "compareJSValue:"

-- | @Selector@ for @compareInt64:@
compareInt64Selector :: Selector
compareInt64Selector = mkSelector "compareInt64:"

-- | @Selector@ for @compareUInt64:@
compareUInt64Selector :: Selector
compareUInt64Selector = mkSelector "compareUInt64:"

-- | @Selector@ for @compareDouble:@
compareDoubleSelector :: Selector
compareDoubleSelector = mkSelector "compareDouble:"

-- | @Selector@ for @callWithArguments:@
callWithArgumentsSelector :: Selector
callWithArgumentsSelector = mkSelector "callWithArguments:"

-- | @Selector@ for @constructWithArguments:@
constructWithArgumentsSelector :: Selector
constructWithArgumentsSelector = mkSelector "constructWithArguments:"

-- | @Selector@ for @invokeMethod:withArguments:@
invokeMethod_withArgumentsSelector :: Selector
invokeMethod_withArgumentsSelector = mkSelector "invokeMethod:withArguments:"

-- | @Selector@ for @valueWithJSValueRef:inContext:@
valueWithJSValueRef_inContextSelector :: Selector
valueWithJSValueRef_inContextSelector = mkSelector "valueWithJSValueRef:inContext:"

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @objectAtIndexedSubscript:@
objectAtIndexedSubscriptSelector :: Selector
objectAtIndexedSubscriptSelector = mkSelector "objectAtIndexedSubscript:"

-- | @Selector@ for @setObject:forKeyedSubscript:@
setObject_forKeyedSubscriptSelector :: Selector
setObject_forKeyedSubscriptSelector = mkSelector "setObject:forKeyedSubscript:"

-- | @Selector@ for @setObject:atIndexedSubscript:@
setObject_atIndexedSubscriptSelector :: Selector
setObject_atIndexedSubscriptSelector = mkSelector "setObject:atIndexedSubscript:"

-- | @Selector@ for @valueForProperty:@
valueForPropertySelector :: Selector
valueForPropertySelector = mkSelector "valueForProperty:"

-- | @Selector@ for @setValue:forProperty:@
setValue_forPropertySelector :: Selector
setValue_forPropertySelector = mkSelector "setValue:forProperty:"

-- | @Selector@ for @deleteProperty:@
deletePropertySelector :: Selector
deletePropertySelector = mkSelector "deleteProperty:"

-- | @Selector@ for @hasProperty:@
hasPropertySelector :: Selector
hasPropertySelector = mkSelector "hasProperty:"

-- | @Selector@ for @defineProperty:descriptor:@
defineProperty_descriptorSelector :: Selector
defineProperty_descriptorSelector = mkSelector "defineProperty:descriptor:"

-- | @Selector@ for @valueAtIndex:@
valueAtIndexSelector :: Selector
valueAtIndexSelector = mkSelector "valueAtIndex:"

-- | @Selector@ for @setValue:atIndex:@
setValue_atIndexSelector :: Selector
setValue_atIndexSelector = mkSelector "setValue:atIndex:"

-- | @Selector@ for @valueWithRange:inContext:@
valueWithRange_inContextSelector :: Selector
valueWithRange_inContextSelector = mkSelector "valueWithRange:inContext:"

-- | @Selector@ for @toRange@
toRangeSelector :: Selector
toRangeSelector = mkSelector "toRange"

-- | @Selector@ for @context@
contextSelector :: Selector
contextSelector = mkSelector "context"

-- | @Selector@ for @isUndefined@
isUndefinedSelector :: Selector
isUndefinedSelector = mkSelector "isUndefined"

-- | @Selector@ for @isNull@
isNullSelector :: Selector
isNullSelector = mkSelector "isNull"

-- | @Selector@ for @isBoolean@
isBooleanSelector :: Selector
isBooleanSelector = mkSelector "isBoolean"

-- | @Selector@ for @isNumber@
isNumberSelector :: Selector
isNumberSelector = mkSelector "isNumber"

-- | @Selector@ for @isString@
isStringSelector :: Selector
isStringSelector = mkSelector "isString"

-- | @Selector@ for @isObject@
isObjectSelector :: Selector
isObjectSelector = mkSelector "isObject"

-- | @Selector@ for @isArray@
isArraySelector :: Selector
isArraySelector = mkSelector "isArray"

-- | @Selector@ for @isDate@
isDateSelector :: Selector
isDateSelector = mkSelector "isDate"

-- | @Selector@ for @isSymbol@
isSymbolSelector :: Selector
isSymbolSelector = mkSelector "isSymbol"

-- | @Selector@ for @isBigInt@
isBigIntSelector :: Selector
isBigIntSelector = mkSelector "isBigInt"

-- | @Selector@ for @JSValueRef@
jsValueRefSelector :: Selector
jsValueRefSelector = mkSelector "JSValueRef"

