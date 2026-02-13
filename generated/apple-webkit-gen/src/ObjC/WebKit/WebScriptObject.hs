{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | WebScriptObject
--
-- WebScriptObjects are used to wrap script objects passed from    script environments to Objective-C. WebScriptObjects cannot be created    directly. In normal uses of WebKit, you gain access to the script    environment using the "windowScriptObject" method on WebView.
--
-- The following KVC methods are commonly used to access properties of the    WebScriptObject:
--
-- - (void)setValue:(id)value forKey:(NSString *)key        - (id)valueForKey:(NSString *)key
--
-- As it possible to remove attributes from web script objects, the following    additional method augments the basic KVC methods:
--
-- - (void)removeWebScriptKey:(NSString *)name;
--
-- Also, since the sparse array access allowed in script objects doesn't map well    to NSArray, the following methods can be used to access index based properties:
--
-- - (id)webScriptValueAtIndex:(unsigned)index;        - (void)setWebScriptValueAtIndex:(unsigned)index value:(id)value;
--
-- Generated bindings for @WebScriptObject@.
module ObjC.WebKit.WebScriptObject
  ( WebScriptObject
  , IsWebScriptObject(..)
  , throwException
  , jsObject
  , callWebScriptMethod_withArguments
  , evaluateWebScript
  , removeWebScriptKey
  , stringRepresentation
  , webScriptValueAtIndex
  , setWebScriptValueAtIndex_value
  , setException
  , jsValue
  , callWebScriptMethod_withArgumentsSelector
  , evaluateWebScriptSelector
  , jsObjectSelector
  , jsValueSelector
  , removeWebScriptKeySelector
  , setExceptionSelector
  , setWebScriptValueAtIndex_valueSelector
  , stringRepresentationSelector
  , throwExceptionSelector
  , webScriptValueAtIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.JavaScriptCore.Internal.Classes

-- | throwException:
--
-- Throws an exception in the current script execution context.
--
-- Returns: Either NO if an exception could not be raised, YES otherwise.
--
-- ObjC selector: @+ throwException:@
throwException :: IsNSString exceptionMessage => exceptionMessage -> IO Bool
throwException exceptionMessage =
  do
    cls' <- getRequiredClass "WebScriptObject"
    sendClassMessage cls' throwExceptionSelector (toNSString exceptionMessage)

-- | JSObject
--
-- Returns: The equivalent JSObjectRef for this WebScriptObject.
--
-- Use this method to bridge between the WebScriptObject and     JavaScriptCore APIs.
--
-- ObjC selector: @- JSObject@
jsObject :: IsWebScriptObject webScriptObject => webScriptObject -> IO (Ptr ())
jsObject webScriptObject =
  sendMessage webScriptObject jsObjectSelector

-- | callWebScriptMethod:withArguments:
--
-- @name@ — The name of the method to call in the script environment.
--
-- @arguments@ — The arguments to pass to the script environment.
--
-- Calls the specified method in the script environment using the    specified arguments.
--
-- Returns: Returns the result of calling the script method.    Returns WebUndefined when an exception is thrown in the script environment.
--
-- ObjC selector: @- callWebScriptMethod:withArguments:@
callWebScriptMethod_withArguments :: (IsWebScriptObject webScriptObject, IsNSString name, IsNSArray arguments) => webScriptObject -> name -> arguments -> IO RawId
callWebScriptMethod_withArguments webScriptObject name arguments =
  sendMessage webScriptObject callWebScriptMethod_withArgumentsSelector (toNSString name) (toNSArray arguments)

-- | evaluateWebScript:
--
-- @script@ — The script to execute in the target script environment.
--
-- The script will be executed in the target script environment. The format    of the script is dependent of the target script environment.
--
-- Returns: Returns the result of evaluating the script in the script environment.    Returns WebUndefined when an exception is thrown in the script environment.
--
-- ObjC selector: @- evaluateWebScript:@
evaluateWebScript :: (IsWebScriptObject webScriptObject, IsNSString script) => webScriptObject -> script -> IO RawId
evaluateWebScript webScriptObject script =
  sendMessage webScriptObject evaluateWebScriptSelector (toNSString script)

-- | removeWebScriptKey:
--
-- @name@ — The name of the property to remove.
--
-- Removes the property from the object in the script environment.
--
-- ObjC selector: @- removeWebScriptKey:@
removeWebScriptKey :: (IsWebScriptObject webScriptObject, IsNSString name) => webScriptObject -> name -> IO ()
removeWebScriptKey webScriptObject name =
  sendMessage webScriptObject removeWebScriptKeySelector (toNSString name)

-- | stringRepresentation
--
-- Converts the target object to a string representation. The coercion    of non string objects type is dependent on the script environment.
--
-- Returns: Returns the string representation of the object.
--
-- ObjC selector: @- stringRepresentation@
stringRepresentation :: IsWebScriptObject webScriptObject => webScriptObject -> IO (Id NSString)
stringRepresentation webScriptObject =
  sendMessage webScriptObject stringRepresentationSelector

-- | webScriptValueAtIndex:
--
-- @index@ — The index of the property to return.
--
-- Gets the value of the property at the specified index.
--
-- Returns: The value of the property. Returns WebUndefined when an exception is    thrown in the script environment.
--
-- ObjC selector: @- webScriptValueAtIndex:@
webScriptValueAtIndex :: IsWebScriptObject webScriptObject => webScriptObject -> CUInt -> IO RawId
webScriptValueAtIndex webScriptObject index =
  sendMessage webScriptObject webScriptValueAtIndexSelector index

-- | setWebScriptValueAtIndex:value:
--
-- @index@ — The index of the property to set.
--
-- @value@ — The value of the property to set.
--
-- Sets the property value at the specified index.
--
-- ObjC selector: @- setWebScriptValueAtIndex:value:@
setWebScriptValueAtIndex_value :: IsWebScriptObject webScriptObject => webScriptObject -> CUInt -> RawId -> IO ()
setWebScriptValueAtIndex_value webScriptObject index value =
  sendMessage webScriptObject setWebScriptValueAtIndex_valueSelector index value

-- | setException:
--
-- @description@ — The description of the exception.
--
-- Raises an exception in the script environment in the context of the    current object.
--
-- ObjC selector: @- setException:@
setException :: (IsWebScriptObject webScriptObject, IsNSString description) => webScriptObject -> description -> IO ()
setException webScriptObject description =
  sendMessage webScriptObject setExceptionSelector (toNSString description)

-- | JSValue
--
-- Returns: The equivalent Objective-C JSValue for this WebScriptObject.
--
-- Use this method to bridge between the WebScriptObject and     JavaScriptCore Objective-C APIs.
--
-- ObjC selector: @- JSValue@
jsValue :: IsWebScriptObject webScriptObject => webScriptObject -> IO (Id JSValue)
jsValue webScriptObject =
  sendMessage webScriptObject jsValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @throwException:@
throwExceptionSelector :: Selector '[Id NSString] Bool
throwExceptionSelector = mkSelector "throwException:"

-- | @Selector@ for @JSObject@
jsObjectSelector :: Selector '[] (Ptr ())
jsObjectSelector = mkSelector "JSObject"

-- | @Selector@ for @callWebScriptMethod:withArguments:@
callWebScriptMethod_withArgumentsSelector :: Selector '[Id NSString, Id NSArray] RawId
callWebScriptMethod_withArgumentsSelector = mkSelector "callWebScriptMethod:withArguments:"

-- | @Selector@ for @evaluateWebScript:@
evaluateWebScriptSelector :: Selector '[Id NSString] RawId
evaluateWebScriptSelector = mkSelector "evaluateWebScript:"

-- | @Selector@ for @removeWebScriptKey:@
removeWebScriptKeySelector :: Selector '[Id NSString] ()
removeWebScriptKeySelector = mkSelector "removeWebScriptKey:"

-- | @Selector@ for @stringRepresentation@
stringRepresentationSelector :: Selector '[] (Id NSString)
stringRepresentationSelector = mkSelector "stringRepresentation"

-- | @Selector@ for @webScriptValueAtIndex:@
webScriptValueAtIndexSelector :: Selector '[CUInt] RawId
webScriptValueAtIndexSelector = mkSelector "webScriptValueAtIndex:"

-- | @Selector@ for @setWebScriptValueAtIndex:value:@
setWebScriptValueAtIndex_valueSelector :: Selector '[CUInt, RawId] ()
setWebScriptValueAtIndex_valueSelector = mkSelector "setWebScriptValueAtIndex:value:"

-- | @Selector@ for @setException:@
setExceptionSelector :: Selector '[Id NSString] ()
setExceptionSelector = mkSelector "setException:"

-- | @Selector@ for @JSValue@
jsValueSelector :: Selector '[] (Id JSValue)
jsValueSelector = mkSelector "JSValue"

