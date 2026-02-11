{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | JSManagedValue represents a "conditionally retained" JSValue.  "Conditionally retained" means that as long as the JSManagedValue's  JSValue is reachable through the JavaScript object graph, or through the Objective-C object graph reported to the JSVirtualMachine using addManagedReference:withOwner:, the corresponding JSValue will  be retained. However, if neither graph reaches the JSManagedValue, the  corresponding JSValue will be released and set to nil.
--
-- The primary use for a JSManagedValue is to store a JSValue in an Objective-Cor Swift object that is exported to JavaScript. It is incorrect to store a JSValuein an object that is exported to JavaScript, since doing so creates a retain cycle.
--
-- Generated bindings for @JSManagedValue@.
module ObjC.JavaScriptCore.JSManagedValue
  ( JSManagedValue
  , IsJSManagedValue(..)
  , managedValueWithValue
  , managedValueWithValue_andOwner
  , initWithValue
  , value
  , managedValueWithValueSelector
  , managedValueWithValue_andOwnerSelector
  , initWithValueSelector
  , valueSelector


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

-- | Create a JSManagedValue from a JSValue.
--
-- Returns: The new JSManagedValue.
--
-- ObjC selector: @+ managedValueWithValue:@
managedValueWithValue :: IsJSValue value => value -> IO (Id JSManagedValue)
managedValueWithValue value =
  do
    cls' <- getRequiredClass "JSManagedValue"
    withObjCPtr value $ \raw_value ->
      sendClassMsg cls' (mkSelector "managedValueWithValue:") (retPtr retVoid) [argPtr (castPtr raw_value :: Ptr ())] >>= retainedObject . castPtr

-- | @+ managedValueWithValue:andOwner:@
managedValueWithValue_andOwner :: IsJSValue value => value -> RawId -> IO (Id JSManagedValue)
managedValueWithValue_andOwner value owner =
  do
    cls' <- getRequiredClass "JSManagedValue"
    withObjCPtr value $ \raw_value ->
      sendClassMsg cls' (mkSelector "managedValueWithValue:andOwner:") (retPtr retVoid) [argPtr (castPtr raw_value :: Ptr ()), argPtr (castPtr (unRawId owner) :: Ptr ())] >>= retainedObject . castPtr

-- | Create a JSManagedValue.
--
-- Returns: The new JSManagedValue.
--
-- ObjC selector: @- initWithValue:@
initWithValue :: (IsJSManagedValue jsManagedValue, IsJSValue value) => jsManagedValue -> value -> IO (Id JSManagedValue)
initWithValue jsManagedValue  value =
withObjCPtr value $ \raw_value ->
    sendMsg jsManagedValue (mkSelector "initWithValue:") (retPtr retVoid) [argPtr (castPtr raw_value :: Ptr ())] >>= ownedObject . castPtr

-- | Get the JSValue from the JSManagedValue.
--
-- Returns: The corresponding JSValue for this JSManagedValue or  nil if the JSValue has been collected.
--
-- ObjC selector: @- value@
value :: IsJSManagedValue jsManagedValue => jsManagedValue -> IO (Id JSValue)
value jsManagedValue  =
  sendMsg jsManagedValue (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @managedValueWithValue:@
managedValueWithValueSelector :: Selector
managedValueWithValueSelector = mkSelector "managedValueWithValue:"

-- | @Selector@ for @managedValueWithValue:andOwner:@
managedValueWithValue_andOwnerSelector :: Selector
managedValueWithValue_andOwnerSelector = mkSelector "managedValueWithValue:andOwner:"

-- | @Selector@ for @initWithValue:@
initWithValueSelector :: Selector
initWithValueSelector = mkSelector "initWithValue:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

