{-# LANGUAGE DataKinds #-}
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
  , initWithValueSelector
  , managedValueWithValueSelector
  , managedValueWithValue_andOwnerSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' managedValueWithValueSelector (toJSValue value)

-- | @+ managedValueWithValue:andOwner:@
managedValueWithValue_andOwner :: IsJSValue value => value -> RawId -> IO (Id JSManagedValue)
managedValueWithValue_andOwner value owner =
  do
    cls' <- getRequiredClass "JSManagedValue"
    sendClassMessage cls' managedValueWithValue_andOwnerSelector (toJSValue value) owner

-- | Create a JSManagedValue.
--
-- Returns: The new JSManagedValue.
--
-- ObjC selector: @- initWithValue:@
initWithValue :: (IsJSManagedValue jsManagedValue, IsJSValue value) => jsManagedValue -> value -> IO (Id JSManagedValue)
initWithValue jsManagedValue value =
  sendOwnedMessage jsManagedValue initWithValueSelector (toJSValue value)

-- | Get the JSValue from the JSManagedValue.
--
-- Returns: The corresponding JSValue for this JSManagedValue or  nil if the JSValue has been collected.
--
-- ObjC selector: @- value@
value :: IsJSManagedValue jsManagedValue => jsManagedValue -> IO (Id JSValue)
value jsManagedValue =
  sendMessage jsManagedValue valueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @managedValueWithValue:@
managedValueWithValueSelector :: Selector '[Id JSValue] (Id JSManagedValue)
managedValueWithValueSelector = mkSelector "managedValueWithValue:"

-- | @Selector@ for @managedValueWithValue:andOwner:@
managedValueWithValue_andOwnerSelector :: Selector '[Id JSValue, RawId] (Id JSManagedValue)
managedValueWithValue_andOwnerSelector = mkSelector "managedValueWithValue:andOwner:"

-- | @Selector@ for @initWithValue:@
initWithValueSelector :: Selector '[Id JSValue] (Id JSManagedValue)
initWithValueSelector = mkSelector "initWithValue:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id JSValue)
valueSelector = mkSelector "value"

