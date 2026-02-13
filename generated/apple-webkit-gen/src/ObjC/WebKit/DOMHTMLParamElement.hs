{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLParamElement@.
module ObjC.WebKit.DOMHTMLParamElement
  ( DOMHTMLParamElement
  , IsDOMHTMLParamElement(..)
  , name
  , setName
  , type_
  , setType
  , value
  , setValue
  , valueType
  , setValueType
  , nameSelector
  , setNameSelector
  , setTypeSelector
  , setValueSelector
  , setValueTypeSelector
  , typeSelector
  , valueSelector
  , valueTypeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsDOMHTMLParamElement domhtmlParamElement => domhtmlParamElement -> IO (Id NSString)
name domhtmlParamElement =
  sendMessage domhtmlParamElement nameSelector

-- | @- setName:@
setName :: (IsDOMHTMLParamElement domhtmlParamElement, IsNSString value) => domhtmlParamElement -> value -> IO ()
setName domhtmlParamElement value =
  sendMessage domhtmlParamElement setNameSelector (toNSString value)

-- | @- type@
type_ :: IsDOMHTMLParamElement domhtmlParamElement => domhtmlParamElement -> IO (Id NSString)
type_ domhtmlParamElement =
  sendMessage domhtmlParamElement typeSelector

-- | @- setType:@
setType :: (IsDOMHTMLParamElement domhtmlParamElement, IsNSString value) => domhtmlParamElement -> value -> IO ()
setType domhtmlParamElement value =
  sendMessage domhtmlParamElement setTypeSelector (toNSString value)

-- | @- value@
value :: IsDOMHTMLParamElement domhtmlParamElement => domhtmlParamElement -> IO (Id NSString)
value domhtmlParamElement =
  sendMessage domhtmlParamElement valueSelector

-- | @- setValue:@
setValue :: (IsDOMHTMLParamElement domhtmlParamElement, IsNSString value) => domhtmlParamElement -> value -> IO ()
setValue domhtmlParamElement value =
  sendMessage domhtmlParamElement setValueSelector (toNSString value)

-- | @- valueType@
valueType :: IsDOMHTMLParamElement domhtmlParamElement => domhtmlParamElement -> IO (Id NSString)
valueType domhtmlParamElement =
  sendMessage domhtmlParamElement valueTypeSelector

-- | @- setValueType:@
setValueType :: (IsDOMHTMLParamElement domhtmlParamElement, IsNSString value) => domhtmlParamElement -> value -> IO ()
setValueType domhtmlParamElement value =
  sendMessage domhtmlParamElement setValueTypeSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[Id NSString] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSString)
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSString] ()
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @valueType@
valueTypeSelector :: Selector '[] (Id NSString)
valueTypeSelector = mkSelector "valueType"

-- | @Selector@ for @setValueType:@
setValueTypeSelector :: Selector '[Id NSString] ()
setValueTypeSelector = mkSelector "setValueType:"

