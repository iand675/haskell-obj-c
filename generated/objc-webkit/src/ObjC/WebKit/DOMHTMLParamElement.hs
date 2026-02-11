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
  , typeSelector
  , setTypeSelector
  , valueSelector
  , setValueSelector
  , valueTypeSelector
  , setValueTypeSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsDOMHTMLParamElement domhtmlParamElement => domhtmlParamElement -> IO (Id NSString)
name domhtmlParamElement  =
  sendMsg domhtmlParamElement (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsDOMHTMLParamElement domhtmlParamElement, IsNSString value) => domhtmlParamElement -> value -> IO ()
setName domhtmlParamElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlParamElement (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- type@
type_ :: IsDOMHTMLParamElement domhtmlParamElement => domhtmlParamElement -> IO (Id NSString)
type_ domhtmlParamElement  =
  sendMsg domhtmlParamElement (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsDOMHTMLParamElement domhtmlParamElement, IsNSString value) => domhtmlParamElement -> value -> IO ()
setType domhtmlParamElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlParamElement (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsDOMHTMLParamElement domhtmlParamElement => domhtmlParamElement -> IO (Id NSString)
value domhtmlParamElement  =
  sendMsg domhtmlParamElement (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsDOMHTMLParamElement domhtmlParamElement, IsNSString value) => domhtmlParamElement -> value -> IO ()
setValue domhtmlParamElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlParamElement (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- valueType@
valueType :: IsDOMHTMLParamElement domhtmlParamElement => domhtmlParamElement -> IO (Id NSString)
valueType domhtmlParamElement  =
  sendMsg domhtmlParamElement (mkSelector "valueType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValueType:@
setValueType :: (IsDOMHTMLParamElement domhtmlParamElement, IsNSString value) => domhtmlParamElement -> value -> IO ()
setValueType domhtmlParamElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlParamElement (mkSelector "setValueType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @valueType@
valueTypeSelector :: Selector
valueTypeSelector = mkSelector "valueType"

-- | @Selector@ for @setValueType:@
setValueTypeSelector :: Selector
setValueTypeSelector = mkSelector "setValueType:"

