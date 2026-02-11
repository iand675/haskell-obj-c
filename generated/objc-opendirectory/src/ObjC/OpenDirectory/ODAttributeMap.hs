{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ODAttributeMap@.
module ObjC.OpenDirectory.ODAttributeMap
  ( ODAttributeMap
  , IsODAttributeMap(..)
  , attributeMapWithValue
  , attributeMapWithStaticValue
  , setStaticValue
  , setVariableSubstitution
  , customQueryFunction
  , setCustomQueryFunction
  , customTranslationFunction
  , setCustomTranslationFunction
  , customAttributes
  , setCustomAttributes
  , value
  , setValue
  , attributeMapWithValueSelector
  , attributeMapWithStaticValueSelector
  , setStaticValueSelector
  , setVariableSubstitutionSelector
  , customQueryFunctionSelector
  , setCustomQueryFunctionSelector
  , customTranslationFunctionSelector
  , setCustomTranslationFunctionSelector
  , customAttributesSelector
  , setCustomAttributesSelector
  , valueSelector
  , setValueSelector


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

import ObjC.OpenDirectory.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | attributeMapWithValue:
--
-- Returns an initialized and autoreleased ODAttributeMap object with the given value mapped.
--
-- Returns an initialized and autoreleased ODAttributeMap object with the given value mapped.
--
-- ObjC selector: @+ attributeMapWithValue:@
attributeMapWithValue :: IsNSString value => value -> IO (Id ODAttributeMap)
attributeMapWithValue value =
  do
    cls' <- getRequiredClass "ODAttributeMap"
    withObjCPtr value $ \raw_value ->
      sendClassMsg cls' (mkSelector "attributeMapWithValue:") (retPtr retVoid) [argPtr (castPtr raw_value :: Ptr ())] >>= retainedObject . castPtr

-- | attributeMapWithStaticValue:
--
-- Returns an initialized and autoreleased ODAttributeMap object with the given static value.
--
-- Returns an initialized and autoreleased ODAttributeMap object with the given static value.
--
-- ObjC selector: @+ attributeMapWithStaticValue:@
attributeMapWithStaticValue :: IsNSString staticValue => staticValue -> IO (Id ODAttributeMap)
attributeMapWithStaticValue staticValue =
  do
    cls' <- getRequiredClass "ODAttributeMap"
    withObjCPtr staticValue $ \raw_staticValue ->
      sendClassMsg cls' (mkSelector "attributeMapWithStaticValue:") (retPtr retVoid) [argPtr (castPtr raw_staticValue :: Ptr ())] >>= retainedObject . castPtr

-- | setStaticValue:
--
-- Sets a static value that will always be returned for this mapping.
--
-- Sets a static value that will always be returned for this mapping, i.e., "20".
--
-- ObjC selector: @- setStaticValue:@
setStaticValue :: (IsODAttributeMap odAttributeMap, IsNSString staticValue) => odAttributeMap -> staticValue -> IO ()
setStaticValue odAttributeMap  staticValue =
withObjCPtr staticValue $ \raw_staticValue ->
    sendMsg odAttributeMap (mkSelector "setStaticValue:") retVoid [argPtr (castPtr raw_staticValue :: Ptr ())]

-- | setVariableSubstitution:
--
-- Sets a variable substitution-based value.
--
-- Value should be using the syntax '$native$' for all substited values.  For example, to form a home directory using the "cn" of an LDAP record, substitution could be done with "/home/$cn$".
--
-- ObjC selector: @- setVariableSubstitution:@
setVariableSubstitution :: (IsODAttributeMap odAttributeMap, IsNSString variableSubstitution) => odAttributeMap -> variableSubstitution -> IO ()
setVariableSubstitution odAttributeMap  variableSubstitution =
withObjCPtr variableSubstitution $ \raw_variableSubstitution ->
    sendMsg odAttributeMap (mkSelector "setVariableSubstitution:") retVoid [argPtr (castPtr raw_variableSubstitution :: Ptr ())]

-- | @- customQueryFunction@
customQueryFunction :: IsODAttributeMap odAttributeMap => odAttributeMap -> IO (Id NSString)
customQueryFunction odAttributeMap  =
  sendMsg odAttributeMap (mkSelector "customQueryFunction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCustomQueryFunction:@
setCustomQueryFunction :: (IsODAttributeMap odAttributeMap, IsNSString value) => odAttributeMap -> value -> IO ()
setCustomQueryFunction odAttributeMap  value =
withObjCPtr value $ \raw_value ->
    sendMsg odAttributeMap (mkSelector "setCustomQueryFunction:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- customTranslationFunction@
customTranslationFunction :: IsODAttributeMap odAttributeMap => odAttributeMap -> IO (Id NSString)
customTranslationFunction odAttributeMap  =
  sendMsg odAttributeMap (mkSelector "customTranslationFunction") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCustomTranslationFunction:@
setCustomTranslationFunction :: (IsODAttributeMap odAttributeMap, IsNSString value) => odAttributeMap -> value -> IO ()
setCustomTranslationFunction odAttributeMap  value =
withObjCPtr value $ \raw_value ->
    sendMsg odAttributeMap (mkSelector "setCustomTranslationFunction:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- customAttributes@
customAttributes :: IsODAttributeMap odAttributeMap => odAttributeMap -> IO (Id NSArray)
customAttributes odAttributeMap  =
  sendMsg odAttributeMap (mkSelector "customAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCustomAttributes:@
setCustomAttributes :: (IsODAttributeMap odAttributeMap, IsNSArray value) => odAttributeMap -> value -> IO ()
setCustomAttributes odAttributeMap  value =
withObjCPtr value $ \raw_value ->
    sendMsg odAttributeMap (mkSelector "setCustomAttributes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsODAttributeMap odAttributeMap => odAttributeMap -> IO (Id NSString)
value odAttributeMap  =
  sendMsg odAttributeMap (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValue:@
setValue :: (IsODAttributeMap odAttributeMap, IsNSString value) => odAttributeMap -> value -> IO ()
setValue odAttributeMap  value =
withObjCPtr value $ \raw_value ->
    sendMsg odAttributeMap (mkSelector "setValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributeMapWithValue:@
attributeMapWithValueSelector :: Selector
attributeMapWithValueSelector = mkSelector "attributeMapWithValue:"

-- | @Selector@ for @attributeMapWithStaticValue:@
attributeMapWithStaticValueSelector :: Selector
attributeMapWithStaticValueSelector = mkSelector "attributeMapWithStaticValue:"

-- | @Selector@ for @setStaticValue:@
setStaticValueSelector :: Selector
setStaticValueSelector = mkSelector "setStaticValue:"

-- | @Selector@ for @setVariableSubstitution:@
setVariableSubstitutionSelector :: Selector
setVariableSubstitutionSelector = mkSelector "setVariableSubstitution:"

-- | @Selector@ for @customQueryFunction@
customQueryFunctionSelector :: Selector
customQueryFunctionSelector = mkSelector "customQueryFunction"

-- | @Selector@ for @setCustomQueryFunction:@
setCustomQueryFunctionSelector :: Selector
setCustomQueryFunctionSelector = mkSelector "setCustomQueryFunction:"

-- | @Selector@ for @customTranslationFunction@
customTranslationFunctionSelector :: Selector
customTranslationFunctionSelector = mkSelector "customTranslationFunction"

-- | @Selector@ for @setCustomTranslationFunction:@
setCustomTranslationFunctionSelector :: Selector
setCustomTranslationFunctionSelector = mkSelector "setCustomTranslationFunction:"

-- | @Selector@ for @customAttributes@
customAttributesSelector :: Selector
customAttributesSelector = mkSelector "customAttributes"

-- | @Selector@ for @setCustomAttributes:@
setCustomAttributesSelector :: Selector
setCustomAttributesSelector = mkSelector "setCustomAttributes:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

