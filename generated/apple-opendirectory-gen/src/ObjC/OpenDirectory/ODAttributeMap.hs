{-# LANGUAGE DataKinds #-}
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
  , attributeMapWithStaticValueSelector
  , attributeMapWithValueSelector
  , customAttributesSelector
  , customQueryFunctionSelector
  , customTranslationFunctionSelector
  , setCustomAttributesSelector
  , setCustomQueryFunctionSelector
  , setCustomTranslationFunctionSelector
  , setStaticValueSelector
  , setValueSelector
  , setVariableSubstitutionSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' attributeMapWithValueSelector (toNSString value)

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
    sendClassMessage cls' attributeMapWithStaticValueSelector (toNSString staticValue)

-- | setStaticValue:
--
-- Sets a static value that will always be returned for this mapping.
--
-- Sets a static value that will always be returned for this mapping, i.e., "20".
--
-- ObjC selector: @- setStaticValue:@
setStaticValue :: (IsODAttributeMap odAttributeMap, IsNSString staticValue) => odAttributeMap -> staticValue -> IO ()
setStaticValue odAttributeMap staticValue =
  sendMessage odAttributeMap setStaticValueSelector (toNSString staticValue)

-- | setVariableSubstitution:
--
-- Sets a variable substitution-based value.
--
-- Value should be using the syntax '$native$' for all substited values.  For example, to form a home directory using the "cn" of an LDAP record, substitution could be done with "/home/$cn$".
--
-- ObjC selector: @- setVariableSubstitution:@
setVariableSubstitution :: (IsODAttributeMap odAttributeMap, IsNSString variableSubstitution) => odAttributeMap -> variableSubstitution -> IO ()
setVariableSubstitution odAttributeMap variableSubstitution =
  sendMessage odAttributeMap setVariableSubstitutionSelector (toNSString variableSubstitution)

-- | @- customQueryFunction@
customQueryFunction :: IsODAttributeMap odAttributeMap => odAttributeMap -> IO (Id NSString)
customQueryFunction odAttributeMap =
  sendMessage odAttributeMap customQueryFunctionSelector

-- | @- setCustomQueryFunction:@
setCustomQueryFunction :: (IsODAttributeMap odAttributeMap, IsNSString value) => odAttributeMap -> value -> IO ()
setCustomQueryFunction odAttributeMap value =
  sendMessage odAttributeMap setCustomQueryFunctionSelector (toNSString value)

-- | @- customTranslationFunction@
customTranslationFunction :: IsODAttributeMap odAttributeMap => odAttributeMap -> IO (Id NSString)
customTranslationFunction odAttributeMap =
  sendMessage odAttributeMap customTranslationFunctionSelector

-- | @- setCustomTranslationFunction:@
setCustomTranslationFunction :: (IsODAttributeMap odAttributeMap, IsNSString value) => odAttributeMap -> value -> IO ()
setCustomTranslationFunction odAttributeMap value =
  sendMessage odAttributeMap setCustomTranslationFunctionSelector (toNSString value)

-- | @- customAttributes@
customAttributes :: IsODAttributeMap odAttributeMap => odAttributeMap -> IO (Id NSArray)
customAttributes odAttributeMap =
  sendMessage odAttributeMap customAttributesSelector

-- | @- setCustomAttributes:@
setCustomAttributes :: (IsODAttributeMap odAttributeMap, IsNSArray value) => odAttributeMap -> value -> IO ()
setCustomAttributes odAttributeMap value =
  sendMessage odAttributeMap setCustomAttributesSelector (toNSArray value)

-- | @- value@
value :: IsODAttributeMap odAttributeMap => odAttributeMap -> IO (Id NSString)
value odAttributeMap =
  sendMessage odAttributeMap valueSelector

-- | @- setValue:@
setValue :: (IsODAttributeMap odAttributeMap, IsNSString value) => odAttributeMap -> value -> IO ()
setValue odAttributeMap value =
  sendMessage odAttributeMap setValueSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributeMapWithValue:@
attributeMapWithValueSelector :: Selector '[Id NSString] (Id ODAttributeMap)
attributeMapWithValueSelector = mkSelector "attributeMapWithValue:"

-- | @Selector@ for @attributeMapWithStaticValue:@
attributeMapWithStaticValueSelector :: Selector '[Id NSString] (Id ODAttributeMap)
attributeMapWithStaticValueSelector = mkSelector "attributeMapWithStaticValue:"

-- | @Selector@ for @setStaticValue:@
setStaticValueSelector :: Selector '[Id NSString] ()
setStaticValueSelector = mkSelector "setStaticValue:"

-- | @Selector@ for @setVariableSubstitution:@
setVariableSubstitutionSelector :: Selector '[Id NSString] ()
setVariableSubstitutionSelector = mkSelector "setVariableSubstitution:"

-- | @Selector@ for @customQueryFunction@
customQueryFunctionSelector :: Selector '[] (Id NSString)
customQueryFunctionSelector = mkSelector "customQueryFunction"

-- | @Selector@ for @setCustomQueryFunction:@
setCustomQueryFunctionSelector :: Selector '[Id NSString] ()
setCustomQueryFunctionSelector = mkSelector "setCustomQueryFunction:"

-- | @Selector@ for @customTranslationFunction@
customTranslationFunctionSelector :: Selector '[] (Id NSString)
customTranslationFunctionSelector = mkSelector "customTranslationFunction"

-- | @Selector@ for @setCustomTranslationFunction:@
setCustomTranslationFunctionSelector :: Selector '[Id NSString] ()
setCustomTranslationFunctionSelector = mkSelector "setCustomTranslationFunction:"

-- | @Selector@ for @customAttributes@
customAttributesSelector :: Selector '[] (Id NSArray)
customAttributesSelector = mkSelector "customAttributes"

-- | @Selector@ for @setCustomAttributes:@
setCustomAttributesSelector :: Selector '[Id NSArray] ()
setCustomAttributesSelector = mkSelector "setCustomAttributes:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSString)
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSString] ()
setValueSelector = mkSelector "setValue:"

