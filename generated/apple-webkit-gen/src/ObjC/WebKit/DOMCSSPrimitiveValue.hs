{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMCSSPrimitiveValue@.
module ObjC.WebKit.DOMCSSPrimitiveValue
  ( DOMCSSPrimitiveValue
  , IsDOMCSSPrimitiveValue(..)
  , setFloatValue_floatValue
  , getFloatValue
  , setStringValue_stringValue
  , getStringValue
  , getCounterValue
  , getRectValue
  , getRGBColorValue
  , setFloatValue
  , setStringValue
  , primitiveType
  , getCounterValueSelector
  , getFloatValueSelector
  , getRGBColorValueSelector
  , getRectValueSelector
  , getStringValueSelector
  , primitiveTypeSelector
  , setFloatValueSelector
  , setFloatValue_floatValueSelector
  , setStringValueSelector
  , setStringValue_stringValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- setFloatValue:floatValue:@
setFloatValue_floatValue :: IsDOMCSSPrimitiveValue domcssPrimitiveValue => domcssPrimitiveValue -> CUShort -> CFloat -> IO ()
setFloatValue_floatValue domcssPrimitiveValue unitType floatValue =
  sendMessage domcssPrimitiveValue setFloatValue_floatValueSelector unitType floatValue

-- | @- getFloatValue:@
getFloatValue :: IsDOMCSSPrimitiveValue domcssPrimitiveValue => domcssPrimitiveValue -> CUShort -> IO CFloat
getFloatValue domcssPrimitiveValue unitType =
  sendMessage domcssPrimitiveValue getFloatValueSelector unitType

-- | @- setStringValue:stringValue:@
setStringValue_stringValue :: (IsDOMCSSPrimitiveValue domcssPrimitiveValue, IsNSString stringValue) => domcssPrimitiveValue -> CUShort -> stringValue -> IO ()
setStringValue_stringValue domcssPrimitiveValue stringType stringValue =
  sendMessage domcssPrimitiveValue setStringValue_stringValueSelector stringType (toNSString stringValue)

-- | @- getStringValue@
getStringValue :: IsDOMCSSPrimitiveValue domcssPrimitiveValue => domcssPrimitiveValue -> IO (Id NSString)
getStringValue domcssPrimitiveValue =
  sendMessage domcssPrimitiveValue getStringValueSelector

-- | @- getCounterValue@
getCounterValue :: IsDOMCSSPrimitiveValue domcssPrimitiveValue => domcssPrimitiveValue -> IO (Id DOMCounter)
getCounterValue domcssPrimitiveValue =
  sendMessage domcssPrimitiveValue getCounterValueSelector

-- | @- getRectValue@
getRectValue :: IsDOMCSSPrimitiveValue domcssPrimitiveValue => domcssPrimitiveValue -> IO (Id DOMRect)
getRectValue domcssPrimitiveValue =
  sendMessage domcssPrimitiveValue getRectValueSelector

-- | @- getRGBColorValue@
getRGBColorValue :: IsDOMCSSPrimitiveValue domcssPrimitiveValue => domcssPrimitiveValue -> IO (Id DOMRGBColor)
getRGBColorValue domcssPrimitiveValue =
  sendMessage domcssPrimitiveValue getRGBColorValueSelector

-- | @- setFloatValue::@
setFloatValue :: IsDOMCSSPrimitiveValue domcssPrimitiveValue => domcssPrimitiveValue -> CUShort -> CFloat -> IO ()
setFloatValue domcssPrimitiveValue unitType floatValue =
  sendMessage domcssPrimitiveValue setFloatValueSelector unitType floatValue

-- | @- setStringValue::@
setStringValue :: (IsDOMCSSPrimitiveValue domcssPrimitiveValue, IsNSString stringValue) => domcssPrimitiveValue -> CUShort -> stringValue -> IO ()
setStringValue domcssPrimitiveValue stringType stringValue =
  sendMessage domcssPrimitiveValue setStringValueSelector stringType (toNSString stringValue)

-- | @- primitiveType@
primitiveType :: IsDOMCSSPrimitiveValue domcssPrimitiveValue => domcssPrimitiveValue -> IO CUShort
primitiveType domcssPrimitiveValue =
  sendMessage domcssPrimitiveValue primitiveTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setFloatValue:floatValue:@
setFloatValue_floatValueSelector :: Selector '[CUShort, CFloat] ()
setFloatValue_floatValueSelector = mkSelector "setFloatValue:floatValue:"

-- | @Selector@ for @getFloatValue:@
getFloatValueSelector :: Selector '[CUShort] CFloat
getFloatValueSelector = mkSelector "getFloatValue:"

-- | @Selector@ for @setStringValue:stringValue:@
setStringValue_stringValueSelector :: Selector '[CUShort, Id NSString] ()
setStringValue_stringValueSelector = mkSelector "setStringValue:stringValue:"

-- | @Selector@ for @getStringValue@
getStringValueSelector :: Selector '[] (Id NSString)
getStringValueSelector = mkSelector "getStringValue"

-- | @Selector@ for @getCounterValue@
getCounterValueSelector :: Selector '[] (Id DOMCounter)
getCounterValueSelector = mkSelector "getCounterValue"

-- | @Selector@ for @getRectValue@
getRectValueSelector :: Selector '[] (Id DOMRect)
getRectValueSelector = mkSelector "getRectValue"

-- | @Selector@ for @getRGBColorValue@
getRGBColorValueSelector :: Selector '[] (Id DOMRGBColor)
getRGBColorValueSelector = mkSelector "getRGBColorValue"

-- | @Selector@ for @setFloatValue::@
setFloatValueSelector :: Selector '[CUShort, CFloat] ()
setFloatValueSelector = mkSelector "setFloatValue::"

-- | @Selector@ for @setStringValue::@
setStringValueSelector :: Selector '[CUShort, Id NSString] ()
setStringValueSelector = mkSelector "setStringValue::"

-- | @Selector@ for @primitiveType@
primitiveTypeSelector :: Selector '[] CUShort
primitiveTypeSelector = mkSelector "primitiveType"

