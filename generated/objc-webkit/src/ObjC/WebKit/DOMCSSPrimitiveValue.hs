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
  , setFloatValue_floatValueSelector
  , getFloatValueSelector
  , setStringValue_stringValueSelector
  , getStringValueSelector
  , getCounterValueSelector
  , getRectValueSelector
  , getRGBColorValueSelector
  , setFloatValueSelector
  , setStringValueSelector
  , primitiveTypeSelector


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

-- | @- setFloatValue:floatValue:@
setFloatValue_floatValue :: IsDOMCSSPrimitiveValue domcssPrimitiveValue => domcssPrimitiveValue -> CUShort -> CFloat -> IO ()
setFloatValue_floatValue domcssPrimitiveValue  unitType floatValue =
  sendMsg domcssPrimitiveValue (mkSelector "setFloatValue:floatValue:") retVoid [argCUInt (fromIntegral unitType), argCFloat (fromIntegral floatValue)]

-- | @- getFloatValue:@
getFloatValue :: IsDOMCSSPrimitiveValue domcssPrimitiveValue => domcssPrimitiveValue -> CUShort -> IO CFloat
getFloatValue domcssPrimitiveValue  unitType =
  sendMsg domcssPrimitiveValue (mkSelector "getFloatValue:") retCFloat [argCUInt (fromIntegral unitType)]

-- | @- setStringValue:stringValue:@
setStringValue_stringValue :: (IsDOMCSSPrimitiveValue domcssPrimitiveValue, IsNSString stringValue) => domcssPrimitiveValue -> CUShort -> stringValue -> IO ()
setStringValue_stringValue domcssPrimitiveValue  stringType stringValue =
withObjCPtr stringValue $ \raw_stringValue ->
    sendMsg domcssPrimitiveValue (mkSelector "setStringValue:stringValue:") retVoid [argCUInt (fromIntegral stringType), argPtr (castPtr raw_stringValue :: Ptr ())]

-- | @- getStringValue@
getStringValue :: IsDOMCSSPrimitiveValue domcssPrimitiveValue => domcssPrimitiveValue -> IO (Id NSString)
getStringValue domcssPrimitiveValue  =
  sendMsg domcssPrimitiveValue (mkSelector "getStringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- getCounterValue@
getCounterValue :: IsDOMCSSPrimitiveValue domcssPrimitiveValue => domcssPrimitiveValue -> IO (Id DOMCounter)
getCounterValue domcssPrimitiveValue  =
  sendMsg domcssPrimitiveValue (mkSelector "getCounterValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- getRectValue@
getRectValue :: IsDOMCSSPrimitiveValue domcssPrimitiveValue => domcssPrimitiveValue -> IO (Id DOMRect)
getRectValue domcssPrimitiveValue  =
  sendMsg domcssPrimitiveValue (mkSelector "getRectValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- getRGBColorValue@
getRGBColorValue :: IsDOMCSSPrimitiveValue domcssPrimitiveValue => domcssPrimitiveValue -> IO (Id DOMRGBColor)
getRGBColorValue domcssPrimitiveValue  =
  sendMsg domcssPrimitiveValue (mkSelector "getRGBColorValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFloatValue::@
setFloatValue :: IsDOMCSSPrimitiveValue domcssPrimitiveValue => domcssPrimitiveValue -> CUShort -> CFloat -> IO ()
setFloatValue domcssPrimitiveValue  unitType floatValue =
  sendMsg domcssPrimitiveValue (mkSelector "setFloatValue::") retVoid [argCUInt (fromIntegral unitType), argCFloat (fromIntegral floatValue)]

-- | @- setStringValue::@
setStringValue :: (IsDOMCSSPrimitiveValue domcssPrimitiveValue, IsNSString stringValue) => domcssPrimitiveValue -> CUShort -> stringValue -> IO ()
setStringValue domcssPrimitiveValue  stringType stringValue =
withObjCPtr stringValue $ \raw_stringValue ->
    sendMsg domcssPrimitiveValue (mkSelector "setStringValue::") retVoid [argCUInt (fromIntegral stringType), argPtr (castPtr raw_stringValue :: Ptr ())]

-- | @- primitiveType@
primitiveType :: IsDOMCSSPrimitiveValue domcssPrimitiveValue => domcssPrimitiveValue -> IO CUShort
primitiveType domcssPrimitiveValue  =
  fmap fromIntegral $ sendMsg domcssPrimitiveValue (mkSelector "primitiveType") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setFloatValue:floatValue:@
setFloatValue_floatValueSelector :: Selector
setFloatValue_floatValueSelector = mkSelector "setFloatValue:floatValue:"

-- | @Selector@ for @getFloatValue:@
getFloatValueSelector :: Selector
getFloatValueSelector = mkSelector "getFloatValue:"

-- | @Selector@ for @setStringValue:stringValue:@
setStringValue_stringValueSelector :: Selector
setStringValue_stringValueSelector = mkSelector "setStringValue:stringValue:"

-- | @Selector@ for @getStringValue@
getStringValueSelector :: Selector
getStringValueSelector = mkSelector "getStringValue"

-- | @Selector@ for @getCounterValue@
getCounterValueSelector :: Selector
getCounterValueSelector = mkSelector "getCounterValue"

-- | @Selector@ for @getRectValue@
getRectValueSelector :: Selector
getRectValueSelector = mkSelector "getRectValue"

-- | @Selector@ for @getRGBColorValue@
getRGBColorValueSelector :: Selector
getRGBColorValueSelector = mkSelector "getRGBColorValue"

-- | @Selector@ for @setFloatValue::@
setFloatValueSelector :: Selector
setFloatValueSelector = mkSelector "setFloatValue::"

-- | @Selector@ for @setStringValue::@
setStringValueSelector :: Selector
setStringValueSelector = mkSelector "setStringValue::"

-- | @Selector@ for @primitiveType@
primitiveTypeSelector :: Selector
primitiveTypeSelector = mkSelector "primitiveType"

