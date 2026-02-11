{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CMIOExtensionPropertyAttributes
--
-- A CMIOExtensionPropertyAttributes describes attributes of a property's value.
--
-- Generated bindings for @CMIOExtensionPropertyAttributes@.
module ObjC.CoreMediaIO.CMIOExtensionPropertyAttributes
  ( CMIOExtensionPropertyAttributes
  , IsCMIOExtensionPropertyAttributes(..)
  , init_
  , new
  , propertyAttributesWithMinValue_maxValue_validValues_readOnly
  , initWithMinValue_maxValue_validValues_readOnly
  , readOnlyPropertyAttribute
  , minValue
  , maxValue
  , validValues
  , readOnly
  , initSelector
  , newSelector
  , propertyAttributesWithMinValue_maxValue_validValues_readOnlySelector
  , initWithMinValue_maxValue_validValues_readOnlySelector
  , readOnlyPropertyAttributeSelector
  , minValueSelector
  , maxValueSelector
  , validValuesSelector
  , readOnlySelector


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

import ObjC.CoreMediaIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCMIOExtensionPropertyAttributes cmioExtensionPropertyAttributes => cmioExtensionPropertyAttributes -> IO (Id CMIOExtensionPropertyAttributes)
init_ cmioExtensionPropertyAttributes  =
  sendMsg cmioExtensionPropertyAttributes (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CMIOExtensionPropertyAttributes)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionPropertyAttributes"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | propertyAttributesWithMinValue:maxValue:validValues:readonly:
--
-- Returns a property attributes instance.
--
-- @minValue@ — The minimum value of the property.
--
-- @maxValue@ — The maximum value of the property.
--
-- @validValues@ — The array of valid values.
--
-- @readOnly@ — A readOnly flag.
--
-- Returns: A CMIOExtensionPropertyAttributes instance that describes the attributes of a property.
--
-- Property attributes may contain a minValue in which case the property is bounded by a minimum value. Property attributes may contain a maxValue in which case the property is bounded by a maximum value. Property attributes may contain both minValue and maxValue in which case the property is bounded within a range. Property attributes may contain a validValues in which case the property is discrete and can only have a certain set of value. If a property does not have a minValue/maxValue/validValues attributes, the property can have any value.
--
-- ObjC selector: @+ propertyAttributesWithMinValue:maxValue:validValues:readOnly:@
propertyAttributesWithMinValue_maxValue_validValues_readOnly :: IsNSArray validValues => RawId -> RawId -> validValues -> Bool -> IO (Id CMIOExtensionPropertyAttributes)
propertyAttributesWithMinValue_maxValue_validValues_readOnly minValue maxValue validValues readOnly =
  do
    cls' <- getRequiredClass "CMIOExtensionPropertyAttributes"
    withObjCPtr validValues $ \raw_validValues ->
      sendClassMsg cls' (mkSelector "propertyAttributesWithMinValue:maxValue:validValues:readOnly:") (retPtr retVoid) [argPtr (castPtr (unRawId minValue) :: Ptr ()), argPtr (castPtr (unRawId maxValue) :: Ptr ()), argPtr (castPtr raw_validValues :: Ptr ()), argCULong (if readOnly then 1 else 0)] >>= retainedObject . castPtr

-- | propertyAttributesWithMinValue:maxValue:validValues:readonly:
--
-- Initialize a property attributes instance.
--
-- @minValue@ — The minimum value of the property.
--
-- @maxValue@ — The maximum value of the property.
--
-- @validValues@ — The array of valid values.
--
-- @readOnly@ — A readOnly flag.
--
-- Returns: A CMIOExtensionPropertyAttributes instance that describes the attributes of a property.
--
-- Property attributes may contain a minValue in which case the property is bounded by a minimum value. Property attributes may contain a maxValue in which case the property is bounded by a maximum value. Property attributes may contain both minValue and maxValue in which case the property is bounded within a range. Property attributes may contain a validValues in which case the property is discrete and can only have a certain set of value. If a property does not have a minValue/maxValue/validValues attributes, the property can have any value.
--
-- ObjC selector: @- initWithMinValue:maxValue:validValues:readOnly:@
initWithMinValue_maxValue_validValues_readOnly :: (IsCMIOExtensionPropertyAttributes cmioExtensionPropertyAttributes, IsNSArray validValues) => cmioExtensionPropertyAttributes -> RawId -> RawId -> validValues -> Bool -> IO (Id CMIOExtensionPropertyAttributes)
initWithMinValue_maxValue_validValues_readOnly cmioExtensionPropertyAttributes  minValue maxValue validValues readOnly =
withObjCPtr validValues $ \raw_validValues ->
    sendMsg cmioExtensionPropertyAttributes (mkSelector "initWithMinValue:maxValue:validValues:readOnly:") (retPtr retVoid) [argPtr (castPtr (unRawId minValue) :: Ptr ()), argPtr (castPtr (unRawId maxValue) :: Ptr ()), argPtr (castPtr raw_validValues :: Ptr ()), argCULong (if readOnly then 1 else 0)] >>= ownedObject . castPtr

-- | readOnlyPropertyAttribute
--
-- The class property representing a readOnly property attribute with no minValue/maxValue/validValues.
--
-- ObjC selector: @+ readOnlyPropertyAttribute@
readOnlyPropertyAttribute :: IO (Id CMIOExtensionPropertyAttributes)
readOnlyPropertyAttribute  =
  do
    cls' <- getRequiredClass "CMIOExtensionPropertyAttributes"
    sendClassMsg cls' (mkSelector "readOnlyPropertyAttribute") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | minValue
--
-- The minimum value of a property.
--
-- ObjC selector: @- minValue@
minValue :: IsCMIOExtensionPropertyAttributes cmioExtensionPropertyAttributes => cmioExtensionPropertyAttributes -> IO RawId
minValue cmioExtensionPropertyAttributes  =
  fmap (RawId . castPtr) $ sendMsg cmioExtensionPropertyAttributes (mkSelector "minValue") (retPtr retVoid) []

-- | maxValue
--
-- The maximum value of a property.
--
-- ObjC selector: @- maxValue@
maxValue :: IsCMIOExtensionPropertyAttributes cmioExtensionPropertyAttributes => cmioExtensionPropertyAttributes -> IO RawId
maxValue cmioExtensionPropertyAttributes  =
  fmap (RawId . castPtr) $ sendMsg cmioExtensionPropertyAttributes (mkSelector "maxValue") (retPtr retVoid) []

-- | validValues
--
-- An array of valid values.
--
-- ObjC selector: @- validValues@
validValues :: IsCMIOExtensionPropertyAttributes cmioExtensionPropertyAttributes => cmioExtensionPropertyAttributes -> IO (Id NSArray)
validValues cmioExtensionPropertyAttributes  =
  sendMsg cmioExtensionPropertyAttributes (mkSelector "validValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | readOnly
--
-- The readOnly flag attribute.
--
-- ObjC selector: @- readOnly@
readOnly :: IsCMIOExtensionPropertyAttributes cmioExtensionPropertyAttributes => cmioExtensionPropertyAttributes -> IO Bool
readOnly cmioExtensionPropertyAttributes  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cmioExtensionPropertyAttributes (mkSelector "readOnly") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @propertyAttributesWithMinValue:maxValue:validValues:readOnly:@
propertyAttributesWithMinValue_maxValue_validValues_readOnlySelector :: Selector
propertyAttributesWithMinValue_maxValue_validValues_readOnlySelector = mkSelector "propertyAttributesWithMinValue:maxValue:validValues:readOnly:"

-- | @Selector@ for @initWithMinValue:maxValue:validValues:readOnly:@
initWithMinValue_maxValue_validValues_readOnlySelector :: Selector
initWithMinValue_maxValue_validValues_readOnlySelector = mkSelector "initWithMinValue:maxValue:validValues:readOnly:"

-- | @Selector@ for @readOnlyPropertyAttribute@
readOnlyPropertyAttributeSelector :: Selector
readOnlyPropertyAttributeSelector = mkSelector "readOnlyPropertyAttribute"

-- | @Selector@ for @minValue@
minValueSelector :: Selector
minValueSelector = mkSelector "minValue"

-- | @Selector@ for @maxValue@
maxValueSelector :: Selector
maxValueSelector = mkSelector "maxValue"

-- | @Selector@ for @validValues@
validValuesSelector :: Selector
validValuesSelector = mkSelector "validValues"

-- | @Selector@ for @readOnly@
readOnlySelector :: Selector
readOnlySelector = mkSelector "readOnly"

