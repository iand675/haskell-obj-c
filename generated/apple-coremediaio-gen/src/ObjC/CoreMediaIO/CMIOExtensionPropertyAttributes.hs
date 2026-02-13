{-# LANGUAGE DataKinds #-}
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
  , initWithMinValue_maxValue_validValues_readOnlySelector
  , maxValueSelector
  , minValueSelector
  , newSelector
  , propertyAttributesWithMinValue_maxValue_validValues_readOnlySelector
  , readOnlyPropertyAttributeSelector
  , readOnlySelector
  , validValuesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMediaIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCMIOExtensionPropertyAttributes cmioExtensionPropertyAttributes => cmioExtensionPropertyAttributes -> IO (Id CMIOExtensionPropertyAttributes)
init_ cmioExtensionPropertyAttributes =
  sendOwnedMessage cmioExtensionPropertyAttributes initSelector

-- | @+ new@
new :: IO (Id CMIOExtensionPropertyAttributes)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionPropertyAttributes"
    sendOwnedClassMessage cls' newSelector

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
    sendClassMessage cls' propertyAttributesWithMinValue_maxValue_validValues_readOnlySelector minValue maxValue (toNSArray validValues) readOnly

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
initWithMinValue_maxValue_validValues_readOnly cmioExtensionPropertyAttributes minValue maxValue validValues readOnly =
  sendOwnedMessage cmioExtensionPropertyAttributes initWithMinValue_maxValue_validValues_readOnlySelector minValue maxValue (toNSArray validValues) readOnly

-- | readOnlyPropertyAttribute
--
-- The class property representing a readOnly property attribute with no minValue/maxValue/validValues.
--
-- ObjC selector: @+ readOnlyPropertyAttribute@
readOnlyPropertyAttribute :: IO (Id CMIOExtensionPropertyAttributes)
readOnlyPropertyAttribute  =
  do
    cls' <- getRequiredClass "CMIOExtensionPropertyAttributes"
    sendClassMessage cls' readOnlyPropertyAttributeSelector

-- | minValue
--
-- The minimum value of a property.
--
-- ObjC selector: @- minValue@
minValue :: IsCMIOExtensionPropertyAttributes cmioExtensionPropertyAttributes => cmioExtensionPropertyAttributes -> IO RawId
minValue cmioExtensionPropertyAttributes =
  sendMessage cmioExtensionPropertyAttributes minValueSelector

-- | maxValue
--
-- The maximum value of a property.
--
-- ObjC selector: @- maxValue@
maxValue :: IsCMIOExtensionPropertyAttributes cmioExtensionPropertyAttributes => cmioExtensionPropertyAttributes -> IO RawId
maxValue cmioExtensionPropertyAttributes =
  sendMessage cmioExtensionPropertyAttributes maxValueSelector

-- | validValues
--
-- An array of valid values.
--
-- ObjC selector: @- validValues@
validValues :: IsCMIOExtensionPropertyAttributes cmioExtensionPropertyAttributes => cmioExtensionPropertyAttributes -> IO (Id NSArray)
validValues cmioExtensionPropertyAttributes =
  sendMessage cmioExtensionPropertyAttributes validValuesSelector

-- | readOnly
--
-- The readOnly flag attribute.
--
-- ObjC selector: @- readOnly@
readOnly :: IsCMIOExtensionPropertyAttributes cmioExtensionPropertyAttributes => cmioExtensionPropertyAttributes -> IO Bool
readOnly cmioExtensionPropertyAttributes =
  sendMessage cmioExtensionPropertyAttributes readOnlySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CMIOExtensionPropertyAttributes)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CMIOExtensionPropertyAttributes)
newSelector = mkSelector "new"

-- | @Selector@ for @propertyAttributesWithMinValue:maxValue:validValues:readOnly:@
propertyAttributesWithMinValue_maxValue_validValues_readOnlySelector :: Selector '[RawId, RawId, Id NSArray, Bool] (Id CMIOExtensionPropertyAttributes)
propertyAttributesWithMinValue_maxValue_validValues_readOnlySelector = mkSelector "propertyAttributesWithMinValue:maxValue:validValues:readOnly:"

-- | @Selector@ for @initWithMinValue:maxValue:validValues:readOnly:@
initWithMinValue_maxValue_validValues_readOnlySelector :: Selector '[RawId, RawId, Id NSArray, Bool] (Id CMIOExtensionPropertyAttributes)
initWithMinValue_maxValue_validValues_readOnlySelector = mkSelector "initWithMinValue:maxValue:validValues:readOnly:"

-- | @Selector@ for @readOnlyPropertyAttribute@
readOnlyPropertyAttributeSelector :: Selector '[] (Id CMIOExtensionPropertyAttributes)
readOnlyPropertyAttributeSelector = mkSelector "readOnlyPropertyAttribute"

-- | @Selector@ for @minValue@
minValueSelector :: Selector '[] RawId
minValueSelector = mkSelector "minValue"

-- | @Selector@ for @maxValue@
maxValueSelector :: Selector '[] RawId
maxValueSelector = mkSelector "maxValue"

-- | @Selector@ for @validValues@
validValuesSelector :: Selector '[] (Id NSArray)
validValuesSelector = mkSelector "validValues"

-- | @Selector@ for @readOnly@
readOnlySelector :: Selector '[] Bool
readOnlySelector = mkSelector "readOnly"

