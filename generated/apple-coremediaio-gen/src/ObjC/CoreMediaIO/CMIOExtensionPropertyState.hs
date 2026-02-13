{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CMIOExtensionPropertyState
--
-- A CMIOExtensionPropertyState describes a property state.
--
-- Generated bindings for @CMIOExtensionPropertyState@.
module ObjC.CoreMediaIO.CMIOExtensionPropertyState
  ( CMIOExtensionPropertyState
  , IsCMIOExtensionPropertyState(..)
  , init_
  , new
  , propertyStateWithValue
  , propertyStateWithValue_attributes
  , initWithValue
  , initWithValue_attributes
  , value
  , attributes
  , attributesSelector
  , initSelector
  , initWithValueSelector
  , initWithValue_attributesSelector
  , newSelector
  , propertyStateWithValueSelector
  , propertyStateWithValue_attributesSelector
  , valueSelector


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
init_ :: IsCMIOExtensionPropertyState cmioExtensionPropertyState => cmioExtensionPropertyState -> IO (Id CMIOExtensionPropertyState)
init_ cmioExtensionPropertyState =
  sendOwnedMessage cmioExtensionPropertyState initSelector

-- | @+ new@
new :: IO (Id CMIOExtensionPropertyState)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionPropertyState"
    sendOwnedClassMessage cls' newSelector

-- | propertyStateWithValue:
--
-- Returns a property state instance.
--
-- @value@ — The value associated with a property state.
--
-- Returns: A CMIOExtensionPropertyState instance with a nil attributes.
--
-- ObjC selector: @+ propertyStateWithValue:@
propertyStateWithValue :: RawId -> IO (Id CMIOExtensionPropertyState)
propertyStateWithValue value =
  do
    cls' <- getRequiredClass "CMIOExtensionPropertyState"
    sendClassMessage cls' propertyStateWithValueSelector value

-- | propertyStateWithValue:attributes:
--
-- Returns a property state instance.
--
-- @value@ — The value associated with a property state.
--
-- @attributes@ — The property attributes associated with a property state.
--
-- Returns: A CMIOExtensionPropertyState instance.
--
-- A nil property attributes defaults to a read/write property that doesn't have a minValue/maxValue/validValues. The supported value types are NSDictionary/NSArray/NSString/NSData/NSNumber.
--
-- ObjC selector: @+ propertyStateWithValue:attributes:@
propertyStateWithValue_attributes :: IsCMIOExtensionPropertyAttributes attributes => RawId -> attributes -> IO (Id CMIOExtensionPropertyState)
propertyStateWithValue_attributes value attributes =
  do
    cls' <- getRequiredClass "CMIOExtensionPropertyState"
    sendClassMessage cls' propertyStateWithValue_attributesSelector value (toCMIOExtensionPropertyAttributes attributes)

-- | initWithValue:
--
-- Initialize a property state instance.
--
-- @value@ — The value associated with a property state.
--
-- Returns: A CMIOExtensionPropertyState instance.
--
-- A nil property attributes defaults to a read/write property that doesn't have a minValue/maxValue/validValues. The supported value types are NSDictionary/NSArray/NSString/NSData/NSNumber.
--
-- ObjC selector: @- initWithValue:@
initWithValue :: IsCMIOExtensionPropertyState cmioExtensionPropertyState => cmioExtensionPropertyState -> RawId -> IO (Id CMIOExtensionPropertyState)
initWithValue cmioExtensionPropertyState value =
  sendOwnedMessage cmioExtensionPropertyState initWithValueSelector value

-- | initWithValue:attributes:
--
-- Initialize a property state instance.
--
-- @value@ — The value associated with a property state.
--
-- @attributes@ — The property attributes associated with a property state.
--
-- Returns: A CMIOExtensionPropertyState instance.
--
-- A nil property attributes defaults to a read/write property that doesn't have a minValue/maxValue/validValues. The supported value types are NSDictionary/NSArray/NSString/NSData/NSNumber.
--
-- ObjC selector: @- initWithValue:attributes:@
initWithValue_attributes :: (IsCMIOExtensionPropertyState cmioExtensionPropertyState, IsCMIOExtensionPropertyAttributes attributes) => cmioExtensionPropertyState -> RawId -> attributes -> IO (Id CMIOExtensionPropertyState)
initWithValue_attributes cmioExtensionPropertyState value attributes =
  sendOwnedMessage cmioExtensionPropertyState initWithValue_attributesSelector value (toCMIOExtensionPropertyAttributes attributes)

-- | value
--
-- The value of the property.
--
-- ObjC selector: @- value@
value :: IsCMIOExtensionPropertyState cmioExtensionPropertyState => cmioExtensionPropertyState -> IO RawId
value cmioExtensionPropertyState =
  sendMessage cmioExtensionPropertyState valueSelector

-- | attributes
--
-- The property attributes of the property.
--
-- ObjC selector: @- attributes@
attributes :: IsCMIOExtensionPropertyState cmioExtensionPropertyState => cmioExtensionPropertyState -> IO (Id CMIOExtensionPropertyAttributes)
attributes cmioExtensionPropertyState =
  sendMessage cmioExtensionPropertyState attributesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CMIOExtensionPropertyState)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CMIOExtensionPropertyState)
newSelector = mkSelector "new"

-- | @Selector@ for @propertyStateWithValue:@
propertyStateWithValueSelector :: Selector '[RawId] (Id CMIOExtensionPropertyState)
propertyStateWithValueSelector = mkSelector "propertyStateWithValue:"

-- | @Selector@ for @propertyStateWithValue:attributes:@
propertyStateWithValue_attributesSelector :: Selector '[RawId, Id CMIOExtensionPropertyAttributes] (Id CMIOExtensionPropertyState)
propertyStateWithValue_attributesSelector = mkSelector "propertyStateWithValue:attributes:"

-- | @Selector@ for @initWithValue:@
initWithValueSelector :: Selector '[RawId] (Id CMIOExtensionPropertyState)
initWithValueSelector = mkSelector "initWithValue:"

-- | @Selector@ for @initWithValue:attributes:@
initWithValue_attributesSelector :: Selector '[RawId, Id CMIOExtensionPropertyAttributes] (Id CMIOExtensionPropertyState)
initWithValue_attributesSelector = mkSelector "initWithValue:attributes:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] RawId
valueSelector = mkSelector "value"

-- | @Selector@ for @attributes@
attributesSelector :: Selector '[] (Id CMIOExtensionPropertyAttributes)
attributesSelector = mkSelector "attributes"

