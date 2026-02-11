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
  , initSelector
  , newSelector
  , propertyStateWithValueSelector
  , propertyStateWithValue_attributesSelector
  , initWithValueSelector
  , initWithValue_attributesSelector
  , valueSelector
  , attributesSelector


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
init_ :: IsCMIOExtensionPropertyState cmioExtensionPropertyState => cmioExtensionPropertyState -> IO (Id CMIOExtensionPropertyState)
init_ cmioExtensionPropertyState  =
  sendMsg cmioExtensionPropertyState (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CMIOExtensionPropertyState)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionPropertyState"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    sendClassMsg cls' (mkSelector "propertyStateWithValue:") (retPtr retVoid) [argPtr (castPtr (unRawId value) :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr attributes $ \raw_attributes ->
      sendClassMsg cls' (mkSelector "propertyStateWithValue:attributes:") (retPtr retVoid) [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_attributes :: Ptr ())] >>= retainedObject . castPtr

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
initWithValue cmioExtensionPropertyState  value =
  sendMsg cmioExtensionPropertyState (mkSelector "initWithValue:") (retPtr retVoid) [argPtr (castPtr (unRawId value) :: Ptr ())] >>= ownedObject . castPtr

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
initWithValue_attributes cmioExtensionPropertyState  value attributes =
withObjCPtr attributes $ \raw_attributes ->
    sendMsg cmioExtensionPropertyState (mkSelector "initWithValue:attributes:") (retPtr retVoid) [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_attributes :: Ptr ())] >>= ownedObject . castPtr

-- | value
--
-- The value of the property.
--
-- ObjC selector: @- value@
value :: IsCMIOExtensionPropertyState cmioExtensionPropertyState => cmioExtensionPropertyState -> IO RawId
value cmioExtensionPropertyState  =
  fmap (RawId . castPtr) $ sendMsg cmioExtensionPropertyState (mkSelector "value") (retPtr retVoid) []

-- | attributes
--
-- The property attributes of the property.
--
-- ObjC selector: @- attributes@
attributes :: IsCMIOExtensionPropertyState cmioExtensionPropertyState => cmioExtensionPropertyState -> IO (Id CMIOExtensionPropertyAttributes)
attributes cmioExtensionPropertyState  =
  sendMsg cmioExtensionPropertyState (mkSelector "attributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @propertyStateWithValue:@
propertyStateWithValueSelector :: Selector
propertyStateWithValueSelector = mkSelector "propertyStateWithValue:"

-- | @Selector@ for @propertyStateWithValue:attributes:@
propertyStateWithValue_attributesSelector :: Selector
propertyStateWithValue_attributesSelector = mkSelector "propertyStateWithValue:attributes:"

-- | @Selector@ for @initWithValue:@
initWithValueSelector :: Selector
initWithValueSelector = mkSelector "initWithValue:"

-- | @Selector@ for @initWithValue:attributes:@
initWithValue_attributesSelector :: Selector
initWithValue_attributesSelector = mkSelector "initWithValue:attributes:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @attributes@
attributesSelector :: Selector
attributesSelector = mkSelector "attributes"

