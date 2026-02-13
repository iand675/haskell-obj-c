{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOBluetoothSDPServiceAttribute
--
-- IOBluetoothSDPServiceAttribute represents a single SDP service attribute.
--
-- A service attribute contains two components: an attribute ID and a data element.
--
-- Generated bindings for @IOBluetoothSDPServiceAttribute@.
module ObjC.IOBluetooth.IOBluetoothSDPServiceAttribute
  ( IOBluetoothSDPServiceAttribute
  , IsIOBluetoothSDPServiceAttribute(..)
  , withID_attributeElementValue
  , withID_attributeElement
  , initWithID_attributeElementValue
  , initWithID_attributeElement
  , getAttributeID
  , getDataElement
  , getIDDataElement
  , getAttributeIDSelector
  , getDataElementSelector
  , getIDDataElementSelector
  , initWithID_attributeElementSelector
  , initWithID_attributeElementValueSelector
  , withID_attributeElementSelector
  , withID_attributeElementValueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IOBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | withID:attributeElementValue:
--
-- Creates a new service attribute with the given ID and element value.
--
-- See +[IOBluetoothSDPDataElement withElementValue:] for a description of the types that                 may be passed in as the attributeElementValue.
--
-- @attributeID@ — The attribute ID of the new service attribute.
--
-- @attributeElementValue@ — The data element value of the new service attribute
--
-- Returns: Returns the newly allocated service attribute object.  Returns nil if there was an error parsing the 				element value.  The returned IOBluetoothSDPDataElement object has been autoreleased, so it is not             necessary for the caller to release it.  If the object is to be referenced and kept around, retain             should be called.
--
-- ObjC selector: @+ withID:attributeElementValue:@
withID_attributeElementValue :: IsNSObject attributeElementValue => CUShort -> attributeElementValue -> IO (Id IOBluetoothSDPServiceAttribute)
withID_attributeElementValue newAttributeID attributeElementValue =
  do
    cls' <- getRequiredClass "IOBluetoothSDPServiceAttribute"
    sendClassMessage cls' withID_attributeElementValueSelector newAttributeID (toNSObject attributeElementValue)

-- | withID:attributeElement:
--
-- Creates a new service attribute with the given ID and data element.
--
-- @attributeID@ — The attribute ID of the new service attribute.
--
-- @attributeElement@ — The data element of the new service attribute.
--
-- Returns: Returns the newly allocated service attribute object.  Returns nil if there was an error.              The returned IOBluetoothSDPDataElement object has been autoreleased, so it is not             necessary for the caller to release it.  If the object is to be referenced and kept around, retain             should be called.
--
-- ObjC selector: @+ withID:attributeElement:@
withID_attributeElement :: IsIOBluetoothSDPDataElement attributeElement => CUShort -> attributeElement -> IO (Id IOBluetoothSDPServiceAttribute)
withID_attributeElement newAttributeID attributeElement =
  do
    cls' <- getRequiredClass "IOBluetoothSDPServiceAttribute"
    sendClassMessage cls' withID_attributeElementSelector newAttributeID (toIOBluetoothSDPDataElement attributeElement)

-- | initWithID:attributeElementValue:
--
-- Initializes a new service attribute with the given ID and element value.
--
-- See +[IOBluetoothSDPDataElement withElementValue:] for a description of the types that                 may be passed in as the attributeElementValue.
--
-- @attributeID@ — The attribute ID of the new service attribute.
--
-- @attributeElementValue@ — The data element value of the new service attribute
--
-- Returns: Returns self if successful.  Returns nil if there was an error parsing the element value.
--
-- ObjC selector: @- initWithID:attributeElementValue:@
initWithID_attributeElementValue :: (IsIOBluetoothSDPServiceAttribute ioBluetoothSDPServiceAttribute, IsNSObject attributeElementValue) => ioBluetoothSDPServiceAttribute -> CUShort -> attributeElementValue -> IO (Id IOBluetoothSDPServiceAttribute)
initWithID_attributeElementValue ioBluetoothSDPServiceAttribute newAttributeID attributeElementValue =
  sendOwnedMessage ioBluetoothSDPServiceAttribute initWithID_attributeElementValueSelector newAttributeID (toNSObject attributeElementValue)

-- | initWithID:attributeElement:
--
-- Initializes a new service attribute with the given ID and data element.
--
-- @attributeID@ — The attribute ID of the new service attribute.
--
-- @attributeElement@ — The data element of the new service attribute.
--
-- Returns: Returns self if successful.  Returns nil if there was an error.
--
-- ObjC selector: @- initWithID:attributeElement:@
initWithID_attributeElement :: (IsIOBluetoothSDPServiceAttribute ioBluetoothSDPServiceAttribute, IsIOBluetoothSDPDataElement attributeElement) => ioBluetoothSDPServiceAttribute -> CUShort -> attributeElement -> IO (Id IOBluetoothSDPServiceAttribute)
initWithID_attributeElement ioBluetoothSDPServiceAttribute newAttributeID attributeElement =
  sendOwnedMessage ioBluetoothSDPServiceAttribute initWithID_attributeElementSelector newAttributeID (toIOBluetoothSDPDataElement attributeElement)

-- | getAttributeID
--
-- Returns the attribute ID for the target service attribute.
--
-- Returns: Returns the attribute ID for the target service attribute.
--
-- ObjC selector: @- getAttributeID@
getAttributeID :: IsIOBluetoothSDPServiceAttribute ioBluetoothSDPServiceAttribute => ioBluetoothSDPServiceAttribute -> IO CUShort
getAttributeID ioBluetoothSDPServiceAttribute =
  sendMessage ioBluetoothSDPServiceAttribute getAttributeIDSelector

-- | getDataElement
--
-- Returns the data element for the target service attribute.
--
-- Returns: Returns the data element for the target service attribute.
--
-- ObjC selector: @- getDataElement@
getDataElement :: IsIOBluetoothSDPServiceAttribute ioBluetoothSDPServiceAttribute => ioBluetoothSDPServiceAttribute -> IO (Id IOBluetoothSDPDataElement)
getDataElement ioBluetoothSDPServiceAttribute =
  sendMessage ioBluetoothSDPServiceAttribute getDataElementSelector

-- | getIDDataElement
--
-- Returns the data element representing the attribute ID for the target service attribute.
--
-- Returns: Returns the data element representing the attribute ID for the target service attribute.
--
-- ObjC selector: @- getIDDataElement@
getIDDataElement :: IsIOBluetoothSDPServiceAttribute ioBluetoothSDPServiceAttribute => ioBluetoothSDPServiceAttribute -> IO (Id IOBluetoothSDPDataElement)
getIDDataElement ioBluetoothSDPServiceAttribute =
  sendMessage ioBluetoothSDPServiceAttribute getIDDataElementSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @withID:attributeElementValue:@
withID_attributeElementValueSelector :: Selector '[CUShort, Id NSObject] (Id IOBluetoothSDPServiceAttribute)
withID_attributeElementValueSelector = mkSelector "withID:attributeElementValue:"

-- | @Selector@ for @withID:attributeElement:@
withID_attributeElementSelector :: Selector '[CUShort, Id IOBluetoothSDPDataElement] (Id IOBluetoothSDPServiceAttribute)
withID_attributeElementSelector = mkSelector "withID:attributeElement:"

-- | @Selector@ for @initWithID:attributeElementValue:@
initWithID_attributeElementValueSelector :: Selector '[CUShort, Id NSObject] (Id IOBluetoothSDPServiceAttribute)
initWithID_attributeElementValueSelector = mkSelector "initWithID:attributeElementValue:"

-- | @Selector@ for @initWithID:attributeElement:@
initWithID_attributeElementSelector :: Selector '[CUShort, Id IOBluetoothSDPDataElement] (Id IOBluetoothSDPServiceAttribute)
initWithID_attributeElementSelector = mkSelector "initWithID:attributeElement:"

-- | @Selector@ for @getAttributeID@
getAttributeIDSelector :: Selector '[] CUShort
getAttributeIDSelector = mkSelector "getAttributeID"

-- | @Selector@ for @getDataElement@
getDataElementSelector :: Selector '[] (Id IOBluetoothSDPDataElement)
getDataElementSelector = mkSelector "getDataElement"

-- | @Selector@ for @getIDDataElement@
getIDDataElementSelector :: Selector '[] (Id IOBluetoothSDPDataElement)
getIDDataElementSelector = mkSelector "getIDDataElement"

