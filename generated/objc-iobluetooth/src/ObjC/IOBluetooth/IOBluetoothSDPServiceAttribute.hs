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
  , withID_attributeElementValueSelector
  , withID_attributeElementSelector
  , initWithID_attributeElementValueSelector
  , initWithID_attributeElementSelector
  , getAttributeIDSelector
  , getDataElementSelector
  , getIDDataElementSelector


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
    withObjCPtr attributeElementValue $ \raw_attributeElementValue ->
      sendClassMsg cls' (mkSelector "withID:attributeElementValue:") (retPtr retVoid) [argCUInt (fromIntegral newAttributeID), argPtr (castPtr raw_attributeElementValue :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr attributeElement $ \raw_attributeElement ->
      sendClassMsg cls' (mkSelector "withID:attributeElement:") (retPtr retVoid) [argCUInt (fromIntegral newAttributeID), argPtr (castPtr raw_attributeElement :: Ptr ())] >>= retainedObject . castPtr

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
initWithID_attributeElementValue ioBluetoothSDPServiceAttribute  newAttributeID attributeElementValue =
withObjCPtr attributeElementValue $ \raw_attributeElementValue ->
    sendMsg ioBluetoothSDPServiceAttribute (mkSelector "initWithID:attributeElementValue:") (retPtr retVoid) [argCUInt (fromIntegral newAttributeID), argPtr (castPtr raw_attributeElementValue :: Ptr ())] >>= ownedObject . castPtr

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
initWithID_attributeElement ioBluetoothSDPServiceAttribute  newAttributeID attributeElement =
withObjCPtr attributeElement $ \raw_attributeElement ->
    sendMsg ioBluetoothSDPServiceAttribute (mkSelector "initWithID:attributeElement:") (retPtr retVoid) [argCUInt (fromIntegral newAttributeID), argPtr (castPtr raw_attributeElement :: Ptr ())] >>= ownedObject . castPtr

-- | getAttributeID
--
-- Returns the attribute ID for the target service attribute.
--
-- Returns: Returns the attribute ID for the target service attribute.
--
-- ObjC selector: @- getAttributeID@
getAttributeID :: IsIOBluetoothSDPServiceAttribute ioBluetoothSDPServiceAttribute => ioBluetoothSDPServiceAttribute -> IO CUShort
getAttributeID ioBluetoothSDPServiceAttribute  =
  fmap fromIntegral $ sendMsg ioBluetoothSDPServiceAttribute (mkSelector "getAttributeID") retCUInt []

-- | getDataElement
--
-- Returns the data element for the target service attribute.
--
-- Returns: Returns the data element for the target service attribute.
--
-- ObjC selector: @- getDataElement@
getDataElement :: IsIOBluetoothSDPServiceAttribute ioBluetoothSDPServiceAttribute => ioBluetoothSDPServiceAttribute -> IO (Id IOBluetoothSDPDataElement)
getDataElement ioBluetoothSDPServiceAttribute  =
  sendMsg ioBluetoothSDPServiceAttribute (mkSelector "getDataElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | getIDDataElement
--
-- Returns the data element representing the attribute ID for the target service attribute.
--
-- Returns: Returns the data element representing the attribute ID for the target service attribute.
--
-- ObjC selector: @- getIDDataElement@
getIDDataElement :: IsIOBluetoothSDPServiceAttribute ioBluetoothSDPServiceAttribute => ioBluetoothSDPServiceAttribute -> IO (Id IOBluetoothSDPDataElement)
getIDDataElement ioBluetoothSDPServiceAttribute  =
  sendMsg ioBluetoothSDPServiceAttribute (mkSelector "getIDDataElement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @withID:attributeElementValue:@
withID_attributeElementValueSelector :: Selector
withID_attributeElementValueSelector = mkSelector "withID:attributeElementValue:"

-- | @Selector@ for @withID:attributeElement:@
withID_attributeElementSelector :: Selector
withID_attributeElementSelector = mkSelector "withID:attributeElement:"

-- | @Selector@ for @initWithID:attributeElementValue:@
initWithID_attributeElementValueSelector :: Selector
initWithID_attributeElementValueSelector = mkSelector "initWithID:attributeElementValue:"

-- | @Selector@ for @initWithID:attributeElement:@
initWithID_attributeElementSelector :: Selector
initWithID_attributeElementSelector = mkSelector "initWithID:attributeElement:"

-- | @Selector@ for @getAttributeID@
getAttributeIDSelector :: Selector
getAttributeIDSelector = mkSelector "getAttributeID"

-- | @Selector@ for @getDataElement@
getDataElementSelector :: Selector
getDataElementSelector = mkSelector "getDataElement"

-- | @Selector@ for @getIDDataElement@
getIDDataElementSelector :: Selector
getIDDataElementSelector = mkSelector "getIDDataElement"

