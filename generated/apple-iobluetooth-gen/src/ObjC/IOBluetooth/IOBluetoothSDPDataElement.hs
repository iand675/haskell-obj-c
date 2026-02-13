{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOBluetoothSDPDataElement
--
-- An instance of this class represents a single SDP data element as defined by the Bluetooth SDP spec.
--
-- The data types described by the spec have been mapped onto the base Foundation classes NSNumber,                 NSArray, NSData as well as IOBluetoothSDPUUID.  The number and boolean types (type descriptor 1, 2                 and 5) are represented as NSNumber objects with the exception of 128-bit numbers which are                 represented as NSData objects in their raw format.  The UUID type (type descriptor 3) is                 represented by IOBluetoothSDPUUID.  The string and URL types (type descriptor 4 and 8) are                 represented by NSString.  The sequence types (type descriptor 6 and 7) are represented by NSArray.
--
-- Typically, you will not need to create an IOBluetoothSDPDataElement directly, the system will                do that automatically for both client and server operations.  However, the current API for adding                 SDP services to the system does allow the use of an NSDictionary based format for creating new                 services.  The purpose for that is to allow a service to be built up completely in a text file                (a plist for example) and then easily imported into an app and added to the system without a                 lot of tedious code to build up the entire SDP service record.
--
-- The basis for that NSDictionary structure comes from the IOBluetoothSDPDataElement.  At its                simplest, a data element is made up of three parts: the type descriptor, the size (from which                the size descriptor is generated) and the actual value.  To provide a complete representation                of a data element, an NSDictionary with three entries can be used.  Each of the three entries                has a key/value pair representing one of the three attributes of a data element.  The first                key/value pair has a key 'DataElementType' that contains a number value with the actual                type descriptor for the data element.  The second pair has a key 'DataElementSize' that                contains the actual size of the element in bytes.  The size descriptor will be calculated                based on the size and type of the element.  The third pair is the value itself whose key is                'DataElementValue' and whose type corresponds to the type mapping above.
--
-- In addition to this complete description of a data element, their are some shortcuts that                can be used for some of the common types and sizes.
--
-- If the 'DataElementType' value is one of the numeric types (1, 2), the 'DataElementValue'                 can be an NSData instead of an NSNumber.  In that case, the numeric data is taken in network                 byte order (MSB first).  Additionally, the 'DataElementSize' parameter may be omitted and the                 size will be taken from the length of the data object.
--
-- If the 'DataElementType' value is the nil type (0), no 'DataElementSize' or 'DataElementValue'                entries are needed.
--
-- If the 'DataElementType' value is any of the other types, the 'DataElementSize' entry is not                needed since the size will be taken directly from the value (data, array, string).
--
-- In the case where the element is an unsigned, 32-bit integer (type descriptor 1, size descriptor                4), the value itself may simply be a number (instead of a dictionary as in the previous examples).
--
-- In the case where the element is a UUID (type descriptor 3), the value itself may be a data object.                  The UUID type will be inferred and the size taken from the length of the data object.
--
-- In the case where the element is a text string (type descriptor 4), the value may be a string object.                The text string type will be inferred and the size taken from the length of the string.
--
-- In the case where the element is a data element sequence, the value may be an array object.  The                type will be inferred and the size taken from the length of the array.  Additionally, the array                must contain sub-elements that will be parsed out individually.
--
-- Generated bindings for @IOBluetoothSDPDataElement@.
module ObjC.IOBluetooth.IOBluetoothSDPDataElement
  ( IOBluetoothSDPDataElement
  , IsIOBluetoothSDPDataElement(..)
  , withElementValue
  , withType_sizeDescriptor_size_value
  , withSDPDataElementRef
  , initWithElementValue
  , initWithType_sizeDescriptor_size_value
  , getSDPDataElementRef
  , getTypeDescriptor
  , getSizeDescriptor
  , getSize
  , getNumberValue
  , getDataValue
  , getStringValue
  , getArrayValue
  , getUUIDValue
  , getValue
  , containsDataElement
  , containsValue
  , containsDataElementSelector
  , containsValueSelector
  , getArrayValueSelector
  , getDataValueSelector
  , getNumberValueSelector
  , getSDPDataElementRefSelector
  , getSizeDescriptorSelector
  , getSizeSelector
  , getStringValueSelector
  , getTypeDescriptorSelector
  , getUUIDValueSelector
  , getValueSelector
  , initWithElementValueSelector
  , initWithType_sizeDescriptor_size_valueSelector
  , withElementValueSelector
  , withSDPDataElementRefSelector
  , withType_sizeDescriptor_size_valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IOBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | withElementValue:
--
-- Creates a new IOBluetoothSDPDataElement with the given value.
--
-- The value must follow the format listed above and must be an instance of NSData, NSString, NSNumber,                 NSArray, NSDictionary, IOBluetoothSDPUUID.
--
-- @element@ — The data element value of one of the specified types.
--
-- Returns: Returns the newly allocated data element object.  Returns nil if there was an error parsing the element            value.  The returned IOBluetoothSDPDataElement object has been autoreleased, so it is not necessary            for the caller to release it.  If the object is to be referenced and kept around, retain should be            called.
--
-- ObjC selector: @+ withElementValue:@
withElementValue :: IsNSObject element => element -> IO (Id IOBluetoothSDPDataElement)
withElementValue element =
  do
    cls' <- getRequiredClass "IOBluetoothSDPDataElement"
    sendClassMessage cls' withElementValueSelector (toNSObject element)

-- | withType:sizeDescriptor:size:value:
--
-- Creates a new IOBluetoothSDPDataElement with the given attributes.
--
-- Warning - be careful using this method.  There is next to no error checking done on the                 attributes.  It is entirely possible to construct an invalid data element.  It is recommended                that +withElementValue: be used instead of this one.
--
-- @typeDescriptor@ — The type descriptor for the data element.
--
-- @sizeDescriptor@ — The size descriptor for the data element (verify it matches the size parameter).
--
-- @size@ — The size of the data element in bytes (make sure it is a valid size for the given size descriptor).
--
-- @value@ — The raw value itself.  This must be the base NSString, NSNumber, NSArray or NSData objects.  It                    may not be NSDictionary.  If a dictionary format is present, use +withElementValue:.
--
-- Returns: Returns the newly allocated data element object.  Returns nil if an error is encountered (not likely                    due to the limited error checking currently done).  The returned IOBluetoothSDPDataElement                     object has been autoreleased, so it is not necessary for the caller to release it.  If the                     object is to be referenced and kept around, retain should be called.
--
-- ObjC selector: @+ withType:sizeDescriptor:size:value:@
withType_sizeDescriptor_size_value :: IsNSObject newValue => CUChar -> CUChar -> CUInt -> newValue -> IO (Id IOBluetoothSDPDataElement)
withType_sizeDescriptor_size_value type_ newSizeDescriptor newSize newValue =
  do
    cls' <- getRequiredClass "IOBluetoothSDPDataElement"
    sendClassMessage cls' withType_sizeDescriptor_size_valueSelector type_ newSizeDescriptor newSize (toNSObject newValue)

-- | withSDPDataElementRef:
--
-- Method call to convert an IOBluetoothSDPDataElementRef into an IOBluetoothSDPDataElement *.
--
-- @sdpDataElementRef@ — IOBluetoothSDPDataElementRef for which an IOBluetoothSDPDataElement * is desired.
--
-- Returns: Returns the IOBluetoothSDPDataElement * for the given IOBluetoothSDPDataElementRef.
--
-- ObjC selector: @+ withSDPDataElementRef:@
withSDPDataElementRef :: Ptr () -> IO (Id IOBluetoothSDPDataElement)
withSDPDataElementRef sdpDataElementRef =
  do
    cls' <- getRequiredClass "IOBluetoothSDPDataElement"
    sendClassMessage cls' withSDPDataElementRefSelector sdpDataElementRef

-- | initWithElementValue:
--
-- Initializes a new IOBluetoothSDPDataElement with the given value.
--
-- The value must follow the format listed above and must be an instance of NSData, NSString, NSNumber,                 NSArray, NSDictionary, IOBluetoothSDPUUID.
--
-- @element@ — The data element value of one of the specified types.
--
-- Returns: Returns self if successful.  Returns nil if there was an error parsing the element value.
--
-- ObjC selector: @- initWithElementValue:@
initWithElementValue :: (IsIOBluetoothSDPDataElement ioBluetoothSDPDataElement, IsNSObject element) => ioBluetoothSDPDataElement -> element -> IO (Id IOBluetoothSDPDataElement)
initWithElementValue ioBluetoothSDPDataElement element =
  sendOwnedMessage ioBluetoothSDPDataElement initWithElementValueSelector (toNSObject element)

-- | initWithType:sizeDescriptor:size:value:
--
-- Initializes a new IOBluetoothSDPDataElement with the given attributes.
--
-- Warning - be careful using this method.  There is next to no error checking done on the                 attributes.  It is entirely possible to construct an invalid data element.  It is recommended                that +withElementValue: be used instead of this one.
--
-- @typeDescriptor@ — The type descriptor for the data element.
--
-- @sizeDescriptor@ — The size descriptor for the data element (verify it matches the size parameter).
--
-- @size@ — The size of the data element in bytes (make sure it is a valid size for the given size descriptor).
--
-- @value@ — The raw value itself.  This must be the base NSString, NSNumber, NSArray or NSData objects.  It                    may not be NSDictionary.  If a dictionary format is present, use +withElementValue:.
--
-- Returns: Returns self if successful.  Returns nil if an error is encountered (not likely                    due to the limited error checking currently done).
--
-- ObjC selector: @- initWithType:sizeDescriptor:size:value:@
initWithType_sizeDescriptor_size_value :: (IsIOBluetoothSDPDataElement ioBluetoothSDPDataElement, IsNSObject newValue) => ioBluetoothSDPDataElement -> CUChar -> CUChar -> CUInt -> newValue -> IO (Id IOBluetoothSDPDataElement)
initWithType_sizeDescriptor_size_value ioBluetoothSDPDataElement newType newSizeDescriptor newSize newValue =
  sendOwnedMessage ioBluetoothSDPDataElement initWithType_sizeDescriptor_size_valueSelector newType newSizeDescriptor newSize (toNSObject newValue)

-- | getSDPDataElementRef
--
-- Returns an IOBluetoothSDPDataElementRef representation of the target IOBluetoothSDPDataElement object.
--
-- Returns: Returns an IOBluetoothSDPDataElementRef representation of the target IOBluetoothSDPDataElement object.
--
-- ObjC selector: @- getSDPDataElementRef@
getSDPDataElementRef :: IsIOBluetoothSDPDataElement ioBluetoothSDPDataElement => ioBluetoothSDPDataElement -> IO (Ptr ())
getSDPDataElementRef ioBluetoothSDPDataElement =
  sendMessage ioBluetoothSDPDataElement getSDPDataElementRefSelector

-- | getTypeDescriptor
--
-- Returns the SDP spec defined data element type descriptor for the target data element.
--
-- Returns: Returns the type descriptor for the target data element.
--
-- ObjC selector: @- getTypeDescriptor@
getTypeDescriptor :: IsIOBluetoothSDPDataElement ioBluetoothSDPDataElement => ioBluetoothSDPDataElement -> IO CUChar
getTypeDescriptor ioBluetoothSDPDataElement =
  sendMessage ioBluetoothSDPDataElement getTypeDescriptorSelector

-- | getSizeDescriptor
--
-- Returns the SDP spec defined data element size descriptor for the target data element.
--
-- Returns: Returns the size descriptor for the target data element.
--
-- ObjC selector: @- getSizeDescriptor@
getSizeDescriptor :: IsIOBluetoothSDPDataElement ioBluetoothSDPDataElement => ioBluetoothSDPDataElement -> IO CUChar
getSizeDescriptor ioBluetoothSDPDataElement =
  sendMessage ioBluetoothSDPDataElement getSizeDescriptorSelector

-- | getSize
--
-- Returns the size in bytes of the target data element.
--
-- The size is valid whether the data element has a fixed or variable size descriptor.
--
-- Returns: Returns the size in bytes of the target data element.
--
-- ObjC selector: @- getSize@
getSize :: IsIOBluetoothSDPDataElement ioBluetoothSDPDataElement => ioBluetoothSDPDataElement -> IO CUInt
getSize ioBluetoothSDPDataElement =
  sendMessage ioBluetoothSDPDataElement getSizeSelector

-- | getNumberValue
--
-- If the data element is represented by a number, it returns the value as an NSNumber.
--
-- The data types represented by a number are 1 (unsigned int), 2 (signed int) and 5 (boolean)                 except for 128-bit versions of 1 and 2.
--
-- Returns: Returns an NSNumber representation of the data element if it is a numeric type.
--
-- ObjC selector: @- getNumberValue@
getNumberValue :: IsIOBluetoothSDPDataElement ioBluetoothSDPDataElement => ioBluetoothSDPDataElement -> IO (Id NSNumber)
getNumberValue ioBluetoothSDPDataElement =
  sendMessage ioBluetoothSDPDataElement getNumberValueSelector

-- | getDataValue
--
-- If the data element is represented by a data object, it returns the value as an NSData.
--
-- The data types represented by a data object are 128-bit versions of 1 (unsigned int) and                 2 (signed int).
--
-- Returns: Returns an NSData representation of the data element if it is a 128-bit number.
--
-- ObjC selector: @- getDataValue@
getDataValue :: IsIOBluetoothSDPDataElement ioBluetoothSDPDataElement => ioBluetoothSDPDataElement -> IO (Id NSData)
getDataValue ioBluetoothSDPDataElement =
  sendMessage ioBluetoothSDPDataElement getDataValueSelector

-- | getStringValue
--
-- If the data element is represented by a string object, it returns the value as an NSString.
--
-- The data types represented by a string object are 4 (text string) and 8 (URL).
--
-- Returns: Returns an NSString representation of the data element if it is a text or URL type.
--
-- ObjC selector: @- getStringValue@
getStringValue :: IsIOBluetoothSDPDataElement ioBluetoothSDPDataElement => ioBluetoothSDPDataElement -> IO (Id NSString)
getStringValue ioBluetoothSDPDataElement =
  sendMessage ioBluetoothSDPDataElement getStringValueSelector

-- | getArrayValue
--
-- If the data element is represented by an array object, it returns the value as an NSArray.
--
-- The data types represented by an array object are 6 (data element sequence) and 7 (data                element alternative).
--
-- Returns: Returns an NSArray representation of the data element if it is a sequence type.
--
-- ObjC selector: @- getArrayValue@
getArrayValue :: IsIOBluetoothSDPDataElement ioBluetoothSDPDataElement => ioBluetoothSDPDataElement -> IO (Id NSArray)
getArrayValue ioBluetoothSDPDataElement =
  sendMessage ioBluetoothSDPDataElement getArrayValueSelector

-- | getUUIDValue
--
-- If the data element is a UUID (type 3), it returns the value as an IOBluetoothSDPUUID.
--
-- Returns: Returns an IOBluetoothSDPUUID representation of the data element if it is a UUID.
--
-- ObjC selector: @- getUUIDValue@
getUUIDValue :: IsIOBluetoothSDPDataElement ioBluetoothSDPDataElement => ioBluetoothSDPDataElement -> IO (Id IOBluetoothSDPUUID)
getUUIDValue ioBluetoothSDPDataElement =
  sendMessage ioBluetoothSDPDataElement getUUIDValueSelector

-- | getValue
--
-- Returns the object value of the data element.
--
-- The value returned may be an NSNumber, NSString, NSData, NSArray or IOBluetoothSDPDataElement                depending on the type of the data element.
--
-- Returns: Returns the object value of the target data element.
--
-- ObjC selector: @- getValue@
getValue :: IsIOBluetoothSDPDataElement ioBluetoothSDPDataElement => ioBluetoothSDPDataElement -> IO (Id NSObject)
getValue ioBluetoothSDPDataElement =
  sendMessage ioBluetoothSDPDataElement getValueSelector

-- | containsDataElement:
--
-- Checks to see if the target data element is the same as the dataElement parameter or if it contains                the dataElement parameter (if its a sequence type).
--
-- If the target data element is not a sequence type, this method simply compares the two data elements.  If                it is a sequence type, it will search through the sequence (and sub-sequences) for the dataElement                parameter.
--
-- @dataElement@ — The data element to compare with (and search for).
--
-- Returns: Returns TRUE if the target either matches the given data element or if it contains the given data element.
--
-- ObjC selector: @- containsDataElement:@
containsDataElement :: (IsIOBluetoothSDPDataElement ioBluetoothSDPDataElement, IsIOBluetoothSDPDataElement dataElement) => ioBluetoothSDPDataElement -> dataElement -> IO Bool
containsDataElement ioBluetoothSDPDataElement dataElement =
  sendMessage ioBluetoothSDPDataElement containsDataElementSelector (toIOBluetoothSDPDataElement dataElement)

-- | containsValue:
--
-- Checks to see if the target data element's value is the same as the value parameter or if it contains                the value parameter.
--
-- This method works just like -containsDataElement: except that it is comparing the value objects directly.
--
-- @cmpValue@ — The value to compare with (and search for).
--
-- Returns: Returns TRUE if the target's value either matches the given value or if it contains the given value.
--
-- ObjC selector: @- containsValue:@
containsValue :: (IsIOBluetoothSDPDataElement ioBluetoothSDPDataElement, IsNSObject cmpValue) => ioBluetoothSDPDataElement -> cmpValue -> IO Bool
containsValue ioBluetoothSDPDataElement cmpValue =
  sendMessage ioBluetoothSDPDataElement containsValueSelector (toNSObject cmpValue)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @withElementValue:@
withElementValueSelector :: Selector '[Id NSObject] (Id IOBluetoothSDPDataElement)
withElementValueSelector = mkSelector "withElementValue:"

-- | @Selector@ for @withType:sizeDescriptor:size:value:@
withType_sizeDescriptor_size_valueSelector :: Selector '[CUChar, CUChar, CUInt, Id NSObject] (Id IOBluetoothSDPDataElement)
withType_sizeDescriptor_size_valueSelector = mkSelector "withType:sizeDescriptor:size:value:"

-- | @Selector@ for @withSDPDataElementRef:@
withSDPDataElementRefSelector :: Selector '[Ptr ()] (Id IOBluetoothSDPDataElement)
withSDPDataElementRefSelector = mkSelector "withSDPDataElementRef:"

-- | @Selector@ for @initWithElementValue:@
initWithElementValueSelector :: Selector '[Id NSObject] (Id IOBluetoothSDPDataElement)
initWithElementValueSelector = mkSelector "initWithElementValue:"

-- | @Selector@ for @initWithType:sizeDescriptor:size:value:@
initWithType_sizeDescriptor_size_valueSelector :: Selector '[CUChar, CUChar, CUInt, Id NSObject] (Id IOBluetoothSDPDataElement)
initWithType_sizeDescriptor_size_valueSelector = mkSelector "initWithType:sizeDescriptor:size:value:"

-- | @Selector@ for @getSDPDataElementRef@
getSDPDataElementRefSelector :: Selector '[] (Ptr ())
getSDPDataElementRefSelector = mkSelector "getSDPDataElementRef"

-- | @Selector@ for @getTypeDescriptor@
getTypeDescriptorSelector :: Selector '[] CUChar
getTypeDescriptorSelector = mkSelector "getTypeDescriptor"

-- | @Selector@ for @getSizeDescriptor@
getSizeDescriptorSelector :: Selector '[] CUChar
getSizeDescriptorSelector = mkSelector "getSizeDescriptor"

-- | @Selector@ for @getSize@
getSizeSelector :: Selector '[] CUInt
getSizeSelector = mkSelector "getSize"

-- | @Selector@ for @getNumberValue@
getNumberValueSelector :: Selector '[] (Id NSNumber)
getNumberValueSelector = mkSelector "getNumberValue"

-- | @Selector@ for @getDataValue@
getDataValueSelector :: Selector '[] (Id NSData)
getDataValueSelector = mkSelector "getDataValue"

-- | @Selector@ for @getStringValue@
getStringValueSelector :: Selector '[] (Id NSString)
getStringValueSelector = mkSelector "getStringValue"

-- | @Selector@ for @getArrayValue@
getArrayValueSelector :: Selector '[] (Id NSArray)
getArrayValueSelector = mkSelector "getArrayValue"

-- | @Selector@ for @getUUIDValue@
getUUIDValueSelector :: Selector '[] (Id IOBluetoothSDPUUID)
getUUIDValueSelector = mkSelector "getUUIDValue"

-- | @Selector@ for @getValue@
getValueSelector :: Selector '[] (Id NSObject)
getValueSelector = mkSelector "getValue"

-- | @Selector@ for @containsDataElement:@
containsDataElementSelector :: Selector '[Id IOBluetoothSDPDataElement] Bool
containsDataElementSelector = mkSelector "containsDataElement:"

-- | @Selector@ for @containsValue:@
containsValueSelector :: Selector '[Id NSObject] Bool
containsValueSelector = mkSelector "containsValue:"

