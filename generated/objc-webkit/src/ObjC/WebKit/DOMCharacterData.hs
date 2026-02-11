{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMCharacterData@.
module ObjC.WebKit.DOMCharacterData
  ( DOMCharacterData
  , IsDOMCharacterData(..)
  , substringData_length
  , appendData
  , insertData_data
  , deleteData_length
  , replaceData_length_data
  , substringData
  , insertData
  , deleteData
  , replaceData
  , data_
  , setData
  , length_
  , substringData_lengthSelector
  , appendDataSelector
  , insertData_dataSelector
  , deleteData_lengthSelector
  , replaceData_length_dataSelector
  , substringDataSelector
  , insertDataSelector
  , deleteDataSelector
  , replaceDataSelector
  , dataSelector
  , setDataSelector
  , lengthSelector


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

-- | @- substringData:length:@
substringData_length :: IsDOMCharacterData domCharacterData => domCharacterData -> CUInt -> CUInt -> IO (Id NSString)
substringData_length domCharacterData  offset length_ =
  sendMsg domCharacterData (mkSelector "substringData:length:") (retPtr retVoid) [argCUInt (fromIntegral offset), argCUInt (fromIntegral length_)] >>= retainedObject . castPtr

-- | @- appendData:@
appendData :: (IsDOMCharacterData domCharacterData, IsNSString data_) => domCharacterData -> data_ -> IO ()
appendData domCharacterData  data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg domCharacterData (mkSelector "appendData:") retVoid [argPtr (castPtr raw_data_ :: Ptr ())]

-- | @- insertData:data:@
insertData_data :: (IsDOMCharacterData domCharacterData, IsNSString data_) => domCharacterData -> CUInt -> data_ -> IO ()
insertData_data domCharacterData  offset data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg domCharacterData (mkSelector "insertData:data:") retVoid [argCUInt (fromIntegral offset), argPtr (castPtr raw_data_ :: Ptr ())]

-- | @- deleteData:length:@
deleteData_length :: IsDOMCharacterData domCharacterData => domCharacterData -> CUInt -> CUInt -> IO ()
deleteData_length domCharacterData  offset length_ =
  sendMsg domCharacterData (mkSelector "deleteData:length:") retVoid [argCUInt (fromIntegral offset), argCUInt (fromIntegral length_)]

-- | @- replaceData:length:data:@
replaceData_length_data :: (IsDOMCharacterData domCharacterData, IsNSString data_) => domCharacterData -> CUInt -> CUInt -> data_ -> IO ()
replaceData_length_data domCharacterData  offset length_ data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg domCharacterData (mkSelector "replaceData:length:data:") retVoid [argCUInt (fromIntegral offset), argCUInt (fromIntegral length_), argPtr (castPtr raw_data_ :: Ptr ())]

-- | @- substringData::@
substringData :: IsDOMCharacterData domCharacterData => domCharacterData -> CUInt -> CUInt -> IO (Id NSString)
substringData domCharacterData  offset length_ =
  sendMsg domCharacterData (mkSelector "substringData::") (retPtr retVoid) [argCUInt (fromIntegral offset), argCUInt (fromIntegral length_)] >>= retainedObject . castPtr

-- | @- insertData::@
insertData :: (IsDOMCharacterData domCharacterData, IsNSString data_) => domCharacterData -> CUInt -> data_ -> IO ()
insertData domCharacterData  offset data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg domCharacterData (mkSelector "insertData::") retVoid [argCUInt (fromIntegral offset), argPtr (castPtr raw_data_ :: Ptr ())]

-- | @- deleteData::@
deleteData :: IsDOMCharacterData domCharacterData => domCharacterData -> CUInt -> CUInt -> IO ()
deleteData domCharacterData  offset length_ =
  sendMsg domCharacterData (mkSelector "deleteData::") retVoid [argCUInt (fromIntegral offset), argCUInt (fromIntegral length_)]

-- | @- replaceData:::@
replaceData :: (IsDOMCharacterData domCharacterData, IsNSString data_) => domCharacterData -> CUInt -> CUInt -> data_ -> IO ()
replaceData domCharacterData  offset length_ data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg domCharacterData (mkSelector "replaceData:::") retVoid [argCUInt (fromIntegral offset), argCUInt (fromIntegral length_), argPtr (castPtr raw_data_ :: Ptr ())]

-- | @- data@
data_ :: IsDOMCharacterData domCharacterData => domCharacterData -> IO (Id NSString)
data_ domCharacterData  =
  sendMsg domCharacterData (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setData:@
setData :: (IsDOMCharacterData domCharacterData, IsNSString value) => domCharacterData -> value -> IO ()
setData domCharacterData  value =
withObjCPtr value $ \raw_value ->
    sendMsg domCharacterData (mkSelector "setData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- length@
length_ :: IsDOMCharacterData domCharacterData => domCharacterData -> IO CUInt
length_ domCharacterData  =
  sendMsg domCharacterData (mkSelector "length") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @substringData:length:@
substringData_lengthSelector :: Selector
substringData_lengthSelector = mkSelector "substringData:length:"

-- | @Selector@ for @appendData:@
appendDataSelector :: Selector
appendDataSelector = mkSelector "appendData:"

-- | @Selector@ for @insertData:data:@
insertData_dataSelector :: Selector
insertData_dataSelector = mkSelector "insertData:data:"

-- | @Selector@ for @deleteData:length:@
deleteData_lengthSelector :: Selector
deleteData_lengthSelector = mkSelector "deleteData:length:"

-- | @Selector@ for @replaceData:length:data:@
replaceData_length_dataSelector :: Selector
replaceData_length_dataSelector = mkSelector "replaceData:length:data:"

-- | @Selector@ for @substringData::@
substringDataSelector :: Selector
substringDataSelector = mkSelector "substringData::"

-- | @Selector@ for @insertData::@
insertDataSelector :: Selector
insertDataSelector = mkSelector "insertData::"

-- | @Selector@ for @deleteData::@
deleteDataSelector :: Selector
deleteDataSelector = mkSelector "deleteData::"

-- | @Selector@ for @replaceData:::@
replaceDataSelector :: Selector
replaceDataSelector = mkSelector "replaceData:::"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector
setDataSelector = mkSelector "setData:"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

