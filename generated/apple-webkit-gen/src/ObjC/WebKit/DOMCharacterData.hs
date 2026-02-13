{-# LANGUAGE DataKinds #-}
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
  , appendDataSelector
  , dataSelector
  , deleteDataSelector
  , deleteData_lengthSelector
  , insertDataSelector
  , insertData_dataSelector
  , lengthSelector
  , replaceDataSelector
  , replaceData_length_dataSelector
  , setDataSelector
  , substringDataSelector
  , substringData_lengthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- substringData:length:@
substringData_length :: IsDOMCharacterData domCharacterData => domCharacterData -> CUInt -> CUInt -> IO (Id NSString)
substringData_length domCharacterData offset length_ =
  sendMessage domCharacterData substringData_lengthSelector offset length_

-- | @- appendData:@
appendData :: (IsDOMCharacterData domCharacterData, IsNSString data_) => domCharacterData -> data_ -> IO ()
appendData domCharacterData data_ =
  sendMessage domCharacterData appendDataSelector (toNSString data_)

-- | @- insertData:data:@
insertData_data :: (IsDOMCharacterData domCharacterData, IsNSString data_) => domCharacterData -> CUInt -> data_ -> IO ()
insertData_data domCharacterData offset data_ =
  sendMessage domCharacterData insertData_dataSelector offset (toNSString data_)

-- | @- deleteData:length:@
deleteData_length :: IsDOMCharacterData domCharacterData => domCharacterData -> CUInt -> CUInt -> IO ()
deleteData_length domCharacterData offset length_ =
  sendMessage domCharacterData deleteData_lengthSelector offset length_

-- | @- replaceData:length:data:@
replaceData_length_data :: (IsDOMCharacterData domCharacterData, IsNSString data_) => domCharacterData -> CUInt -> CUInt -> data_ -> IO ()
replaceData_length_data domCharacterData offset length_ data_ =
  sendMessage domCharacterData replaceData_length_dataSelector offset length_ (toNSString data_)

-- | @- substringData::@
substringData :: IsDOMCharacterData domCharacterData => domCharacterData -> CUInt -> CUInt -> IO (Id NSString)
substringData domCharacterData offset length_ =
  sendMessage domCharacterData substringDataSelector offset length_

-- | @- insertData::@
insertData :: (IsDOMCharacterData domCharacterData, IsNSString data_) => domCharacterData -> CUInt -> data_ -> IO ()
insertData domCharacterData offset data_ =
  sendMessage domCharacterData insertDataSelector offset (toNSString data_)

-- | @- deleteData::@
deleteData :: IsDOMCharacterData domCharacterData => domCharacterData -> CUInt -> CUInt -> IO ()
deleteData domCharacterData offset length_ =
  sendMessage domCharacterData deleteDataSelector offset length_

-- | @- replaceData:::@
replaceData :: (IsDOMCharacterData domCharacterData, IsNSString data_) => domCharacterData -> CUInt -> CUInt -> data_ -> IO ()
replaceData domCharacterData offset length_ data_ =
  sendMessage domCharacterData replaceDataSelector offset length_ (toNSString data_)

-- | @- data@
data_ :: IsDOMCharacterData domCharacterData => domCharacterData -> IO (Id NSString)
data_ domCharacterData =
  sendMessage domCharacterData dataSelector

-- | @- setData:@
setData :: (IsDOMCharacterData domCharacterData, IsNSString value) => domCharacterData -> value -> IO ()
setData domCharacterData value =
  sendMessage domCharacterData setDataSelector (toNSString value)

-- | @- length@
length_ :: IsDOMCharacterData domCharacterData => domCharacterData -> IO CUInt
length_ domCharacterData =
  sendMessage domCharacterData lengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @substringData:length:@
substringData_lengthSelector :: Selector '[CUInt, CUInt] (Id NSString)
substringData_lengthSelector = mkSelector "substringData:length:"

-- | @Selector@ for @appendData:@
appendDataSelector :: Selector '[Id NSString] ()
appendDataSelector = mkSelector "appendData:"

-- | @Selector@ for @insertData:data:@
insertData_dataSelector :: Selector '[CUInt, Id NSString] ()
insertData_dataSelector = mkSelector "insertData:data:"

-- | @Selector@ for @deleteData:length:@
deleteData_lengthSelector :: Selector '[CUInt, CUInt] ()
deleteData_lengthSelector = mkSelector "deleteData:length:"

-- | @Selector@ for @replaceData:length:data:@
replaceData_length_dataSelector :: Selector '[CUInt, CUInt, Id NSString] ()
replaceData_length_dataSelector = mkSelector "replaceData:length:data:"

-- | @Selector@ for @substringData::@
substringDataSelector :: Selector '[CUInt, CUInt] (Id NSString)
substringDataSelector = mkSelector "substringData::"

-- | @Selector@ for @insertData::@
insertDataSelector :: Selector '[CUInt, Id NSString] ()
insertDataSelector = mkSelector "insertData::"

-- | @Selector@ for @deleteData::@
deleteDataSelector :: Selector '[CUInt, CUInt] ()
deleteDataSelector = mkSelector "deleteData::"

-- | @Selector@ for @replaceData:::@
replaceDataSelector :: Selector '[CUInt, CUInt, Id NSString] ()
replaceDataSelector = mkSelector "replaceData:::"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSString)
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector '[Id NSString] ()
setDataSelector = mkSelector "setData:"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CUInt
lengthSelector = mkSelector "length"

