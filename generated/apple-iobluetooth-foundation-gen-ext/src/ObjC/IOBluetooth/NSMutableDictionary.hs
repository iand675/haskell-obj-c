{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | **************	Mutable Dictionary	***************
--
-- Generated bindings for @NSMutableDictionary@.
module ObjC.IOBluetooth.NSMutableDictionary
  ( NSMutableDictionary
  , IsNSMutableDictionary(..)
  , dictionaryWithOBEXHeadersData_headersDataSize
  , dictionaryWithOBEXHeadersData
  , getHeaderBytes
  , addTargetHeader_length
  , addHTTPHeader_length
  , addBodyHeader_length_endOfBody
  , addWhoHeader_length
  , addConnectionIDHeader_length
  , addApplicationParameterHeader_length
  , addByteSequenceHeader_length
  , addObjectClassHeader_length
  , addAuthorizationChallengeHeader_length
  , addAuthorizationResponseHeader_length
  , addTimeISOHeader_length
  , addTypeHeader
  , addLengthHeader
  , addTime4ByteHeader
  , addCountHeader
  , addDescriptionHeader
  , addNameHeader
  , addUserDefinedHeader_length
  , addImageHandleHeader
  , addImageDescriptorHeader_length
  , withOBEXHeadersData_headersDataSize
  , addApplicationParameterHeader_lengthSelector
  , addAuthorizationChallengeHeader_lengthSelector
  , addAuthorizationResponseHeader_lengthSelector
  , addBodyHeader_length_endOfBodySelector
  , addByteSequenceHeader_lengthSelector
  , addConnectionIDHeader_lengthSelector
  , addCountHeaderSelector
  , addDescriptionHeaderSelector
  , addHTTPHeader_lengthSelector
  , addImageDescriptorHeader_lengthSelector
  , addImageHandleHeaderSelector
  , addLengthHeaderSelector
  , addNameHeaderSelector
  , addObjectClassHeader_lengthSelector
  , addTargetHeader_lengthSelector
  , addTime4ByteHeaderSelector
  , addTimeISOHeader_lengthSelector
  , addTypeHeaderSelector
  , addUserDefinedHeader_lengthSelector
  , addWhoHeader_lengthSelector
  , dictionaryWithOBEXHeadersDataSelector
  , dictionaryWithOBEXHeadersData_headersDataSizeSelector
  , getHeaderBytesSelector
  , withOBEXHeadersData_headersDataSizeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IOBluetooth.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ dictionaryWithOBEXHeadersData:headersDataSize:@
dictionaryWithOBEXHeadersData_headersDataSize :: Const (Ptr ()) -> CULong -> IO (Id NSMutableDictionary)
dictionaryWithOBEXHeadersData_headersDataSize inHeadersData inDataSize =
  do
    cls' <- getRequiredClass "NSMutableDictionary"
    sendClassMessage cls' dictionaryWithOBEXHeadersData_headersDataSizeSelector inHeadersData inDataSize

-- | @+ dictionaryWithOBEXHeadersData:@
dictionaryWithOBEXHeadersData :: IsNSData inHeadersData => inHeadersData -> IO (Id NSMutableDictionary)
dictionaryWithOBEXHeadersData inHeadersData =
  do
    cls' <- getRequiredClass "NSMutableDictionary"
    sendClassMessage cls' dictionaryWithOBEXHeadersDataSelector (toNSData inHeadersData)

-- | @- getHeaderBytes@
getHeaderBytes :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> IO (Id NSMutableData)
getHeaderBytes nsMutableDictionary =
  sendMessage nsMutableDictionary getHeaderBytesSelector

-- | @- addTargetHeader:length:@
addTargetHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addTargetHeader_length nsMutableDictionary inHeaderData inHeaderDataLength =
  sendMessage nsMutableDictionary addTargetHeader_lengthSelector inHeaderData inHeaderDataLength

-- | @- addHTTPHeader:length:@
addHTTPHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addHTTPHeader_length nsMutableDictionary inHeaderData inHeaderDataLength =
  sendMessage nsMutableDictionary addHTTPHeader_lengthSelector inHeaderData inHeaderDataLength

-- | @- addBodyHeader:length:endOfBody:@
addBodyHeader_length_endOfBody :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> Bool -> IO CInt
addBodyHeader_length_endOfBody nsMutableDictionary inHeaderData inHeaderDataLength isEndOfBody =
  sendMessage nsMutableDictionary addBodyHeader_length_endOfBodySelector inHeaderData inHeaderDataLength isEndOfBody

-- | @- addWhoHeader:length:@
addWhoHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addWhoHeader_length nsMutableDictionary inHeaderData inHeaderDataLength =
  sendMessage nsMutableDictionary addWhoHeader_lengthSelector inHeaderData inHeaderDataLength

-- | @- addConnectionIDHeader:length:@
addConnectionIDHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addConnectionIDHeader_length nsMutableDictionary inHeaderData inHeaderDataLength =
  sendMessage nsMutableDictionary addConnectionIDHeader_lengthSelector inHeaderData inHeaderDataLength

-- | @- addApplicationParameterHeader:length:@
addApplicationParameterHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addApplicationParameterHeader_length nsMutableDictionary inHeaderData inHeaderDataLength =
  sendMessage nsMutableDictionary addApplicationParameterHeader_lengthSelector inHeaderData inHeaderDataLength

-- | @- addByteSequenceHeader:length:@
addByteSequenceHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addByteSequenceHeader_length nsMutableDictionary inHeaderData inHeaderDataLength =
  sendMessage nsMutableDictionary addByteSequenceHeader_lengthSelector inHeaderData inHeaderDataLength

-- | @- addObjectClassHeader:length:@
addObjectClassHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addObjectClassHeader_length nsMutableDictionary inHeaderData inHeaderDataLength =
  sendMessage nsMutableDictionary addObjectClassHeader_lengthSelector inHeaderData inHeaderDataLength

-- | @- addAuthorizationChallengeHeader:length:@
addAuthorizationChallengeHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addAuthorizationChallengeHeader_length nsMutableDictionary inHeaderData inHeaderDataLength =
  sendMessage nsMutableDictionary addAuthorizationChallengeHeader_lengthSelector inHeaderData inHeaderDataLength

-- | @- addAuthorizationResponseHeader:length:@
addAuthorizationResponseHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addAuthorizationResponseHeader_length nsMutableDictionary inHeaderData inHeaderDataLength =
  sendMessage nsMutableDictionary addAuthorizationResponseHeader_lengthSelector inHeaderData inHeaderDataLength

-- | @- addTimeISOHeader:length:@
addTimeISOHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addTimeISOHeader_length nsMutableDictionary inHeaderData inHeaderDataLength =
  sendMessage nsMutableDictionary addTimeISOHeader_lengthSelector inHeaderData inHeaderDataLength

-- | @- addTypeHeader:@
addTypeHeader :: (IsNSMutableDictionary nsMutableDictionary, IsNSString type_) => nsMutableDictionary -> type_ -> IO CInt
addTypeHeader nsMutableDictionary type_ =
  sendMessage nsMutableDictionary addTypeHeaderSelector (toNSString type_)

-- | @- addLengthHeader:@
addLengthHeader :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> CUInt -> IO CInt
addLengthHeader nsMutableDictionary length_ =
  sendMessage nsMutableDictionary addLengthHeaderSelector length_

-- | @- addTime4ByteHeader:@
addTime4ByteHeader :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> CUInt -> IO CInt
addTime4ByteHeader nsMutableDictionary time4Byte =
  sendMessage nsMutableDictionary addTime4ByteHeaderSelector time4Byte

-- | @- addCountHeader:@
addCountHeader :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> CUInt -> IO CInt
addCountHeader nsMutableDictionary inCount =
  sendMessage nsMutableDictionary addCountHeaderSelector inCount

-- | @- addDescriptionHeader:@
addDescriptionHeader :: (IsNSMutableDictionary nsMutableDictionary, IsNSString inDescriptionString) => nsMutableDictionary -> inDescriptionString -> IO CInt
addDescriptionHeader nsMutableDictionary inDescriptionString =
  sendMessage nsMutableDictionary addDescriptionHeaderSelector (toNSString inDescriptionString)

-- | @- addNameHeader:@
addNameHeader :: (IsNSMutableDictionary nsMutableDictionary, IsNSString inNameString) => nsMutableDictionary -> inNameString -> IO CInt
addNameHeader nsMutableDictionary inNameString =
  sendMessage nsMutableDictionary addNameHeaderSelector (toNSString inNameString)

-- | @- addUserDefinedHeader:length:@
addUserDefinedHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addUserDefinedHeader_length nsMutableDictionary inHeaderData inHeaderDataLength =
  sendMessage nsMutableDictionary addUserDefinedHeader_lengthSelector inHeaderData inHeaderDataLength

-- | @- addImageHandleHeader:@
addImageHandleHeader :: (IsNSMutableDictionary nsMutableDictionary, IsNSString type_) => nsMutableDictionary -> type_ -> IO CInt
addImageHandleHeader nsMutableDictionary type_ =
  sendMessage nsMutableDictionary addImageHandleHeaderSelector (toNSString type_)

-- | @- addImageDescriptorHeader:length:@
addImageDescriptorHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addImageDescriptorHeader_length nsMutableDictionary inHeaderData inHeaderDataLength =
  sendMessage nsMutableDictionary addImageDescriptorHeader_lengthSelector inHeaderData inHeaderDataLength

-- | @+ withOBEXHeadersData:headersDataSize:@
withOBEXHeadersData_headersDataSize :: Const (Ptr ()) -> CULong -> IO (Id NSMutableDictionary)
withOBEXHeadersData_headersDataSize inHeadersData inDataSize =
  do
    cls' <- getRequiredClass "NSMutableDictionary"
    sendClassMessage cls' withOBEXHeadersData_headersDataSizeSelector inHeadersData inDataSize

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dictionaryWithOBEXHeadersData:headersDataSize:@
dictionaryWithOBEXHeadersData_headersDataSizeSelector :: Selector '[Const (Ptr ()), CULong] (Id NSMutableDictionary)
dictionaryWithOBEXHeadersData_headersDataSizeSelector = mkSelector "dictionaryWithOBEXHeadersData:headersDataSize:"

-- | @Selector@ for @dictionaryWithOBEXHeadersData:@
dictionaryWithOBEXHeadersDataSelector :: Selector '[Id NSData] (Id NSMutableDictionary)
dictionaryWithOBEXHeadersDataSelector = mkSelector "dictionaryWithOBEXHeadersData:"

-- | @Selector@ for @getHeaderBytes@
getHeaderBytesSelector :: Selector '[] (Id NSMutableData)
getHeaderBytesSelector = mkSelector "getHeaderBytes"

-- | @Selector@ for @addTargetHeader:length:@
addTargetHeader_lengthSelector :: Selector '[Const (Ptr ()), CUInt] CInt
addTargetHeader_lengthSelector = mkSelector "addTargetHeader:length:"

-- | @Selector@ for @addHTTPHeader:length:@
addHTTPHeader_lengthSelector :: Selector '[Const (Ptr ()), CUInt] CInt
addHTTPHeader_lengthSelector = mkSelector "addHTTPHeader:length:"

-- | @Selector@ for @addBodyHeader:length:endOfBody:@
addBodyHeader_length_endOfBodySelector :: Selector '[Const (Ptr ()), CUInt, Bool] CInt
addBodyHeader_length_endOfBodySelector = mkSelector "addBodyHeader:length:endOfBody:"

-- | @Selector@ for @addWhoHeader:length:@
addWhoHeader_lengthSelector :: Selector '[Const (Ptr ()), CUInt] CInt
addWhoHeader_lengthSelector = mkSelector "addWhoHeader:length:"

-- | @Selector@ for @addConnectionIDHeader:length:@
addConnectionIDHeader_lengthSelector :: Selector '[Const (Ptr ()), CUInt] CInt
addConnectionIDHeader_lengthSelector = mkSelector "addConnectionIDHeader:length:"

-- | @Selector@ for @addApplicationParameterHeader:length:@
addApplicationParameterHeader_lengthSelector :: Selector '[Const (Ptr ()), CUInt] CInt
addApplicationParameterHeader_lengthSelector = mkSelector "addApplicationParameterHeader:length:"

-- | @Selector@ for @addByteSequenceHeader:length:@
addByteSequenceHeader_lengthSelector :: Selector '[Const (Ptr ()), CUInt] CInt
addByteSequenceHeader_lengthSelector = mkSelector "addByteSequenceHeader:length:"

-- | @Selector@ for @addObjectClassHeader:length:@
addObjectClassHeader_lengthSelector :: Selector '[Const (Ptr ()), CUInt] CInt
addObjectClassHeader_lengthSelector = mkSelector "addObjectClassHeader:length:"

-- | @Selector@ for @addAuthorizationChallengeHeader:length:@
addAuthorizationChallengeHeader_lengthSelector :: Selector '[Const (Ptr ()), CUInt] CInt
addAuthorizationChallengeHeader_lengthSelector = mkSelector "addAuthorizationChallengeHeader:length:"

-- | @Selector@ for @addAuthorizationResponseHeader:length:@
addAuthorizationResponseHeader_lengthSelector :: Selector '[Const (Ptr ()), CUInt] CInt
addAuthorizationResponseHeader_lengthSelector = mkSelector "addAuthorizationResponseHeader:length:"

-- | @Selector@ for @addTimeISOHeader:length:@
addTimeISOHeader_lengthSelector :: Selector '[Const (Ptr ()), CUInt] CInt
addTimeISOHeader_lengthSelector = mkSelector "addTimeISOHeader:length:"

-- | @Selector@ for @addTypeHeader:@
addTypeHeaderSelector :: Selector '[Id NSString] CInt
addTypeHeaderSelector = mkSelector "addTypeHeader:"

-- | @Selector@ for @addLengthHeader:@
addLengthHeaderSelector :: Selector '[CUInt] CInt
addLengthHeaderSelector = mkSelector "addLengthHeader:"

-- | @Selector@ for @addTime4ByteHeader:@
addTime4ByteHeaderSelector :: Selector '[CUInt] CInt
addTime4ByteHeaderSelector = mkSelector "addTime4ByteHeader:"

-- | @Selector@ for @addCountHeader:@
addCountHeaderSelector :: Selector '[CUInt] CInt
addCountHeaderSelector = mkSelector "addCountHeader:"

-- | @Selector@ for @addDescriptionHeader:@
addDescriptionHeaderSelector :: Selector '[Id NSString] CInt
addDescriptionHeaderSelector = mkSelector "addDescriptionHeader:"

-- | @Selector@ for @addNameHeader:@
addNameHeaderSelector :: Selector '[Id NSString] CInt
addNameHeaderSelector = mkSelector "addNameHeader:"

-- | @Selector@ for @addUserDefinedHeader:length:@
addUserDefinedHeader_lengthSelector :: Selector '[Const (Ptr ()), CUInt] CInt
addUserDefinedHeader_lengthSelector = mkSelector "addUserDefinedHeader:length:"

-- | @Selector@ for @addImageHandleHeader:@
addImageHandleHeaderSelector :: Selector '[Id NSString] CInt
addImageHandleHeaderSelector = mkSelector "addImageHandleHeader:"

-- | @Selector@ for @addImageDescriptorHeader:length:@
addImageDescriptorHeader_lengthSelector :: Selector '[Const (Ptr ()), CUInt] CInt
addImageDescriptorHeader_lengthSelector = mkSelector "addImageDescriptorHeader:length:"

-- | @Selector@ for @withOBEXHeadersData:headersDataSize:@
withOBEXHeadersData_headersDataSizeSelector :: Selector '[Const (Ptr ()), CULong] (Id NSMutableDictionary)
withOBEXHeadersData_headersDataSizeSelector = mkSelector "withOBEXHeadersData:headersDataSize:"

