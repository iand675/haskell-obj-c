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
  , dictionaryWithOBEXHeadersData_headersDataSizeSelector
  , dictionaryWithOBEXHeadersDataSelector
  , getHeaderBytesSelector
  , addTargetHeader_lengthSelector
  , addHTTPHeader_lengthSelector
  , addBodyHeader_length_endOfBodySelector
  , addWhoHeader_lengthSelector
  , addConnectionIDHeader_lengthSelector
  , addApplicationParameterHeader_lengthSelector
  , addByteSequenceHeader_lengthSelector
  , addObjectClassHeader_lengthSelector
  , addAuthorizationChallengeHeader_lengthSelector
  , addAuthorizationResponseHeader_lengthSelector
  , addTimeISOHeader_lengthSelector
  , addTypeHeaderSelector
  , addLengthHeaderSelector
  , addTime4ByteHeaderSelector
  , addCountHeaderSelector
  , addDescriptionHeaderSelector
  , addNameHeaderSelector
  , addUserDefinedHeader_lengthSelector
  , addImageHandleHeaderSelector
  , addImageDescriptorHeader_lengthSelector
  , withOBEXHeadersData_headersDataSizeSelector


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

-- | @+ dictionaryWithOBEXHeadersData:headersDataSize:@
dictionaryWithOBEXHeadersData_headersDataSize :: Const (Ptr ()) -> CULong -> IO (Id NSMutableDictionary)
dictionaryWithOBEXHeadersData_headersDataSize inHeadersData inDataSize =
  do
    cls' <- getRequiredClass "NSMutableDictionary"
    sendClassMsg cls' (mkSelector "dictionaryWithOBEXHeadersData:headersDataSize:") (retPtr retVoid) [argPtr (unConst inHeadersData), argCULong (fromIntegral inDataSize)] >>= retainedObject . castPtr

-- | @+ dictionaryWithOBEXHeadersData:@
dictionaryWithOBEXHeadersData :: IsNSData inHeadersData => inHeadersData -> IO (Id NSMutableDictionary)
dictionaryWithOBEXHeadersData inHeadersData =
  do
    cls' <- getRequiredClass "NSMutableDictionary"
    withObjCPtr inHeadersData $ \raw_inHeadersData ->
      sendClassMsg cls' (mkSelector "dictionaryWithOBEXHeadersData:") (retPtr retVoid) [argPtr (castPtr raw_inHeadersData :: Ptr ())] >>= retainedObject . castPtr

-- | @- getHeaderBytes@
getHeaderBytes :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> IO (Id NSMutableData)
getHeaderBytes nsMutableDictionary  =
  sendMsg nsMutableDictionary (mkSelector "getHeaderBytes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- addTargetHeader:length:@
addTargetHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addTargetHeader_length nsMutableDictionary  inHeaderData inHeaderDataLength =
  sendMsg nsMutableDictionary (mkSelector "addTargetHeader:length:") retCInt [argPtr (unConst inHeaderData), argCUInt (fromIntegral inHeaderDataLength)]

-- | @- addHTTPHeader:length:@
addHTTPHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addHTTPHeader_length nsMutableDictionary  inHeaderData inHeaderDataLength =
  sendMsg nsMutableDictionary (mkSelector "addHTTPHeader:length:") retCInt [argPtr (unConst inHeaderData), argCUInt (fromIntegral inHeaderDataLength)]

-- | @- addBodyHeader:length:endOfBody:@
addBodyHeader_length_endOfBody :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> Bool -> IO CInt
addBodyHeader_length_endOfBody nsMutableDictionary  inHeaderData inHeaderDataLength isEndOfBody =
  sendMsg nsMutableDictionary (mkSelector "addBodyHeader:length:endOfBody:") retCInt [argPtr (unConst inHeaderData), argCUInt (fromIntegral inHeaderDataLength), argCULong (if isEndOfBody then 1 else 0)]

-- | @- addWhoHeader:length:@
addWhoHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addWhoHeader_length nsMutableDictionary  inHeaderData inHeaderDataLength =
  sendMsg nsMutableDictionary (mkSelector "addWhoHeader:length:") retCInt [argPtr (unConst inHeaderData), argCUInt (fromIntegral inHeaderDataLength)]

-- | @- addConnectionIDHeader:length:@
addConnectionIDHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addConnectionIDHeader_length nsMutableDictionary  inHeaderData inHeaderDataLength =
  sendMsg nsMutableDictionary (mkSelector "addConnectionIDHeader:length:") retCInt [argPtr (unConst inHeaderData), argCUInt (fromIntegral inHeaderDataLength)]

-- | @- addApplicationParameterHeader:length:@
addApplicationParameterHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addApplicationParameterHeader_length nsMutableDictionary  inHeaderData inHeaderDataLength =
  sendMsg nsMutableDictionary (mkSelector "addApplicationParameterHeader:length:") retCInt [argPtr (unConst inHeaderData), argCUInt (fromIntegral inHeaderDataLength)]

-- | @- addByteSequenceHeader:length:@
addByteSequenceHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addByteSequenceHeader_length nsMutableDictionary  inHeaderData inHeaderDataLength =
  sendMsg nsMutableDictionary (mkSelector "addByteSequenceHeader:length:") retCInt [argPtr (unConst inHeaderData), argCUInt (fromIntegral inHeaderDataLength)]

-- | @- addObjectClassHeader:length:@
addObjectClassHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addObjectClassHeader_length nsMutableDictionary  inHeaderData inHeaderDataLength =
  sendMsg nsMutableDictionary (mkSelector "addObjectClassHeader:length:") retCInt [argPtr (unConst inHeaderData), argCUInt (fromIntegral inHeaderDataLength)]

-- | @- addAuthorizationChallengeHeader:length:@
addAuthorizationChallengeHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addAuthorizationChallengeHeader_length nsMutableDictionary  inHeaderData inHeaderDataLength =
  sendMsg nsMutableDictionary (mkSelector "addAuthorizationChallengeHeader:length:") retCInt [argPtr (unConst inHeaderData), argCUInt (fromIntegral inHeaderDataLength)]

-- | @- addAuthorizationResponseHeader:length:@
addAuthorizationResponseHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addAuthorizationResponseHeader_length nsMutableDictionary  inHeaderData inHeaderDataLength =
  sendMsg nsMutableDictionary (mkSelector "addAuthorizationResponseHeader:length:") retCInt [argPtr (unConst inHeaderData), argCUInt (fromIntegral inHeaderDataLength)]

-- | @- addTimeISOHeader:length:@
addTimeISOHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addTimeISOHeader_length nsMutableDictionary  inHeaderData inHeaderDataLength =
  sendMsg nsMutableDictionary (mkSelector "addTimeISOHeader:length:") retCInt [argPtr (unConst inHeaderData), argCUInt (fromIntegral inHeaderDataLength)]

-- | @- addTypeHeader:@
addTypeHeader :: (IsNSMutableDictionary nsMutableDictionary, IsNSString type_) => nsMutableDictionary -> type_ -> IO CInt
addTypeHeader nsMutableDictionary  type_ =
withObjCPtr type_ $ \raw_type_ ->
    sendMsg nsMutableDictionary (mkSelector "addTypeHeader:") retCInt [argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- addLengthHeader:@
addLengthHeader :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> CUInt -> IO CInt
addLengthHeader nsMutableDictionary  length_ =
  sendMsg nsMutableDictionary (mkSelector "addLengthHeader:") retCInt [argCUInt (fromIntegral length_)]

-- | @- addTime4ByteHeader:@
addTime4ByteHeader :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> CUInt -> IO CInt
addTime4ByteHeader nsMutableDictionary  time4Byte =
  sendMsg nsMutableDictionary (mkSelector "addTime4ByteHeader:") retCInt [argCUInt (fromIntegral time4Byte)]

-- | @- addCountHeader:@
addCountHeader :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> CUInt -> IO CInt
addCountHeader nsMutableDictionary  inCount =
  sendMsg nsMutableDictionary (mkSelector "addCountHeader:") retCInt [argCUInt (fromIntegral inCount)]

-- | @- addDescriptionHeader:@
addDescriptionHeader :: (IsNSMutableDictionary nsMutableDictionary, IsNSString inDescriptionString) => nsMutableDictionary -> inDescriptionString -> IO CInt
addDescriptionHeader nsMutableDictionary  inDescriptionString =
withObjCPtr inDescriptionString $ \raw_inDescriptionString ->
    sendMsg nsMutableDictionary (mkSelector "addDescriptionHeader:") retCInt [argPtr (castPtr raw_inDescriptionString :: Ptr ())]

-- | @- addNameHeader:@
addNameHeader :: (IsNSMutableDictionary nsMutableDictionary, IsNSString inNameString) => nsMutableDictionary -> inNameString -> IO CInt
addNameHeader nsMutableDictionary  inNameString =
withObjCPtr inNameString $ \raw_inNameString ->
    sendMsg nsMutableDictionary (mkSelector "addNameHeader:") retCInt [argPtr (castPtr raw_inNameString :: Ptr ())]

-- | @- addUserDefinedHeader:length:@
addUserDefinedHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addUserDefinedHeader_length nsMutableDictionary  inHeaderData inHeaderDataLength =
  sendMsg nsMutableDictionary (mkSelector "addUserDefinedHeader:length:") retCInt [argPtr (unConst inHeaderData), argCUInt (fromIntegral inHeaderDataLength)]

-- | @- addImageHandleHeader:@
addImageHandleHeader :: (IsNSMutableDictionary nsMutableDictionary, IsNSString type_) => nsMutableDictionary -> type_ -> IO CInt
addImageHandleHeader nsMutableDictionary  type_ =
withObjCPtr type_ $ \raw_type_ ->
    sendMsg nsMutableDictionary (mkSelector "addImageHandleHeader:") retCInt [argPtr (castPtr raw_type_ :: Ptr ())]

-- | @- addImageDescriptorHeader:length:@
addImageDescriptorHeader_length :: IsNSMutableDictionary nsMutableDictionary => nsMutableDictionary -> Const (Ptr ()) -> CUInt -> IO CInt
addImageDescriptorHeader_length nsMutableDictionary  inHeaderData inHeaderDataLength =
  sendMsg nsMutableDictionary (mkSelector "addImageDescriptorHeader:length:") retCInt [argPtr (unConst inHeaderData), argCUInt (fromIntegral inHeaderDataLength)]

-- | @+ withOBEXHeadersData:headersDataSize:@
withOBEXHeadersData_headersDataSize :: Const (Ptr ()) -> CULong -> IO (Id NSMutableDictionary)
withOBEXHeadersData_headersDataSize inHeadersData inDataSize =
  do
    cls' <- getRequiredClass "NSMutableDictionary"
    sendClassMsg cls' (mkSelector "withOBEXHeadersData:headersDataSize:") (retPtr retVoid) [argPtr (unConst inHeadersData), argCULong (fromIntegral inDataSize)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dictionaryWithOBEXHeadersData:headersDataSize:@
dictionaryWithOBEXHeadersData_headersDataSizeSelector :: Selector
dictionaryWithOBEXHeadersData_headersDataSizeSelector = mkSelector "dictionaryWithOBEXHeadersData:headersDataSize:"

-- | @Selector@ for @dictionaryWithOBEXHeadersData:@
dictionaryWithOBEXHeadersDataSelector :: Selector
dictionaryWithOBEXHeadersDataSelector = mkSelector "dictionaryWithOBEXHeadersData:"

-- | @Selector@ for @getHeaderBytes@
getHeaderBytesSelector :: Selector
getHeaderBytesSelector = mkSelector "getHeaderBytes"

-- | @Selector@ for @addTargetHeader:length:@
addTargetHeader_lengthSelector :: Selector
addTargetHeader_lengthSelector = mkSelector "addTargetHeader:length:"

-- | @Selector@ for @addHTTPHeader:length:@
addHTTPHeader_lengthSelector :: Selector
addHTTPHeader_lengthSelector = mkSelector "addHTTPHeader:length:"

-- | @Selector@ for @addBodyHeader:length:endOfBody:@
addBodyHeader_length_endOfBodySelector :: Selector
addBodyHeader_length_endOfBodySelector = mkSelector "addBodyHeader:length:endOfBody:"

-- | @Selector@ for @addWhoHeader:length:@
addWhoHeader_lengthSelector :: Selector
addWhoHeader_lengthSelector = mkSelector "addWhoHeader:length:"

-- | @Selector@ for @addConnectionIDHeader:length:@
addConnectionIDHeader_lengthSelector :: Selector
addConnectionIDHeader_lengthSelector = mkSelector "addConnectionIDHeader:length:"

-- | @Selector@ for @addApplicationParameterHeader:length:@
addApplicationParameterHeader_lengthSelector :: Selector
addApplicationParameterHeader_lengthSelector = mkSelector "addApplicationParameterHeader:length:"

-- | @Selector@ for @addByteSequenceHeader:length:@
addByteSequenceHeader_lengthSelector :: Selector
addByteSequenceHeader_lengthSelector = mkSelector "addByteSequenceHeader:length:"

-- | @Selector@ for @addObjectClassHeader:length:@
addObjectClassHeader_lengthSelector :: Selector
addObjectClassHeader_lengthSelector = mkSelector "addObjectClassHeader:length:"

-- | @Selector@ for @addAuthorizationChallengeHeader:length:@
addAuthorizationChallengeHeader_lengthSelector :: Selector
addAuthorizationChallengeHeader_lengthSelector = mkSelector "addAuthorizationChallengeHeader:length:"

-- | @Selector@ for @addAuthorizationResponseHeader:length:@
addAuthorizationResponseHeader_lengthSelector :: Selector
addAuthorizationResponseHeader_lengthSelector = mkSelector "addAuthorizationResponseHeader:length:"

-- | @Selector@ for @addTimeISOHeader:length:@
addTimeISOHeader_lengthSelector :: Selector
addTimeISOHeader_lengthSelector = mkSelector "addTimeISOHeader:length:"

-- | @Selector@ for @addTypeHeader:@
addTypeHeaderSelector :: Selector
addTypeHeaderSelector = mkSelector "addTypeHeader:"

-- | @Selector@ for @addLengthHeader:@
addLengthHeaderSelector :: Selector
addLengthHeaderSelector = mkSelector "addLengthHeader:"

-- | @Selector@ for @addTime4ByteHeader:@
addTime4ByteHeaderSelector :: Selector
addTime4ByteHeaderSelector = mkSelector "addTime4ByteHeader:"

-- | @Selector@ for @addCountHeader:@
addCountHeaderSelector :: Selector
addCountHeaderSelector = mkSelector "addCountHeader:"

-- | @Selector@ for @addDescriptionHeader:@
addDescriptionHeaderSelector :: Selector
addDescriptionHeaderSelector = mkSelector "addDescriptionHeader:"

-- | @Selector@ for @addNameHeader:@
addNameHeaderSelector :: Selector
addNameHeaderSelector = mkSelector "addNameHeader:"

-- | @Selector@ for @addUserDefinedHeader:length:@
addUserDefinedHeader_lengthSelector :: Selector
addUserDefinedHeader_lengthSelector = mkSelector "addUserDefinedHeader:length:"

-- | @Selector@ for @addImageHandleHeader:@
addImageHandleHeaderSelector :: Selector
addImageHandleHeaderSelector = mkSelector "addImageHandleHeader:"

-- | @Selector@ for @addImageDescriptorHeader:length:@
addImageDescriptorHeader_lengthSelector :: Selector
addImageDescriptorHeader_lengthSelector = mkSelector "addImageDescriptorHeader:length:"

-- | @Selector@ for @withOBEXHeadersData:headersDataSize:@
withOBEXHeadersData_headersDataSizeSelector :: Selector
withOBEXHeadersData_headersDataSizeSelector = mkSelector "withOBEXHeadersData:headersDataSize:"

