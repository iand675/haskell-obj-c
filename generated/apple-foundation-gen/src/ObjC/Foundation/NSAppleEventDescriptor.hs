{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAppleEventDescriptor@.
module ObjC.Foundation.NSAppleEventDescriptor
  ( NSAppleEventDescriptor
  , IsNSAppleEventDescriptor(..)
  , nullDescriptor
  , descriptorWithDescriptorType_bytes_length
  , descriptorWithDescriptorType_data
  , descriptorWithBoolean
  , descriptorWithEnumCode
  , descriptorWithInt32
  , descriptorWithDouble
  , descriptorWithTypeCode
  , descriptorWithString
  , descriptorWithDate
  , descriptorWithFileURL
  , appleEventWithEventClass_eventID_targetDescriptor_returnID_transactionID
  , listDescriptor
  , recordDescriptor
  , currentProcessDescriptor
  , descriptorWithProcessIdentifier
  , descriptorWithBundleIdentifier
  , descriptorWithApplicationURL
  , initWithAEDescNoCopy
  , initWithDescriptorType_bytes_length
  , initWithDescriptorType_data
  , initWithEventClass_eventID_targetDescriptor_returnID_transactionID
  , initListDescriptor
  , initRecordDescriptor
  , setParamDescriptor_forKeyword
  , paramDescriptorForKeyword
  , removeParamDescriptorWithKeyword
  , setAttributeDescriptor_forKeyword
  , attributeDescriptorForKeyword
  , sendEventWithOptions_timeout_error
  , insertDescriptor_atIndex
  , descriptorAtIndex
  , removeDescriptorAtIndex
  , setDescriptor_forKeyword
  , descriptorForKeyword
  , removeDescriptorWithKeyword
  , keywordForDescriptorAtIndex
  , coerceToDescriptorType
  , aeDesc
  , descriptorType
  , data_
  , booleanValue
  , enumCodeValue
  , int32Value
  , doubleValue
  , typeCodeValue
  , stringValue
  , dateValue
  , fileURLValue
  , eventClass
  , eventID
  , returnID
  , transactionID
  , isRecordDescriptor
  , numberOfItems
  , nullDescriptorSelector
  , descriptorWithDescriptorType_bytes_lengthSelector
  , descriptorWithDescriptorType_dataSelector
  , descriptorWithBooleanSelector
  , descriptorWithEnumCodeSelector
  , descriptorWithInt32Selector
  , descriptorWithDoubleSelector
  , descriptorWithTypeCodeSelector
  , descriptorWithStringSelector
  , descriptorWithDateSelector
  , descriptorWithFileURLSelector
  , appleEventWithEventClass_eventID_targetDescriptor_returnID_transactionIDSelector
  , listDescriptorSelector
  , recordDescriptorSelector
  , currentProcessDescriptorSelector
  , descriptorWithProcessIdentifierSelector
  , descriptorWithBundleIdentifierSelector
  , descriptorWithApplicationURLSelector
  , initWithAEDescNoCopySelector
  , initWithDescriptorType_bytes_lengthSelector
  , initWithDescriptorType_dataSelector
  , initWithEventClass_eventID_targetDescriptor_returnID_transactionIDSelector
  , initListDescriptorSelector
  , initRecordDescriptorSelector
  , setParamDescriptor_forKeywordSelector
  , paramDescriptorForKeywordSelector
  , removeParamDescriptorWithKeywordSelector
  , setAttributeDescriptor_forKeywordSelector
  , attributeDescriptorForKeywordSelector
  , sendEventWithOptions_timeout_errorSelector
  , insertDescriptor_atIndexSelector
  , descriptorAtIndexSelector
  , removeDescriptorAtIndexSelector
  , setDescriptor_forKeywordSelector
  , descriptorForKeywordSelector
  , removeDescriptorWithKeywordSelector
  , keywordForDescriptorAtIndexSelector
  , coerceToDescriptorTypeSelector
  , aeDescSelector
  , descriptorTypeSelector
  , dataSelector
  , booleanValueSelector
  , enumCodeValueSelector
  , int32ValueSelector
  , doubleValueSelector
  , typeCodeValueSelector
  , stringValueSelector
  , dateValueSelector
  , fileURLValueSelector
  , eventClassSelector
  , eventIDSelector
  , returnIDSelector
  , transactionIDSelector
  , isRecordDescriptorSelector
  , numberOfItemsSelector

  -- * Enum types
  , NSAppleEventSendOptions(NSAppleEventSendOptions)
  , pattern NSAppleEventSendNoReply
  , pattern NSAppleEventSendQueueReply
  , pattern NSAppleEventSendWaitForReply
  , pattern NSAppleEventSendNeverInteract
  , pattern NSAppleEventSendCanInteract
  , pattern NSAppleEventSendAlwaysInteract
  , pattern NSAppleEventSendCanSwitchLayer
  , pattern NSAppleEventSendDontRecord
  , pattern NSAppleEventSendDontExecute
  , pattern NSAppleEventSendDontAnnotate
  , pattern NSAppleEventSendDefaultOptions

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @+ nullDescriptor@
nullDescriptor :: IO (Id NSAppleEventDescriptor)
nullDescriptor  =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMsg cls' (mkSelector "nullDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ descriptorWithDescriptorType:bytes:length:@
descriptorWithDescriptorType_bytes_length :: CUInt -> Const (Ptr ()) -> CULong -> IO (Id NSAppleEventDescriptor)
descriptorWithDescriptorType_bytes_length descriptorType bytes byteCount =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithDescriptorType:bytes:length:") (retPtr retVoid) [argCUInt descriptorType, argPtr (unConst bytes), argCULong byteCount] >>= retainedObject . castPtr

-- | @+ descriptorWithDescriptorType:data:@
descriptorWithDescriptorType_data :: IsNSData data_ => CUInt -> data_ -> IO (Id NSAppleEventDescriptor)
descriptorWithDescriptorType_data descriptorType data_ =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    withObjCPtr data_ $ \raw_data_ ->
      sendClassMsg cls' (mkSelector "descriptorWithDescriptorType:data:") (retPtr retVoid) [argCUInt descriptorType, argPtr (castPtr raw_data_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ descriptorWithBoolean:@
descriptorWithBoolean :: CUChar -> IO (Id NSAppleEventDescriptor)
descriptorWithBoolean boolean =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithBoolean:") (retPtr retVoid) [argCUChar boolean] >>= retainedObject . castPtr

-- | @+ descriptorWithEnumCode:@
descriptorWithEnumCode :: CUInt -> IO (Id NSAppleEventDescriptor)
descriptorWithEnumCode enumerator =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithEnumCode:") (retPtr retVoid) [argCUInt enumerator] >>= retainedObject . castPtr

-- | @+ descriptorWithInt32:@
descriptorWithInt32 :: CInt -> IO (Id NSAppleEventDescriptor)
descriptorWithInt32 signedInt =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithInt32:") (retPtr retVoid) [argCInt signedInt] >>= retainedObject . castPtr

-- | @+ descriptorWithDouble:@
descriptorWithDouble :: CDouble -> IO (Id NSAppleEventDescriptor)
descriptorWithDouble doubleValue =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithDouble:") (retPtr retVoid) [argCDouble doubleValue] >>= retainedObject . castPtr

-- | @+ descriptorWithTypeCode:@
descriptorWithTypeCode :: CUInt -> IO (Id NSAppleEventDescriptor)
descriptorWithTypeCode typeCode =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithTypeCode:") (retPtr retVoid) [argCUInt typeCode] >>= retainedObject . castPtr

-- | @+ descriptorWithString:@
descriptorWithString :: IsNSString string => string -> IO (Id NSAppleEventDescriptor)
descriptorWithString string =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "descriptorWithString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @+ descriptorWithDate:@
descriptorWithDate :: IsNSDate date => date -> IO (Id NSAppleEventDescriptor)
descriptorWithDate date =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    withObjCPtr date $ \raw_date ->
      sendClassMsg cls' (mkSelector "descriptorWithDate:") (retPtr retVoid) [argPtr (castPtr raw_date :: Ptr ())] >>= retainedObject . castPtr

-- | @+ descriptorWithFileURL:@
descriptorWithFileURL :: IsNSURL fileURL => fileURL -> IO (Id NSAppleEventDescriptor)
descriptorWithFileURL fileURL =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    withObjCPtr fileURL $ \raw_fileURL ->
      sendClassMsg cls' (mkSelector "descriptorWithFileURL:") (retPtr retVoid) [argPtr (castPtr raw_fileURL :: Ptr ())] >>= retainedObject . castPtr

-- | @+ appleEventWithEventClass:eventID:targetDescriptor:returnID:transactionID:@
appleEventWithEventClass_eventID_targetDescriptor_returnID_transactionID :: IsNSAppleEventDescriptor targetDescriptor => CUInt -> CUInt -> targetDescriptor -> CShort -> CInt -> IO (Id NSAppleEventDescriptor)
appleEventWithEventClass_eventID_targetDescriptor_returnID_transactionID eventClass eventID targetDescriptor returnID transactionID =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    withObjCPtr targetDescriptor $ \raw_targetDescriptor ->
      sendClassMsg cls' (mkSelector "appleEventWithEventClass:eventID:targetDescriptor:returnID:transactionID:") (retPtr retVoid) [argCUInt eventClass, argCUInt eventID, argPtr (castPtr raw_targetDescriptor :: Ptr ()), argCInt (fromIntegral returnID), argCInt transactionID] >>= retainedObject . castPtr

-- | @+ listDescriptor@
listDescriptor :: IO (Id NSAppleEventDescriptor)
listDescriptor  =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMsg cls' (mkSelector "listDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ recordDescriptor@
recordDescriptor :: IO (Id NSAppleEventDescriptor)
recordDescriptor  =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMsg cls' (mkSelector "recordDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ currentProcessDescriptor@
currentProcessDescriptor :: IO (Id NSAppleEventDescriptor)
currentProcessDescriptor  =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMsg cls' (mkSelector "currentProcessDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ descriptorWithProcessIdentifier:@
descriptorWithProcessIdentifier :: CInt -> IO (Id NSAppleEventDescriptor)
descriptorWithProcessIdentifier processIdentifier =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMsg cls' (mkSelector "descriptorWithProcessIdentifier:") (retPtr retVoid) [argCInt processIdentifier] >>= retainedObject . castPtr

-- | @+ descriptorWithBundleIdentifier:@
descriptorWithBundleIdentifier :: IsNSString bundleIdentifier => bundleIdentifier -> IO (Id NSAppleEventDescriptor)
descriptorWithBundleIdentifier bundleIdentifier =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    withObjCPtr bundleIdentifier $ \raw_bundleIdentifier ->
      sendClassMsg cls' (mkSelector "descriptorWithBundleIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_bundleIdentifier :: Ptr ())] >>= retainedObject . castPtr

-- | @+ descriptorWithApplicationURL:@
descriptorWithApplicationURL :: IsNSURL applicationURL => applicationURL -> IO (Id NSAppleEventDescriptor)
descriptorWithApplicationURL applicationURL =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    withObjCPtr applicationURL $ \raw_applicationURL ->
      sendClassMsg cls' (mkSelector "descriptorWithApplicationURL:") (retPtr retVoid) [argPtr (castPtr raw_applicationURL :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithAEDescNoCopy:@
initWithAEDescNoCopy :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> Const RawId -> IO (Id NSAppleEventDescriptor)
initWithAEDescNoCopy nsAppleEventDescriptor  aeDesc =
    sendMsg nsAppleEventDescriptor (mkSelector "initWithAEDescNoCopy:") (retPtr retVoid) [argPtr (castPtr (unRawId (unConst aeDesc)) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDescriptorType:bytes:length:@
initWithDescriptorType_bytes_length :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> CUInt -> Const (Ptr ()) -> CULong -> IO (Id NSAppleEventDescriptor)
initWithDescriptorType_bytes_length nsAppleEventDescriptor  descriptorType bytes byteCount =
    sendMsg nsAppleEventDescriptor (mkSelector "initWithDescriptorType:bytes:length:") (retPtr retVoid) [argCUInt descriptorType, argPtr (unConst bytes), argCULong byteCount] >>= ownedObject . castPtr

-- | @- initWithDescriptorType:data:@
initWithDescriptorType_data :: (IsNSAppleEventDescriptor nsAppleEventDescriptor, IsNSData data_) => nsAppleEventDescriptor -> CUInt -> data_ -> IO (Id NSAppleEventDescriptor)
initWithDescriptorType_data nsAppleEventDescriptor  descriptorType data_ =
  withObjCPtr data_ $ \raw_data_ ->
      sendMsg nsAppleEventDescriptor (mkSelector "initWithDescriptorType:data:") (retPtr retVoid) [argCUInt descriptorType, argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithEventClass:eventID:targetDescriptor:returnID:transactionID:@
initWithEventClass_eventID_targetDescriptor_returnID_transactionID :: (IsNSAppleEventDescriptor nsAppleEventDescriptor, IsNSAppleEventDescriptor targetDescriptor) => nsAppleEventDescriptor -> CUInt -> CUInt -> targetDescriptor -> CShort -> CInt -> IO (Id NSAppleEventDescriptor)
initWithEventClass_eventID_targetDescriptor_returnID_transactionID nsAppleEventDescriptor  eventClass eventID targetDescriptor returnID transactionID =
  withObjCPtr targetDescriptor $ \raw_targetDescriptor ->
      sendMsg nsAppleEventDescriptor (mkSelector "initWithEventClass:eventID:targetDescriptor:returnID:transactionID:") (retPtr retVoid) [argCUInt eventClass, argCUInt eventID, argPtr (castPtr raw_targetDescriptor :: Ptr ()), argCInt (fromIntegral returnID), argCInt transactionID] >>= ownedObject . castPtr

-- | @- initListDescriptor@
initListDescriptor :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO (Id NSAppleEventDescriptor)
initListDescriptor nsAppleEventDescriptor  =
    sendMsg nsAppleEventDescriptor (mkSelector "initListDescriptor") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initRecordDescriptor@
initRecordDescriptor :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO (Id NSAppleEventDescriptor)
initRecordDescriptor nsAppleEventDescriptor  =
    sendMsg nsAppleEventDescriptor (mkSelector "initRecordDescriptor") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- setParamDescriptor:forKeyword:@
setParamDescriptor_forKeyword :: (IsNSAppleEventDescriptor nsAppleEventDescriptor, IsNSAppleEventDescriptor descriptor) => nsAppleEventDescriptor -> descriptor -> CUInt -> IO ()
setParamDescriptor_forKeyword nsAppleEventDescriptor  descriptor keyword =
  withObjCPtr descriptor $ \raw_descriptor ->
      sendMsg nsAppleEventDescriptor (mkSelector "setParamDescriptor:forKeyword:") retVoid [argPtr (castPtr raw_descriptor :: Ptr ()), argCUInt keyword]

-- | @- paramDescriptorForKeyword:@
paramDescriptorForKeyword :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> CUInt -> IO (Id NSAppleEventDescriptor)
paramDescriptorForKeyword nsAppleEventDescriptor  keyword =
    sendMsg nsAppleEventDescriptor (mkSelector "paramDescriptorForKeyword:") (retPtr retVoid) [argCUInt keyword] >>= retainedObject . castPtr

-- | @- removeParamDescriptorWithKeyword:@
removeParamDescriptorWithKeyword :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> CUInt -> IO ()
removeParamDescriptorWithKeyword nsAppleEventDescriptor  keyword =
    sendMsg nsAppleEventDescriptor (mkSelector "removeParamDescriptorWithKeyword:") retVoid [argCUInt keyword]

-- | @- setAttributeDescriptor:forKeyword:@
setAttributeDescriptor_forKeyword :: (IsNSAppleEventDescriptor nsAppleEventDescriptor, IsNSAppleEventDescriptor descriptor) => nsAppleEventDescriptor -> descriptor -> CUInt -> IO ()
setAttributeDescriptor_forKeyword nsAppleEventDescriptor  descriptor keyword =
  withObjCPtr descriptor $ \raw_descriptor ->
      sendMsg nsAppleEventDescriptor (mkSelector "setAttributeDescriptor:forKeyword:") retVoid [argPtr (castPtr raw_descriptor :: Ptr ()), argCUInt keyword]

-- | @- attributeDescriptorForKeyword:@
attributeDescriptorForKeyword :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> CUInt -> IO (Id NSAppleEventDescriptor)
attributeDescriptorForKeyword nsAppleEventDescriptor  keyword =
    sendMsg nsAppleEventDescriptor (mkSelector "attributeDescriptorForKeyword:") (retPtr retVoid) [argCUInt keyword] >>= retainedObject . castPtr

-- | @- sendEventWithOptions:timeout:error:@
sendEventWithOptions_timeout_error :: (IsNSAppleEventDescriptor nsAppleEventDescriptor, IsNSError error_) => nsAppleEventDescriptor -> NSAppleEventSendOptions -> CDouble -> error_ -> IO (Id NSAppleEventDescriptor)
sendEventWithOptions_timeout_error nsAppleEventDescriptor  sendOptions timeoutInSeconds error_ =
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsAppleEventDescriptor (mkSelector "sendEventWithOptions:timeout:error:") (retPtr retVoid) [argCULong (coerce sendOptions), argCDouble timeoutInSeconds, argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- insertDescriptor:atIndex:@
insertDescriptor_atIndex :: (IsNSAppleEventDescriptor nsAppleEventDescriptor, IsNSAppleEventDescriptor descriptor) => nsAppleEventDescriptor -> descriptor -> CLong -> IO ()
insertDescriptor_atIndex nsAppleEventDescriptor  descriptor index =
  withObjCPtr descriptor $ \raw_descriptor ->
      sendMsg nsAppleEventDescriptor (mkSelector "insertDescriptor:atIndex:") retVoid [argPtr (castPtr raw_descriptor :: Ptr ()), argCLong index]

-- | @- descriptorAtIndex:@
descriptorAtIndex :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> CLong -> IO (Id NSAppleEventDescriptor)
descriptorAtIndex nsAppleEventDescriptor  index =
    sendMsg nsAppleEventDescriptor (mkSelector "descriptorAtIndex:") (retPtr retVoid) [argCLong index] >>= retainedObject . castPtr

-- | @- removeDescriptorAtIndex:@
removeDescriptorAtIndex :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> CLong -> IO ()
removeDescriptorAtIndex nsAppleEventDescriptor  index =
    sendMsg nsAppleEventDescriptor (mkSelector "removeDescriptorAtIndex:") retVoid [argCLong index]

-- | @- setDescriptor:forKeyword:@
setDescriptor_forKeyword :: (IsNSAppleEventDescriptor nsAppleEventDescriptor, IsNSAppleEventDescriptor descriptor) => nsAppleEventDescriptor -> descriptor -> CUInt -> IO ()
setDescriptor_forKeyword nsAppleEventDescriptor  descriptor keyword =
  withObjCPtr descriptor $ \raw_descriptor ->
      sendMsg nsAppleEventDescriptor (mkSelector "setDescriptor:forKeyword:") retVoid [argPtr (castPtr raw_descriptor :: Ptr ()), argCUInt keyword]

-- | @- descriptorForKeyword:@
descriptorForKeyword :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> CUInt -> IO (Id NSAppleEventDescriptor)
descriptorForKeyword nsAppleEventDescriptor  keyword =
    sendMsg nsAppleEventDescriptor (mkSelector "descriptorForKeyword:") (retPtr retVoid) [argCUInt keyword] >>= retainedObject . castPtr

-- | @- removeDescriptorWithKeyword:@
removeDescriptorWithKeyword :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> CUInt -> IO ()
removeDescriptorWithKeyword nsAppleEventDescriptor  keyword =
    sendMsg nsAppleEventDescriptor (mkSelector "removeDescriptorWithKeyword:") retVoid [argCUInt keyword]

-- | @- keywordForDescriptorAtIndex:@
keywordForDescriptorAtIndex :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> CLong -> IO CUInt
keywordForDescriptorAtIndex nsAppleEventDescriptor  index =
    sendMsg nsAppleEventDescriptor (mkSelector "keywordForDescriptorAtIndex:") retCUInt [argCLong index]

-- | @- coerceToDescriptorType:@
coerceToDescriptorType :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> CUInt -> IO (Id NSAppleEventDescriptor)
coerceToDescriptorType nsAppleEventDescriptor  descriptorType =
    sendMsg nsAppleEventDescriptor (mkSelector "coerceToDescriptorType:") (retPtr retVoid) [argCUInt descriptorType] >>= retainedObject . castPtr

-- | @- aeDesc@
aeDesc :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO RawId
aeDesc nsAppleEventDescriptor  =
    fmap (RawId . castPtr) $ sendMsg nsAppleEventDescriptor (mkSelector "aeDesc") (retPtr retVoid) []

-- | @- descriptorType@
descriptorType :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO CUInt
descriptorType nsAppleEventDescriptor  =
    sendMsg nsAppleEventDescriptor (mkSelector "descriptorType") retCUInt []

-- | @- data@
data_ :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO (Id NSData)
data_ nsAppleEventDescriptor  =
    sendMsg nsAppleEventDescriptor (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- booleanValue@
booleanValue :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO CUChar
booleanValue nsAppleEventDescriptor  =
    sendMsg nsAppleEventDescriptor (mkSelector "booleanValue") retCUChar []

-- | @- enumCodeValue@
enumCodeValue :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO CUInt
enumCodeValue nsAppleEventDescriptor  =
    sendMsg nsAppleEventDescriptor (mkSelector "enumCodeValue") retCUInt []

-- | @- int32Value@
int32Value :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO CInt
int32Value nsAppleEventDescriptor  =
    sendMsg nsAppleEventDescriptor (mkSelector "int32Value") retCInt []

-- | @- doubleValue@
doubleValue :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO CDouble
doubleValue nsAppleEventDescriptor  =
    sendMsg nsAppleEventDescriptor (mkSelector "doubleValue") retCDouble []

-- | @- typeCodeValue@
typeCodeValue :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO CUInt
typeCodeValue nsAppleEventDescriptor  =
    sendMsg nsAppleEventDescriptor (mkSelector "typeCodeValue") retCUInt []

-- | @- stringValue@
stringValue :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO (Id NSString)
stringValue nsAppleEventDescriptor  =
    sendMsg nsAppleEventDescriptor (mkSelector "stringValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dateValue@
dateValue :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO (Id NSDate)
dateValue nsAppleEventDescriptor  =
    sendMsg nsAppleEventDescriptor (mkSelector "dateValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fileURLValue@
fileURLValue :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO (Id NSURL)
fileURLValue nsAppleEventDescriptor  =
    sendMsg nsAppleEventDescriptor (mkSelector "fileURLValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- eventClass@
eventClass :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO CUInt
eventClass nsAppleEventDescriptor  =
    sendMsg nsAppleEventDescriptor (mkSelector "eventClass") retCUInt []

-- | @- eventID@
eventID :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO CUInt
eventID nsAppleEventDescriptor  =
    sendMsg nsAppleEventDescriptor (mkSelector "eventID") retCUInt []

-- | @- returnID@
returnID :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO CShort
returnID nsAppleEventDescriptor  =
    fmap fromIntegral $ sendMsg nsAppleEventDescriptor (mkSelector "returnID") retCInt []

-- | @- transactionID@
transactionID :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO CInt
transactionID nsAppleEventDescriptor  =
    sendMsg nsAppleEventDescriptor (mkSelector "transactionID") retCInt []

-- | @- isRecordDescriptor@
isRecordDescriptor :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO Bool
isRecordDescriptor nsAppleEventDescriptor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsAppleEventDescriptor (mkSelector "isRecordDescriptor") retCULong []

-- | @- numberOfItems@
numberOfItems :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO CLong
numberOfItems nsAppleEventDescriptor  =
    sendMsg nsAppleEventDescriptor (mkSelector "numberOfItems") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nullDescriptor@
nullDescriptorSelector :: Selector
nullDescriptorSelector = mkSelector "nullDescriptor"

-- | @Selector@ for @descriptorWithDescriptorType:bytes:length:@
descriptorWithDescriptorType_bytes_lengthSelector :: Selector
descriptorWithDescriptorType_bytes_lengthSelector = mkSelector "descriptorWithDescriptorType:bytes:length:"

-- | @Selector@ for @descriptorWithDescriptorType:data:@
descriptorWithDescriptorType_dataSelector :: Selector
descriptorWithDescriptorType_dataSelector = mkSelector "descriptorWithDescriptorType:data:"

-- | @Selector@ for @descriptorWithBoolean:@
descriptorWithBooleanSelector :: Selector
descriptorWithBooleanSelector = mkSelector "descriptorWithBoolean:"

-- | @Selector@ for @descriptorWithEnumCode:@
descriptorWithEnumCodeSelector :: Selector
descriptorWithEnumCodeSelector = mkSelector "descriptorWithEnumCode:"

-- | @Selector@ for @descriptorWithInt32:@
descriptorWithInt32Selector :: Selector
descriptorWithInt32Selector = mkSelector "descriptorWithInt32:"

-- | @Selector@ for @descriptorWithDouble:@
descriptorWithDoubleSelector :: Selector
descriptorWithDoubleSelector = mkSelector "descriptorWithDouble:"

-- | @Selector@ for @descriptorWithTypeCode:@
descriptorWithTypeCodeSelector :: Selector
descriptorWithTypeCodeSelector = mkSelector "descriptorWithTypeCode:"

-- | @Selector@ for @descriptorWithString:@
descriptorWithStringSelector :: Selector
descriptorWithStringSelector = mkSelector "descriptorWithString:"

-- | @Selector@ for @descriptorWithDate:@
descriptorWithDateSelector :: Selector
descriptorWithDateSelector = mkSelector "descriptorWithDate:"

-- | @Selector@ for @descriptorWithFileURL:@
descriptorWithFileURLSelector :: Selector
descriptorWithFileURLSelector = mkSelector "descriptorWithFileURL:"

-- | @Selector@ for @appleEventWithEventClass:eventID:targetDescriptor:returnID:transactionID:@
appleEventWithEventClass_eventID_targetDescriptor_returnID_transactionIDSelector :: Selector
appleEventWithEventClass_eventID_targetDescriptor_returnID_transactionIDSelector = mkSelector "appleEventWithEventClass:eventID:targetDescriptor:returnID:transactionID:"

-- | @Selector@ for @listDescriptor@
listDescriptorSelector :: Selector
listDescriptorSelector = mkSelector "listDescriptor"

-- | @Selector@ for @recordDescriptor@
recordDescriptorSelector :: Selector
recordDescriptorSelector = mkSelector "recordDescriptor"

-- | @Selector@ for @currentProcessDescriptor@
currentProcessDescriptorSelector :: Selector
currentProcessDescriptorSelector = mkSelector "currentProcessDescriptor"

-- | @Selector@ for @descriptorWithProcessIdentifier:@
descriptorWithProcessIdentifierSelector :: Selector
descriptorWithProcessIdentifierSelector = mkSelector "descriptorWithProcessIdentifier:"

-- | @Selector@ for @descriptorWithBundleIdentifier:@
descriptorWithBundleIdentifierSelector :: Selector
descriptorWithBundleIdentifierSelector = mkSelector "descriptorWithBundleIdentifier:"

-- | @Selector@ for @descriptorWithApplicationURL:@
descriptorWithApplicationURLSelector :: Selector
descriptorWithApplicationURLSelector = mkSelector "descriptorWithApplicationURL:"

-- | @Selector@ for @initWithAEDescNoCopy:@
initWithAEDescNoCopySelector :: Selector
initWithAEDescNoCopySelector = mkSelector "initWithAEDescNoCopy:"

-- | @Selector@ for @initWithDescriptorType:bytes:length:@
initWithDescriptorType_bytes_lengthSelector :: Selector
initWithDescriptorType_bytes_lengthSelector = mkSelector "initWithDescriptorType:bytes:length:"

-- | @Selector@ for @initWithDescriptorType:data:@
initWithDescriptorType_dataSelector :: Selector
initWithDescriptorType_dataSelector = mkSelector "initWithDescriptorType:data:"

-- | @Selector@ for @initWithEventClass:eventID:targetDescriptor:returnID:transactionID:@
initWithEventClass_eventID_targetDescriptor_returnID_transactionIDSelector :: Selector
initWithEventClass_eventID_targetDescriptor_returnID_transactionIDSelector = mkSelector "initWithEventClass:eventID:targetDescriptor:returnID:transactionID:"

-- | @Selector@ for @initListDescriptor@
initListDescriptorSelector :: Selector
initListDescriptorSelector = mkSelector "initListDescriptor"

-- | @Selector@ for @initRecordDescriptor@
initRecordDescriptorSelector :: Selector
initRecordDescriptorSelector = mkSelector "initRecordDescriptor"

-- | @Selector@ for @setParamDescriptor:forKeyword:@
setParamDescriptor_forKeywordSelector :: Selector
setParamDescriptor_forKeywordSelector = mkSelector "setParamDescriptor:forKeyword:"

-- | @Selector@ for @paramDescriptorForKeyword:@
paramDescriptorForKeywordSelector :: Selector
paramDescriptorForKeywordSelector = mkSelector "paramDescriptorForKeyword:"

-- | @Selector@ for @removeParamDescriptorWithKeyword:@
removeParamDescriptorWithKeywordSelector :: Selector
removeParamDescriptorWithKeywordSelector = mkSelector "removeParamDescriptorWithKeyword:"

-- | @Selector@ for @setAttributeDescriptor:forKeyword:@
setAttributeDescriptor_forKeywordSelector :: Selector
setAttributeDescriptor_forKeywordSelector = mkSelector "setAttributeDescriptor:forKeyword:"

-- | @Selector@ for @attributeDescriptorForKeyword:@
attributeDescriptorForKeywordSelector :: Selector
attributeDescriptorForKeywordSelector = mkSelector "attributeDescriptorForKeyword:"

-- | @Selector@ for @sendEventWithOptions:timeout:error:@
sendEventWithOptions_timeout_errorSelector :: Selector
sendEventWithOptions_timeout_errorSelector = mkSelector "sendEventWithOptions:timeout:error:"

-- | @Selector@ for @insertDescriptor:atIndex:@
insertDescriptor_atIndexSelector :: Selector
insertDescriptor_atIndexSelector = mkSelector "insertDescriptor:atIndex:"

-- | @Selector@ for @descriptorAtIndex:@
descriptorAtIndexSelector :: Selector
descriptorAtIndexSelector = mkSelector "descriptorAtIndex:"

-- | @Selector@ for @removeDescriptorAtIndex:@
removeDescriptorAtIndexSelector :: Selector
removeDescriptorAtIndexSelector = mkSelector "removeDescriptorAtIndex:"

-- | @Selector@ for @setDescriptor:forKeyword:@
setDescriptor_forKeywordSelector :: Selector
setDescriptor_forKeywordSelector = mkSelector "setDescriptor:forKeyword:"

-- | @Selector@ for @descriptorForKeyword:@
descriptorForKeywordSelector :: Selector
descriptorForKeywordSelector = mkSelector "descriptorForKeyword:"

-- | @Selector@ for @removeDescriptorWithKeyword:@
removeDescriptorWithKeywordSelector :: Selector
removeDescriptorWithKeywordSelector = mkSelector "removeDescriptorWithKeyword:"

-- | @Selector@ for @keywordForDescriptorAtIndex:@
keywordForDescriptorAtIndexSelector :: Selector
keywordForDescriptorAtIndexSelector = mkSelector "keywordForDescriptorAtIndex:"

-- | @Selector@ for @coerceToDescriptorType:@
coerceToDescriptorTypeSelector :: Selector
coerceToDescriptorTypeSelector = mkSelector "coerceToDescriptorType:"

-- | @Selector@ for @aeDesc@
aeDescSelector :: Selector
aeDescSelector = mkSelector "aeDesc"

-- | @Selector@ for @descriptorType@
descriptorTypeSelector :: Selector
descriptorTypeSelector = mkSelector "descriptorType"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @booleanValue@
booleanValueSelector :: Selector
booleanValueSelector = mkSelector "booleanValue"

-- | @Selector@ for @enumCodeValue@
enumCodeValueSelector :: Selector
enumCodeValueSelector = mkSelector "enumCodeValue"

-- | @Selector@ for @int32Value@
int32ValueSelector :: Selector
int32ValueSelector = mkSelector "int32Value"

-- | @Selector@ for @doubleValue@
doubleValueSelector :: Selector
doubleValueSelector = mkSelector "doubleValue"

-- | @Selector@ for @typeCodeValue@
typeCodeValueSelector :: Selector
typeCodeValueSelector = mkSelector "typeCodeValue"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector
stringValueSelector = mkSelector "stringValue"

-- | @Selector@ for @dateValue@
dateValueSelector :: Selector
dateValueSelector = mkSelector "dateValue"

-- | @Selector@ for @fileURLValue@
fileURLValueSelector :: Selector
fileURLValueSelector = mkSelector "fileURLValue"

-- | @Selector@ for @eventClass@
eventClassSelector :: Selector
eventClassSelector = mkSelector "eventClass"

-- | @Selector@ for @eventID@
eventIDSelector :: Selector
eventIDSelector = mkSelector "eventID"

-- | @Selector@ for @returnID@
returnIDSelector :: Selector
returnIDSelector = mkSelector "returnID"

-- | @Selector@ for @transactionID@
transactionIDSelector :: Selector
transactionIDSelector = mkSelector "transactionID"

-- | @Selector@ for @isRecordDescriptor@
isRecordDescriptorSelector :: Selector
isRecordDescriptorSelector = mkSelector "isRecordDescriptor"

-- | @Selector@ for @numberOfItems@
numberOfItemsSelector :: Selector
numberOfItemsSelector = mkSelector "numberOfItems"

