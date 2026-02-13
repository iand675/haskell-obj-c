{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , aeDescSelector
  , appleEventWithEventClass_eventID_targetDescriptor_returnID_transactionIDSelector
  , attributeDescriptorForKeywordSelector
  , booleanValueSelector
  , coerceToDescriptorTypeSelector
  , currentProcessDescriptorSelector
  , dataSelector
  , dateValueSelector
  , descriptorAtIndexSelector
  , descriptorForKeywordSelector
  , descriptorTypeSelector
  , descriptorWithApplicationURLSelector
  , descriptorWithBooleanSelector
  , descriptorWithBundleIdentifierSelector
  , descriptorWithDateSelector
  , descriptorWithDescriptorType_bytes_lengthSelector
  , descriptorWithDescriptorType_dataSelector
  , descriptorWithDoubleSelector
  , descriptorWithEnumCodeSelector
  , descriptorWithFileURLSelector
  , descriptorWithInt32Selector
  , descriptorWithProcessIdentifierSelector
  , descriptorWithStringSelector
  , descriptorWithTypeCodeSelector
  , doubleValueSelector
  , enumCodeValueSelector
  , eventClassSelector
  , eventIDSelector
  , fileURLValueSelector
  , initListDescriptorSelector
  , initRecordDescriptorSelector
  , initWithAEDescNoCopySelector
  , initWithDescriptorType_bytes_lengthSelector
  , initWithDescriptorType_dataSelector
  , initWithEventClass_eventID_targetDescriptor_returnID_transactionIDSelector
  , insertDescriptor_atIndexSelector
  , int32ValueSelector
  , isRecordDescriptorSelector
  , keywordForDescriptorAtIndexSelector
  , listDescriptorSelector
  , nullDescriptorSelector
  , numberOfItemsSelector
  , paramDescriptorForKeywordSelector
  , recordDescriptorSelector
  , removeDescriptorAtIndexSelector
  , removeDescriptorWithKeywordSelector
  , removeParamDescriptorWithKeywordSelector
  , returnIDSelector
  , sendEventWithOptions_timeout_errorSelector
  , setAttributeDescriptor_forKeywordSelector
  , setDescriptor_forKeywordSelector
  , setParamDescriptor_forKeywordSelector
  , stringValueSelector
  , transactionIDSelector
  , typeCodeValueSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @+ nullDescriptor@
nullDescriptor :: IO (Id NSAppleEventDescriptor)
nullDescriptor  =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMessage cls' nullDescriptorSelector

-- | @+ descriptorWithDescriptorType:bytes:length:@
descriptorWithDescriptorType_bytes_length :: CUInt -> Const (Ptr ()) -> CULong -> IO (Id NSAppleEventDescriptor)
descriptorWithDescriptorType_bytes_length descriptorType bytes byteCount =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMessage cls' descriptorWithDescriptorType_bytes_lengthSelector descriptorType bytes byteCount

-- | @+ descriptorWithDescriptorType:data:@
descriptorWithDescriptorType_data :: IsNSData data_ => CUInt -> data_ -> IO (Id NSAppleEventDescriptor)
descriptorWithDescriptorType_data descriptorType data_ =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMessage cls' descriptorWithDescriptorType_dataSelector descriptorType (toNSData data_)

-- | @+ descriptorWithBoolean:@
descriptorWithBoolean :: CUChar -> IO (Id NSAppleEventDescriptor)
descriptorWithBoolean boolean =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMessage cls' descriptorWithBooleanSelector boolean

-- | @+ descriptorWithEnumCode:@
descriptorWithEnumCode :: CUInt -> IO (Id NSAppleEventDescriptor)
descriptorWithEnumCode enumerator =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMessage cls' descriptorWithEnumCodeSelector enumerator

-- | @+ descriptorWithInt32:@
descriptorWithInt32 :: CInt -> IO (Id NSAppleEventDescriptor)
descriptorWithInt32 signedInt =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMessage cls' descriptorWithInt32Selector signedInt

-- | @+ descriptorWithDouble:@
descriptorWithDouble :: CDouble -> IO (Id NSAppleEventDescriptor)
descriptorWithDouble doubleValue =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMessage cls' descriptorWithDoubleSelector doubleValue

-- | @+ descriptorWithTypeCode:@
descriptorWithTypeCode :: CUInt -> IO (Id NSAppleEventDescriptor)
descriptorWithTypeCode typeCode =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMessage cls' descriptorWithTypeCodeSelector typeCode

-- | @+ descriptorWithString:@
descriptorWithString :: IsNSString string => string -> IO (Id NSAppleEventDescriptor)
descriptorWithString string =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMessage cls' descriptorWithStringSelector (toNSString string)

-- | @+ descriptorWithDate:@
descriptorWithDate :: IsNSDate date => date -> IO (Id NSAppleEventDescriptor)
descriptorWithDate date =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMessage cls' descriptorWithDateSelector (toNSDate date)

-- | @+ descriptorWithFileURL:@
descriptorWithFileURL :: IsNSURL fileURL => fileURL -> IO (Id NSAppleEventDescriptor)
descriptorWithFileURL fileURL =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMessage cls' descriptorWithFileURLSelector (toNSURL fileURL)

-- | @+ appleEventWithEventClass:eventID:targetDescriptor:returnID:transactionID:@
appleEventWithEventClass_eventID_targetDescriptor_returnID_transactionID :: IsNSAppleEventDescriptor targetDescriptor => CUInt -> CUInt -> targetDescriptor -> CShort -> CInt -> IO (Id NSAppleEventDescriptor)
appleEventWithEventClass_eventID_targetDescriptor_returnID_transactionID eventClass eventID targetDescriptor returnID transactionID =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMessage cls' appleEventWithEventClass_eventID_targetDescriptor_returnID_transactionIDSelector eventClass eventID (toNSAppleEventDescriptor targetDescriptor) returnID transactionID

-- | @+ listDescriptor@
listDescriptor :: IO (Id NSAppleEventDescriptor)
listDescriptor  =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMessage cls' listDescriptorSelector

-- | @+ recordDescriptor@
recordDescriptor :: IO (Id NSAppleEventDescriptor)
recordDescriptor  =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMessage cls' recordDescriptorSelector

-- | @+ currentProcessDescriptor@
currentProcessDescriptor :: IO (Id NSAppleEventDescriptor)
currentProcessDescriptor  =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMessage cls' currentProcessDescriptorSelector

-- | @+ descriptorWithProcessIdentifier:@
descriptorWithProcessIdentifier :: CInt -> IO (Id NSAppleEventDescriptor)
descriptorWithProcessIdentifier processIdentifier =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMessage cls' descriptorWithProcessIdentifierSelector processIdentifier

-- | @+ descriptorWithBundleIdentifier:@
descriptorWithBundleIdentifier :: IsNSString bundleIdentifier => bundleIdentifier -> IO (Id NSAppleEventDescriptor)
descriptorWithBundleIdentifier bundleIdentifier =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMessage cls' descriptorWithBundleIdentifierSelector (toNSString bundleIdentifier)

-- | @+ descriptorWithApplicationURL:@
descriptorWithApplicationURL :: IsNSURL applicationURL => applicationURL -> IO (Id NSAppleEventDescriptor)
descriptorWithApplicationURL applicationURL =
  do
    cls' <- getRequiredClass "NSAppleEventDescriptor"
    sendClassMessage cls' descriptorWithApplicationURLSelector (toNSURL applicationURL)

-- | @- initWithAEDescNoCopy:@
initWithAEDescNoCopy :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> Const RawId -> IO (Id NSAppleEventDescriptor)
initWithAEDescNoCopy nsAppleEventDescriptor aeDesc =
  sendOwnedMessage nsAppleEventDescriptor initWithAEDescNoCopySelector aeDesc

-- | @- initWithDescriptorType:bytes:length:@
initWithDescriptorType_bytes_length :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> CUInt -> Const (Ptr ()) -> CULong -> IO (Id NSAppleEventDescriptor)
initWithDescriptorType_bytes_length nsAppleEventDescriptor descriptorType bytes byteCount =
  sendOwnedMessage nsAppleEventDescriptor initWithDescriptorType_bytes_lengthSelector descriptorType bytes byteCount

-- | @- initWithDescriptorType:data:@
initWithDescriptorType_data :: (IsNSAppleEventDescriptor nsAppleEventDescriptor, IsNSData data_) => nsAppleEventDescriptor -> CUInt -> data_ -> IO (Id NSAppleEventDescriptor)
initWithDescriptorType_data nsAppleEventDescriptor descriptorType data_ =
  sendOwnedMessage nsAppleEventDescriptor initWithDescriptorType_dataSelector descriptorType (toNSData data_)

-- | @- initWithEventClass:eventID:targetDescriptor:returnID:transactionID:@
initWithEventClass_eventID_targetDescriptor_returnID_transactionID :: (IsNSAppleEventDescriptor nsAppleEventDescriptor, IsNSAppleEventDescriptor targetDescriptor) => nsAppleEventDescriptor -> CUInt -> CUInt -> targetDescriptor -> CShort -> CInt -> IO (Id NSAppleEventDescriptor)
initWithEventClass_eventID_targetDescriptor_returnID_transactionID nsAppleEventDescriptor eventClass eventID targetDescriptor returnID transactionID =
  sendOwnedMessage nsAppleEventDescriptor initWithEventClass_eventID_targetDescriptor_returnID_transactionIDSelector eventClass eventID (toNSAppleEventDescriptor targetDescriptor) returnID transactionID

-- | @- initListDescriptor@
initListDescriptor :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO (Id NSAppleEventDescriptor)
initListDescriptor nsAppleEventDescriptor =
  sendOwnedMessage nsAppleEventDescriptor initListDescriptorSelector

-- | @- initRecordDescriptor@
initRecordDescriptor :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO (Id NSAppleEventDescriptor)
initRecordDescriptor nsAppleEventDescriptor =
  sendOwnedMessage nsAppleEventDescriptor initRecordDescriptorSelector

-- | @- setParamDescriptor:forKeyword:@
setParamDescriptor_forKeyword :: (IsNSAppleEventDescriptor nsAppleEventDescriptor, IsNSAppleEventDescriptor descriptor) => nsAppleEventDescriptor -> descriptor -> CUInt -> IO ()
setParamDescriptor_forKeyword nsAppleEventDescriptor descriptor keyword =
  sendMessage nsAppleEventDescriptor setParamDescriptor_forKeywordSelector (toNSAppleEventDescriptor descriptor) keyword

-- | @- paramDescriptorForKeyword:@
paramDescriptorForKeyword :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> CUInt -> IO (Id NSAppleEventDescriptor)
paramDescriptorForKeyword nsAppleEventDescriptor keyword =
  sendMessage nsAppleEventDescriptor paramDescriptorForKeywordSelector keyword

-- | @- removeParamDescriptorWithKeyword:@
removeParamDescriptorWithKeyword :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> CUInt -> IO ()
removeParamDescriptorWithKeyword nsAppleEventDescriptor keyword =
  sendMessage nsAppleEventDescriptor removeParamDescriptorWithKeywordSelector keyword

-- | @- setAttributeDescriptor:forKeyword:@
setAttributeDescriptor_forKeyword :: (IsNSAppleEventDescriptor nsAppleEventDescriptor, IsNSAppleEventDescriptor descriptor) => nsAppleEventDescriptor -> descriptor -> CUInt -> IO ()
setAttributeDescriptor_forKeyword nsAppleEventDescriptor descriptor keyword =
  sendMessage nsAppleEventDescriptor setAttributeDescriptor_forKeywordSelector (toNSAppleEventDescriptor descriptor) keyword

-- | @- attributeDescriptorForKeyword:@
attributeDescriptorForKeyword :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> CUInt -> IO (Id NSAppleEventDescriptor)
attributeDescriptorForKeyword nsAppleEventDescriptor keyword =
  sendMessage nsAppleEventDescriptor attributeDescriptorForKeywordSelector keyword

-- | @- sendEventWithOptions:timeout:error:@
sendEventWithOptions_timeout_error :: (IsNSAppleEventDescriptor nsAppleEventDescriptor, IsNSError error_) => nsAppleEventDescriptor -> NSAppleEventSendOptions -> CDouble -> error_ -> IO (Id NSAppleEventDescriptor)
sendEventWithOptions_timeout_error nsAppleEventDescriptor sendOptions timeoutInSeconds error_ =
  sendMessage nsAppleEventDescriptor sendEventWithOptions_timeout_errorSelector sendOptions timeoutInSeconds (toNSError error_)

-- | @- insertDescriptor:atIndex:@
insertDescriptor_atIndex :: (IsNSAppleEventDescriptor nsAppleEventDescriptor, IsNSAppleEventDescriptor descriptor) => nsAppleEventDescriptor -> descriptor -> CLong -> IO ()
insertDescriptor_atIndex nsAppleEventDescriptor descriptor index =
  sendMessage nsAppleEventDescriptor insertDescriptor_atIndexSelector (toNSAppleEventDescriptor descriptor) index

-- | @- descriptorAtIndex:@
descriptorAtIndex :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> CLong -> IO (Id NSAppleEventDescriptor)
descriptorAtIndex nsAppleEventDescriptor index =
  sendMessage nsAppleEventDescriptor descriptorAtIndexSelector index

-- | @- removeDescriptorAtIndex:@
removeDescriptorAtIndex :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> CLong -> IO ()
removeDescriptorAtIndex nsAppleEventDescriptor index =
  sendMessage nsAppleEventDescriptor removeDescriptorAtIndexSelector index

-- | @- setDescriptor:forKeyword:@
setDescriptor_forKeyword :: (IsNSAppleEventDescriptor nsAppleEventDescriptor, IsNSAppleEventDescriptor descriptor) => nsAppleEventDescriptor -> descriptor -> CUInt -> IO ()
setDescriptor_forKeyword nsAppleEventDescriptor descriptor keyword =
  sendMessage nsAppleEventDescriptor setDescriptor_forKeywordSelector (toNSAppleEventDescriptor descriptor) keyword

-- | @- descriptorForKeyword:@
descriptorForKeyword :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> CUInt -> IO (Id NSAppleEventDescriptor)
descriptorForKeyword nsAppleEventDescriptor keyword =
  sendMessage nsAppleEventDescriptor descriptorForKeywordSelector keyword

-- | @- removeDescriptorWithKeyword:@
removeDescriptorWithKeyword :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> CUInt -> IO ()
removeDescriptorWithKeyword nsAppleEventDescriptor keyword =
  sendMessage nsAppleEventDescriptor removeDescriptorWithKeywordSelector keyword

-- | @- keywordForDescriptorAtIndex:@
keywordForDescriptorAtIndex :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> CLong -> IO CUInt
keywordForDescriptorAtIndex nsAppleEventDescriptor index =
  sendMessage nsAppleEventDescriptor keywordForDescriptorAtIndexSelector index

-- | @- coerceToDescriptorType:@
coerceToDescriptorType :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> CUInt -> IO (Id NSAppleEventDescriptor)
coerceToDescriptorType nsAppleEventDescriptor descriptorType =
  sendMessage nsAppleEventDescriptor coerceToDescriptorTypeSelector descriptorType

-- | @- aeDesc@
aeDesc :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO RawId
aeDesc nsAppleEventDescriptor =
  sendMessage nsAppleEventDescriptor aeDescSelector

-- | @- descriptorType@
descriptorType :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO CUInt
descriptorType nsAppleEventDescriptor =
  sendMessage nsAppleEventDescriptor descriptorTypeSelector

-- | @- data@
data_ :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO (Id NSData)
data_ nsAppleEventDescriptor =
  sendMessage nsAppleEventDescriptor dataSelector

-- | @- booleanValue@
booleanValue :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO CUChar
booleanValue nsAppleEventDescriptor =
  sendMessage nsAppleEventDescriptor booleanValueSelector

-- | @- enumCodeValue@
enumCodeValue :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO CUInt
enumCodeValue nsAppleEventDescriptor =
  sendMessage nsAppleEventDescriptor enumCodeValueSelector

-- | @- int32Value@
int32Value :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO CInt
int32Value nsAppleEventDescriptor =
  sendMessage nsAppleEventDescriptor int32ValueSelector

-- | @- doubleValue@
doubleValue :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO CDouble
doubleValue nsAppleEventDescriptor =
  sendMessage nsAppleEventDescriptor doubleValueSelector

-- | @- typeCodeValue@
typeCodeValue :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO CUInt
typeCodeValue nsAppleEventDescriptor =
  sendMessage nsAppleEventDescriptor typeCodeValueSelector

-- | @- stringValue@
stringValue :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO (Id NSString)
stringValue nsAppleEventDescriptor =
  sendMessage nsAppleEventDescriptor stringValueSelector

-- | @- dateValue@
dateValue :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO (Id NSDate)
dateValue nsAppleEventDescriptor =
  sendMessage nsAppleEventDescriptor dateValueSelector

-- | @- fileURLValue@
fileURLValue :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO (Id NSURL)
fileURLValue nsAppleEventDescriptor =
  sendMessage nsAppleEventDescriptor fileURLValueSelector

-- | @- eventClass@
eventClass :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO CUInt
eventClass nsAppleEventDescriptor =
  sendMessage nsAppleEventDescriptor eventClassSelector

-- | @- eventID@
eventID :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO CUInt
eventID nsAppleEventDescriptor =
  sendMessage nsAppleEventDescriptor eventIDSelector

-- | @- returnID@
returnID :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO CShort
returnID nsAppleEventDescriptor =
  sendMessage nsAppleEventDescriptor returnIDSelector

-- | @- transactionID@
transactionID :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO CInt
transactionID nsAppleEventDescriptor =
  sendMessage nsAppleEventDescriptor transactionIDSelector

-- | @- isRecordDescriptor@
isRecordDescriptor :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO Bool
isRecordDescriptor nsAppleEventDescriptor =
  sendMessage nsAppleEventDescriptor isRecordDescriptorSelector

-- | @- numberOfItems@
numberOfItems :: IsNSAppleEventDescriptor nsAppleEventDescriptor => nsAppleEventDescriptor -> IO CLong
numberOfItems nsAppleEventDescriptor =
  sendMessage nsAppleEventDescriptor numberOfItemsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nullDescriptor@
nullDescriptorSelector :: Selector '[] (Id NSAppleEventDescriptor)
nullDescriptorSelector = mkSelector "nullDescriptor"

-- | @Selector@ for @descriptorWithDescriptorType:bytes:length:@
descriptorWithDescriptorType_bytes_lengthSelector :: Selector '[CUInt, Const (Ptr ()), CULong] (Id NSAppleEventDescriptor)
descriptorWithDescriptorType_bytes_lengthSelector = mkSelector "descriptorWithDescriptorType:bytes:length:"

-- | @Selector@ for @descriptorWithDescriptorType:data:@
descriptorWithDescriptorType_dataSelector :: Selector '[CUInt, Id NSData] (Id NSAppleEventDescriptor)
descriptorWithDescriptorType_dataSelector = mkSelector "descriptorWithDescriptorType:data:"

-- | @Selector@ for @descriptorWithBoolean:@
descriptorWithBooleanSelector :: Selector '[CUChar] (Id NSAppleEventDescriptor)
descriptorWithBooleanSelector = mkSelector "descriptorWithBoolean:"

-- | @Selector@ for @descriptorWithEnumCode:@
descriptorWithEnumCodeSelector :: Selector '[CUInt] (Id NSAppleEventDescriptor)
descriptorWithEnumCodeSelector = mkSelector "descriptorWithEnumCode:"

-- | @Selector@ for @descriptorWithInt32:@
descriptorWithInt32Selector :: Selector '[CInt] (Id NSAppleEventDescriptor)
descriptorWithInt32Selector = mkSelector "descriptorWithInt32:"

-- | @Selector@ for @descriptorWithDouble:@
descriptorWithDoubleSelector :: Selector '[CDouble] (Id NSAppleEventDescriptor)
descriptorWithDoubleSelector = mkSelector "descriptorWithDouble:"

-- | @Selector@ for @descriptorWithTypeCode:@
descriptorWithTypeCodeSelector :: Selector '[CUInt] (Id NSAppleEventDescriptor)
descriptorWithTypeCodeSelector = mkSelector "descriptorWithTypeCode:"

-- | @Selector@ for @descriptorWithString:@
descriptorWithStringSelector :: Selector '[Id NSString] (Id NSAppleEventDescriptor)
descriptorWithStringSelector = mkSelector "descriptorWithString:"

-- | @Selector@ for @descriptorWithDate:@
descriptorWithDateSelector :: Selector '[Id NSDate] (Id NSAppleEventDescriptor)
descriptorWithDateSelector = mkSelector "descriptorWithDate:"

-- | @Selector@ for @descriptorWithFileURL:@
descriptorWithFileURLSelector :: Selector '[Id NSURL] (Id NSAppleEventDescriptor)
descriptorWithFileURLSelector = mkSelector "descriptorWithFileURL:"

-- | @Selector@ for @appleEventWithEventClass:eventID:targetDescriptor:returnID:transactionID:@
appleEventWithEventClass_eventID_targetDescriptor_returnID_transactionIDSelector :: Selector '[CUInt, CUInt, Id NSAppleEventDescriptor, CShort, CInt] (Id NSAppleEventDescriptor)
appleEventWithEventClass_eventID_targetDescriptor_returnID_transactionIDSelector = mkSelector "appleEventWithEventClass:eventID:targetDescriptor:returnID:transactionID:"

-- | @Selector@ for @listDescriptor@
listDescriptorSelector :: Selector '[] (Id NSAppleEventDescriptor)
listDescriptorSelector = mkSelector "listDescriptor"

-- | @Selector@ for @recordDescriptor@
recordDescriptorSelector :: Selector '[] (Id NSAppleEventDescriptor)
recordDescriptorSelector = mkSelector "recordDescriptor"

-- | @Selector@ for @currentProcessDescriptor@
currentProcessDescriptorSelector :: Selector '[] (Id NSAppleEventDescriptor)
currentProcessDescriptorSelector = mkSelector "currentProcessDescriptor"

-- | @Selector@ for @descriptorWithProcessIdentifier:@
descriptorWithProcessIdentifierSelector :: Selector '[CInt] (Id NSAppleEventDescriptor)
descriptorWithProcessIdentifierSelector = mkSelector "descriptorWithProcessIdentifier:"

-- | @Selector@ for @descriptorWithBundleIdentifier:@
descriptorWithBundleIdentifierSelector :: Selector '[Id NSString] (Id NSAppleEventDescriptor)
descriptorWithBundleIdentifierSelector = mkSelector "descriptorWithBundleIdentifier:"

-- | @Selector@ for @descriptorWithApplicationURL:@
descriptorWithApplicationURLSelector :: Selector '[Id NSURL] (Id NSAppleEventDescriptor)
descriptorWithApplicationURLSelector = mkSelector "descriptorWithApplicationURL:"

-- | @Selector@ for @initWithAEDescNoCopy:@
initWithAEDescNoCopySelector :: Selector '[Const RawId] (Id NSAppleEventDescriptor)
initWithAEDescNoCopySelector = mkSelector "initWithAEDescNoCopy:"

-- | @Selector@ for @initWithDescriptorType:bytes:length:@
initWithDescriptorType_bytes_lengthSelector :: Selector '[CUInt, Const (Ptr ()), CULong] (Id NSAppleEventDescriptor)
initWithDescriptorType_bytes_lengthSelector = mkSelector "initWithDescriptorType:bytes:length:"

-- | @Selector@ for @initWithDescriptorType:data:@
initWithDescriptorType_dataSelector :: Selector '[CUInt, Id NSData] (Id NSAppleEventDescriptor)
initWithDescriptorType_dataSelector = mkSelector "initWithDescriptorType:data:"

-- | @Selector@ for @initWithEventClass:eventID:targetDescriptor:returnID:transactionID:@
initWithEventClass_eventID_targetDescriptor_returnID_transactionIDSelector :: Selector '[CUInt, CUInt, Id NSAppleEventDescriptor, CShort, CInt] (Id NSAppleEventDescriptor)
initWithEventClass_eventID_targetDescriptor_returnID_transactionIDSelector = mkSelector "initWithEventClass:eventID:targetDescriptor:returnID:transactionID:"

-- | @Selector@ for @initListDescriptor@
initListDescriptorSelector :: Selector '[] (Id NSAppleEventDescriptor)
initListDescriptorSelector = mkSelector "initListDescriptor"

-- | @Selector@ for @initRecordDescriptor@
initRecordDescriptorSelector :: Selector '[] (Id NSAppleEventDescriptor)
initRecordDescriptorSelector = mkSelector "initRecordDescriptor"

-- | @Selector@ for @setParamDescriptor:forKeyword:@
setParamDescriptor_forKeywordSelector :: Selector '[Id NSAppleEventDescriptor, CUInt] ()
setParamDescriptor_forKeywordSelector = mkSelector "setParamDescriptor:forKeyword:"

-- | @Selector@ for @paramDescriptorForKeyword:@
paramDescriptorForKeywordSelector :: Selector '[CUInt] (Id NSAppleEventDescriptor)
paramDescriptorForKeywordSelector = mkSelector "paramDescriptorForKeyword:"

-- | @Selector@ for @removeParamDescriptorWithKeyword:@
removeParamDescriptorWithKeywordSelector :: Selector '[CUInt] ()
removeParamDescriptorWithKeywordSelector = mkSelector "removeParamDescriptorWithKeyword:"

-- | @Selector@ for @setAttributeDescriptor:forKeyword:@
setAttributeDescriptor_forKeywordSelector :: Selector '[Id NSAppleEventDescriptor, CUInt] ()
setAttributeDescriptor_forKeywordSelector = mkSelector "setAttributeDescriptor:forKeyword:"

-- | @Selector@ for @attributeDescriptorForKeyword:@
attributeDescriptorForKeywordSelector :: Selector '[CUInt] (Id NSAppleEventDescriptor)
attributeDescriptorForKeywordSelector = mkSelector "attributeDescriptorForKeyword:"

-- | @Selector@ for @sendEventWithOptions:timeout:error:@
sendEventWithOptions_timeout_errorSelector :: Selector '[NSAppleEventSendOptions, CDouble, Id NSError] (Id NSAppleEventDescriptor)
sendEventWithOptions_timeout_errorSelector = mkSelector "sendEventWithOptions:timeout:error:"

-- | @Selector@ for @insertDescriptor:atIndex:@
insertDescriptor_atIndexSelector :: Selector '[Id NSAppleEventDescriptor, CLong] ()
insertDescriptor_atIndexSelector = mkSelector "insertDescriptor:atIndex:"

-- | @Selector@ for @descriptorAtIndex:@
descriptorAtIndexSelector :: Selector '[CLong] (Id NSAppleEventDescriptor)
descriptorAtIndexSelector = mkSelector "descriptorAtIndex:"

-- | @Selector@ for @removeDescriptorAtIndex:@
removeDescriptorAtIndexSelector :: Selector '[CLong] ()
removeDescriptorAtIndexSelector = mkSelector "removeDescriptorAtIndex:"

-- | @Selector@ for @setDescriptor:forKeyword:@
setDescriptor_forKeywordSelector :: Selector '[Id NSAppleEventDescriptor, CUInt] ()
setDescriptor_forKeywordSelector = mkSelector "setDescriptor:forKeyword:"

-- | @Selector@ for @descriptorForKeyword:@
descriptorForKeywordSelector :: Selector '[CUInt] (Id NSAppleEventDescriptor)
descriptorForKeywordSelector = mkSelector "descriptorForKeyword:"

-- | @Selector@ for @removeDescriptorWithKeyword:@
removeDescriptorWithKeywordSelector :: Selector '[CUInt] ()
removeDescriptorWithKeywordSelector = mkSelector "removeDescriptorWithKeyword:"

-- | @Selector@ for @keywordForDescriptorAtIndex:@
keywordForDescriptorAtIndexSelector :: Selector '[CLong] CUInt
keywordForDescriptorAtIndexSelector = mkSelector "keywordForDescriptorAtIndex:"

-- | @Selector@ for @coerceToDescriptorType:@
coerceToDescriptorTypeSelector :: Selector '[CUInt] (Id NSAppleEventDescriptor)
coerceToDescriptorTypeSelector = mkSelector "coerceToDescriptorType:"

-- | @Selector@ for @aeDesc@
aeDescSelector :: Selector '[] RawId
aeDescSelector = mkSelector "aeDesc"

-- | @Selector@ for @descriptorType@
descriptorTypeSelector :: Selector '[] CUInt
descriptorTypeSelector = mkSelector "descriptorType"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @booleanValue@
booleanValueSelector :: Selector '[] CUChar
booleanValueSelector = mkSelector "booleanValue"

-- | @Selector@ for @enumCodeValue@
enumCodeValueSelector :: Selector '[] CUInt
enumCodeValueSelector = mkSelector "enumCodeValue"

-- | @Selector@ for @int32Value@
int32ValueSelector :: Selector '[] CInt
int32ValueSelector = mkSelector "int32Value"

-- | @Selector@ for @doubleValue@
doubleValueSelector :: Selector '[] CDouble
doubleValueSelector = mkSelector "doubleValue"

-- | @Selector@ for @typeCodeValue@
typeCodeValueSelector :: Selector '[] CUInt
typeCodeValueSelector = mkSelector "typeCodeValue"

-- | @Selector@ for @stringValue@
stringValueSelector :: Selector '[] (Id NSString)
stringValueSelector = mkSelector "stringValue"

-- | @Selector@ for @dateValue@
dateValueSelector :: Selector '[] (Id NSDate)
dateValueSelector = mkSelector "dateValue"

-- | @Selector@ for @fileURLValue@
fileURLValueSelector :: Selector '[] (Id NSURL)
fileURLValueSelector = mkSelector "fileURLValue"

-- | @Selector@ for @eventClass@
eventClassSelector :: Selector '[] CUInt
eventClassSelector = mkSelector "eventClass"

-- | @Selector@ for @eventID@
eventIDSelector :: Selector '[] CUInt
eventIDSelector = mkSelector "eventID"

-- | @Selector@ for @returnID@
returnIDSelector :: Selector '[] CShort
returnIDSelector = mkSelector "returnID"

-- | @Selector@ for @transactionID@
transactionIDSelector :: Selector '[] CInt
transactionIDSelector = mkSelector "transactionID"

-- | @Selector@ for @isRecordDescriptor@
isRecordDescriptorSelector :: Selector '[] Bool
isRecordDescriptorSelector = mkSelector "isRecordDescriptor"

-- | @Selector@ for @numberOfItems@
numberOfItemsSelector :: Selector '[] CLong
numberOfItemsSelector = mkSelector "numberOfItems"

