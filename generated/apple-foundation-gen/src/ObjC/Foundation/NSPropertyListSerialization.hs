{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPropertyListSerialization@.
module ObjC.Foundation.NSPropertyListSerialization
  ( NSPropertyListSerialization
  , IsNSPropertyListSerialization(..)
  , propertyList_isValidForFormat
  , dataWithPropertyList_format_options_error
  , writePropertyList_toStream_format_options_error
  , propertyListWithData_options_format_error
  , propertyListWithStream_options_format_error
  , dataFromPropertyList_format_errorDescription
  , propertyListFromData_mutabilityOption_format_errorDescription
  , dataFromPropertyList_format_errorDescriptionSelector
  , dataWithPropertyList_format_options_errorSelector
  , propertyListFromData_mutabilityOption_format_errorDescriptionSelector
  , propertyListWithData_options_format_errorSelector
  , propertyListWithStream_options_format_errorSelector
  , propertyList_isValidForFormatSelector
  , writePropertyList_toStream_format_options_errorSelector

  -- * Enum types
  , NSPropertyListFormat(NSPropertyListFormat)
  , pattern NSPropertyListOpenStepFormat
  , pattern NSPropertyListXMLFormat_v1_0
  , pattern NSPropertyListBinaryFormat_v1_0
  , NSPropertyListMutabilityOptions(NSPropertyListMutabilityOptions)
  , pattern NSPropertyListImmutable
  , pattern NSPropertyListMutableContainers
  , pattern NSPropertyListMutableContainersAndLeaves

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @+ propertyList:isValidForFormat:@
propertyList_isValidForFormat :: RawId -> NSPropertyListFormat -> IO Bool
propertyList_isValidForFormat plist format =
  do
    cls' <- getRequiredClass "NSPropertyListSerialization"
    sendClassMessage cls' propertyList_isValidForFormatSelector plist format

-- | @+ dataWithPropertyList:format:options:error:@
dataWithPropertyList_format_options_error :: IsNSError error_ => RawId -> NSPropertyListFormat -> CULong -> error_ -> IO (Id NSData)
dataWithPropertyList_format_options_error plist format opt error_ =
  do
    cls' <- getRequiredClass "NSPropertyListSerialization"
    sendClassMessage cls' dataWithPropertyList_format_options_errorSelector plist format opt (toNSError error_)

-- | @+ writePropertyList:toStream:format:options:error:@
writePropertyList_toStream_format_options_error :: (IsNSOutputStream stream, IsNSError error_) => RawId -> stream -> NSPropertyListFormat -> CULong -> error_ -> IO CLong
writePropertyList_toStream_format_options_error plist stream format opt error_ =
  do
    cls' <- getRequiredClass "NSPropertyListSerialization"
    sendClassMessage cls' writePropertyList_toStream_format_options_errorSelector plist (toNSOutputStream stream) format opt (toNSError error_)

-- | @+ propertyListWithData:options:format:error:@
propertyListWithData_options_format_error :: (IsNSData data_, IsNSError error_) => data_ -> NSPropertyListMutabilityOptions -> Ptr NSPropertyListFormat -> error_ -> IO RawId
propertyListWithData_options_format_error data_ opt format error_ =
  do
    cls' <- getRequiredClass "NSPropertyListSerialization"
    sendClassMessage cls' propertyListWithData_options_format_errorSelector (toNSData data_) opt format (toNSError error_)

-- | @+ propertyListWithStream:options:format:error:@
propertyListWithStream_options_format_error :: (IsNSInputStream stream, IsNSError error_) => stream -> NSPropertyListMutabilityOptions -> Ptr NSPropertyListFormat -> error_ -> IO RawId
propertyListWithStream_options_format_error stream opt format error_ =
  do
    cls' <- getRequiredClass "NSPropertyListSerialization"
    sendClassMessage cls' propertyListWithStream_options_format_errorSelector (toNSInputStream stream) opt format (toNSError error_)

-- | @+ dataFromPropertyList:format:errorDescription:@
dataFromPropertyList_format_errorDescription :: IsNSString errorString => RawId -> NSPropertyListFormat -> errorString -> IO (Id NSData)
dataFromPropertyList_format_errorDescription plist format errorString =
  do
    cls' <- getRequiredClass "NSPropertyListSerialization"
    sendClassMessage cls' dataFromPropertyList_format_errorDescriptionSelector plist format (toNSString errorString)

-- | @+ propertyListFromData:mutabilityOption:format:errorDescription:@
propertyListFromData_mutabilityOption_format_errorDescription :: (IsNSData data_, IsNSString errorString) => data_ -> NSPropertyListMutabilityOptions -> Ptr NSPropertyListFormat -> errorString -> IO RawId
propertyListFromData_mutabilityOption_format_errorDescription data_ opt format errorString =
  do
    cls' <- getRequiredClass "NSPropertyListSerialization"
    sendClassMessage cls' propertyListFromData_mutabilityOption_format_errorDescriptionSelector (toNSData data_) opt format (toNSString errorString)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @propertyList:isValidForFormat:@
propertyList_isValidForFormatSelector :: Selector '[RawId, NSPropertyListFormat] Bool
propertyList_isValidForFormatSelector = mkSelector "propertyList:isValidForFormat:"

-- | @Selector@ for @dataWithPropertyList:format:options:error:@
dataWithPropertyList_format_options_errorSelector :: Selector '[RawId, NSPropertyListFormat, CULong, Id NSError] (Id NSData)
dataWithPropertyList_format_options_errorSelector = mkSelector "dataWithPropertyList:format:options:error:"

-- | @Selector@ for @writePropertyList:toStream:format:options:error:@
writePropertyList_toStream_format_options_errorSelector :: Selector '[RawId, Id NSOutputStream, NSPropertyListFormat, CULong, Id NSError] CLong
writePropertyList_toStream_format_options_errorSelector = mkSelector "writePropertyList:toStream:format:options:error:"

-- | @Selector@ for @propertyListWithData:options:format:error:@
propertyListWithData_options_format_errorSelector :: Selector '[Id NSData, NSPropertyListMutabilityOptions, Ptr NSPropertyListFormat, Id NSError] RawId
propertyListWithData_options_format_errorSelector = mkSelector "propertyListWithData:options:format:error:"

-- | @Selector@ for @propertyListWithStream:options:format:error:@
propertyListWithStream_options_format_errorSelector :: Selector '[Id NSInputStream, NSPropertyListMutabilityOptions, Ptr NSPropertyListFormat, Id NSError] RawId
propertyListWithStream_options_format_errorSelector = mkSelector "propertyListWithStream:options:format:error:"

-- | @Selector@ for @dataFromPropertyList:format:errorDescription:@
dataFromPropertyList_format_errorDescriptionSelector :: Selector '[RawId, NSPropertyListFormat, Id NSString] (Id NSData)
dataFromPropertyList_format_errorDescriptionSelector = mkSelector "dataFromPropertyList:format:errorDescription:"

-- | @Selector@ for @propertyListFromData:mutabilityOption:format:errorDescription:@
propertyListFromData_mutabilityOption_format_errorDescriptionSelector :: Selector '[Id NSData, NSPropertyListMutabilityOptions, Ptr NSPropertyListFormat, Id NSString] RawId
propertyListFromData_mutabilityOption_format_errorDescriptionSelector = mkSelector "propertyListFromData:mutabilityOption:format:errorDescription:"

