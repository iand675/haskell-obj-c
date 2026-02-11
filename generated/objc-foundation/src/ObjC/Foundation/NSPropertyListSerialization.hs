{-# LANGUAGE PatternSynonyms #-}
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
  , propertyList_isValidForFormatSelector
  , dataWithPropertyList_format_options_errorSelector
  , writePropertyList_toStream_format_options_errorSelector
  , propertyListWithData_options_format_errorSelector
  , propertyListWithStream_options_format_errorSelector
  , dataFromPropertyList_format_errorDescriptionSelector
  , propertyListFromData_mutabilityOption_format_errorDescriptionSelector

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

-- | @+ propertyList:isValidForFormat:@
propertyList_isValidForFormat :: RawId -> NSPropertyListFormat -> IO Bool
propertyList_isValidForFormat plist format =
  do
    cls' <- getRequiredClass "NSPropertyListSerialization"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "propertyList:isValidForFormat:") retCULong [argPtr (castPtr (unRawId plist) :: Ptr ()), argCULong (coerce format)]

-- | @+ dataWithPropertyList:format:options:error:@
dataWithPropertyList_format_options_error :: IsNSError error_ => RawId -> NSPropertyListFormat -> CULong -> error_ -> IO (Id NSData)
dataWithPropertyList_format_options_error plist format opt error_ =
  do
    cls' <- getRequiredClass "NSPropertyListSerialization"
    withObjCPtr error_ $ \raw_error_ ->
      sendClassMsg cls' (mkSelector "dataWithPropertyList:format:options:error:") (retPtr retVoid) [argPtr (castPtr (unRawId plist) :: Ptr ()), argCULong (coerce format), argCULong (fromIntegral opt), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ writePropertyList:toStream:format:options:error:@
writePropertyList_toStream_format_options_error :: (IsNSOutputStream stream, IsNSError error_) => RawId -> stream -> NSPropertyListFormat -> CULong -> error_ -> IO CLong
writePropertyList_toStream_format_options_error plist stream format opt error_ =
  do
    cls' <- getRequiredClass "NSPropertyListSerialization"
    withObjCPtr stream $ \raw_stream ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "writePropertyList:toStream:format:options:error:") retCLong [argPtr (castPtr (unRawId plist) :: Ptr ()), argPtr (castPtr raw_stream :: Ptr ()), argCULong (coerce format), argCULong (fromIntegral opt), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ propertyListWithData:options:format:error:@
propertyListWithData_options_format_error :: (IsNSData data_, IsNSError error_) => data_ -> NSPropertyListMutabilityOptions -> Ptr NSPropertyListFormat -> error_ -> IO RawId
propertyListWithData_options_format_error data_ opt format error_ =
  do
    cls' <- getRequiredClass "NSPropertyListSerialization"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "propertyListWithData:options:format:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argCULong (coerce opt), argPtr format, argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ propertyListWithStream:options:format:error:@
propertyListWithStream_options_format_error :: (IsNSInputStream stream, IsNSError error_) => stream -> NSPropertyListMutabilityOptions -> Ptr NSPropertyListFormat -> error_ -> IO RawId
propertyListWithStream_options_format_error stream opt format error_ =
  do
    cls' <- getRequiredClass "NSPropertyListSerialization"
    withObjCPtr stream $ \raw_stream ->
      withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "propertyListWithStream:options:format:error:") (retPtr retVoid) [argPtr (castPtr raw_stream :: Ptr ()), argCULong (coerce opt), argPtr format, argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ dataFromPropertyList:format:errorDescription:@
dataFromPropertyList_format_errorDescription :: IsNSString errorString => RawId -> NSPropertyListFormat -> errorString -> IO (Id NSData)
dataFromPropertyList_format_errorDescription plist format errorString =
  do
    cls' <- getRequiredClass "NSPropertyListSerialization"
    withObjCPtr errorString $ \raw_errorString ->
      sendClassMsg cls' (mkSelector "dataFromPropertyList:format:errorDescription:") (retPtr retVoid) [argPtr (castPtr (unRawId plist) :: Ptr ()), argCULong (coerce format), argPtr (castPtr raw_errorString :: Ptr ())] >>= retainedObject . castPtr

-- | @+ propertyListFromData:mutabilityOption:format:errorDescription:@
propertyListFromData_mutabilityOption_format_errorDescription :: (IsNSData data_, IsNSString errorString) => data_ -> NSPropertyListMutabilityOptions -> Ptr NSPropertyListFormat -> errorString -> IO RawId
propertyListFromData_mutabilityOption_format_errorDescription data_ opt format errorString =
  do
    cls' <- getRequiredClass "NSPropertyListSerialization"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr errorString $ \raw_errorString ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "propertyListFromData:mutabilityOption:format:errorDescription:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argCULong (coerce opt), argPtr format, argPtr (castPtr raw_errorString :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @propertyList:isValidForFormat:@
propertyList_isValidForFormatSelector :: Selector
propertyList_isValidForFormatSelector = mkSelector "propertyList:isValidForFormat:"

-- | @Selector@ for @dataWithPropertyList:format:options:error:@
dataWithPropertyList_format_options_errorSelector :: Selector
dataWithPropertyList_format_options_errorSelector = mkSelector "dataWithPropertyList:format:options:error:"

-- | @Selector@ for @writePropertyList:toStream:format:options:error:@
writePropertyList_toStream_format_options_errorSelector :: Selector
writePropertyList_toStream_format_options_errorSelector = mkSelector "writePropertyList:toStream:format:options:error:"

-- | @Selector@ for @propertyListWithData:options:format:error:@
propertyListWithData_options_format_errorSelector :: Selector
propertyListWithData_options_format_errorSelector = mkSelector "propertyListWithData:options:format:error:"

-- | @Selector@ for @propertyListWithStream:options:format:error:@
propertyListWithStream_options_format_errorSelector :: Selector
propertyListWithStream_options_format_errorSelector = mkSelector "propertyListWithStream:options:format:error:"

-- | @Selector@ for @dataFromPropertyList:format:errorDescription:@
dataFromPropertyList_format_errorDescriptionSelector :: Selector
dataFromPropertyList_format_errorDescriptionSelector = mkSelector "dataFromPropertyList:format:errorDescription:"

-- | @Selector@ for @propertyListFromData:mutabilityOption:format:errorDescription:@
propertyListFromData_mutabilityOption_format_errorDescriptionSelector :: Selector
propertyListFromData_mutabilityOption_format_errorDescriptionSelector = mkSelector "propertyListFromData:mutabilityOption:format:errorDescription:"

