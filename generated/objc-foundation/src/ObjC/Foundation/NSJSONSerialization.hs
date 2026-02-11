{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSJSONSerialization@.
module ObjC.Foundation.NSJSONSerialization
  ( NSJSONSerialization
  , IsNSJSONSerialization(..)
  , isValidJSONObject
  , dataWithJSONObject_options_error
  , jsonObjectWithData_options_error
  , writeJSONObject_toStream_options_error
  , jsonObjectWithStream_options_error
  , isValidJSONObjectSelector
  , dataWithJSONObject_options_errorSelector
  , jsonObjectWithData_options_errorSelector
  , writeJSONObject_toStream_options_errorSelector
  , jsonObjectWithStream_options_errorSelector

  -- * Enum types
  , NSJSONReadingOptions(NSJSONReadingOptions)
  , pattern NSJSONReadingMutableContainers
  , pattern NSJSONReadingMutableLeaves
  , pattern NSJSONReadingFragmentsAllowed
  , pattern NSJSONReadingJSON5Allowed
  , pattern NSJSONReadingTopLevelDictionaryAssumed
  , pattern NSJSONReadingAllowFragments
  , NSJSONWritingOptions(NSJSONWritingOptions)
  , pattern NSJSONWritingPrettyPrinted
  , pattern NSJSONWritingSortedKeys
  , pattern NSJSONWritingFragmentsAllowed
  , pattern NSJSONWritingWithoutEscapingSlashes

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

-- | @+ isValidJSONObject:@
isValidJSONObject :: RawId -> IO Bool
isValidJSONObject obj_ =
  do
    cls' <- getRequiredClass "NSJSONSerialization"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "isValidJSONObject:") retCULong [argPtr (castPtr (unRawId obj_) :: Ptr ())]

-- | @+ dataWithJSONObject:options:error:@
dataWithJSONObject_options_error :: IsNSError error_ => RawId -> NSJSONWritingOptions -> error_ -> IO (Id NSData)
dataWithJSONObject_options_error obj_ opt error_ =
  do
    cls' <- getRequiredClass "NSJSONSerialization"
    withObjCPtr error_ $ \raw_error_ ->
      sendClassMsg cls' (mkSelector "dataWithJSONObject:options:error:") (retPtr retVoid) [argPtr (castPtr (unRawId obj_) :: Ptr ()), argCULong (coerce opt), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ JSONObjectWithData:options:error:@
jsonObjectWithData_options_error :: (IsNSData data_, IsNSError error_) => data_ -> NSJSONReadingOptions -> error_ -> IO RawId
jsonObjectWithData_options_error data_ opt error_ =
  do
    cls' <- getRequiredClass "NSJSONSerialization"
    withObjCPtr data_ $ \raw_data_ ->
      withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "JSONObjectWithData:options:error:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ()), argCULong (coerce opt), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ writeJSONObject:toStream:options:error:@
writeJSONObject_toStream_options_error :: (IsNSOutputStream stream, IsNSError error_) => RawId -> stream -> NSJSONWritingOptions -> error_ -> IO CLong
writeJSONObject_toStream_options_error obj_ stream opt error_ =
  do
    cls' <- getRequiredClass "NSJSONSerialization"
    withObjCPtr stream $ \raw_stream ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "writeJSONObject:toStream:options:error:") retCLong [argPtr (castPtr (unRawId obj_) :: Ptr ()), argPtr (castPtr raw_stream :: Ptr ()), argCULong (coerce opt), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ JSONObjectWithStream:options:error:@
jsonObjectWithStream_options_error :: (IsNSInputStream stream, IsNSError error_) => stream -> NSJSONReadingOptions -> error_ -> IO RawId
jsonObjectWithStream_options_error stream opt error_ =
  do
    cls' <- getRequiredClass "NSJSONSerialization"
    withObjCPtr stream $ \raw_stream ->
      withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "JSONObjectWithStream:options:error:") (retPtr retVoid) [argPtr (castPtr raw_stream :: Ptr ()), argCULong (coerce opt), argPtr (castPtr raw_error_ :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isValidJSONObject:@
isValidJSONObjectSelector :: Selector
isValidJSONObjectSelector = mkSelector "isValidJSONObject:"

-- | @Selector@ for @dataWithJSONObject:options:error:@
dataWithJSONObject_options_errorSelector :: Selector
dataWithJSONObject_options_errorSelector = mkSelector "dataWithJSONObject:options:error:"

-- | @Selector@ for @JSONObjectWithData:options:error:@
jsonObjectWithData_options_errorSelector :: Selector
jsonObjectWithData_options_errorSelector = mkSelector "JSONObjectWithData:options:error:"

-- | @Selector@ for @writeJSONObject:toStream:options:error:@
writeJSONObject_toStream_options_errorSelector :: Selector
writeJSONObject_toStream_options_errorSelector = mkSelector "writeJSONObject:toStream:options:error:"

-- | @Selector@ for @JSONObjectWithStream:options:error:@
jsonObjectWithStream_options_errorSelector :: Selector
jsonObjectWithStream_options_errorSelector = mkSelector "JSONObjectWithStream:options:error:"

