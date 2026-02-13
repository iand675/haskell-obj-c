{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , dataWithJSONObject_options_errorSelector
  , isValidJSONObjectSelector
  , jsonObjectWithData_options_errorSelector
  , jsonObjectWithStream_options_errorSelector
  , writeJSONObject_toStream_options_errorSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @+ isValidJSONObject:@
isValidJSONObject :: RawId -> IO Bool
isValidJSONObject obj_ =
  do
    cls' <- getRequiredClass "NSJSONSerialization"
    sendClassMessage cls' isValidJSONObjectSelector obj_

-- | @+ dataWithJSONObject:options:error:@
dataWithJSONObject_options_error :: IsNSError error_ => RawId -> NSJSONWritingOptions -> error_ -> IO (Id NSData)
dataWithJSONObject_options_error obj_ opt error_ =
  do
    cls' <- getRequiredClass "NSJSONSerialization"
    sendClassMessage cls' dataWithJSONObject_options_errorSelector obj_ opt (toNSError error_)

-- | @+ JSONObjectWithData:options:error:@
jsonObjectWithData_options_error :: (IsNSData data_, IsNSError error_) => data_ -> NSJSONReadingOptions -> error_ -> IO RawId
jsonObjectWithData_options_error data_ opt error_ =
  do
    cls' <- getRequiredClass "NSJSONSerialization"
    sendClassMessage cls' jsonObjectWithData_options_errorSelector (toNSData data_) opt (toNSError error_)

-- | @+ writeJSONObject:toStream:options:error:@
writeJSONObject_toStream_options_error :: (IsNSOutputStream stream, IsNSError error_) => RawId -> stream -> NSJSONWritingOptions -> error_ -> IO CLong
writeJSONObject_toStream_options_error obj_ stream opt error_ =
  do
    cls' <- getRequiredClass "NSJSONSerialization"
    sendClassMessage cls' writeJSONObject_toStream_options_errorSelector obj_ (toNSOutputStream stream) opt (toNSError error_)

-- | @+ JSONObjectWithStream:options:error:@
jsonObjectWithStream_options_error :: (IsNSInputStream stream, IsNSError error_) => stream -> NSJSONReadingOptions -> error_ -> IO RawId
jsonObjectWithStream_options_error stream opt error_ =
  do
    cls' <- getRequiredClass "NSJSONSerialization"
    sendClassMessage cls' jsonObjectWithStream_options_errorSelector (toNSInputStream stream) opt (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isValidJSONObject:@
isValidJSONObjectSelector :: Selector '[RawId] Bool
isValidJSONObjectSelector = mkSelector "isValidJSONObject:"

-- | @Selector@ for @dataWithJSONObject:options:error:@
dataWithJSONObject_options_errorSelector :: Selector '[RawId, NSJSONWritingOptions, Id NSError] (Id NSData)
dataWithJSONObject_options_errorSelector = mkSelector "dataWithJSONObject:options:error:"

-- | @Selector@ for @JSONObjectWithData:options:error:@
jsonObjectWithData_options_errorSelector :: Selector '[Id NSData, NSJSONReadingOptions, Id NSError] RawId
jsonObjectWithData_options_errorSelector = mkSelector "JSONObjectWithData:options:error:"

-- | @Selector@ for @writeJSONObject:toStream:options:error:@
writeJSONObject_toStream_options_errorSelector :: Selector '[RawId, Id NSOutputStream, NSJSONWritingOptions, Id NSError] CLong
writeJSONObject_toStream_options_errorSelector = mkSelector "writeJSONObject:toStream:options:error:"

-- | @Selector@ for @JSONObjectWithStream:options:error:@
jsonObjectWithStream_options_errorSelector :: Selector '[Id NSInputStream, NSJSONReadingOptions, Id NSError] RawId
jsonObjectWithStream_options_errorSelector = mkSelector "JSONObjectWithStream:options:error:"

