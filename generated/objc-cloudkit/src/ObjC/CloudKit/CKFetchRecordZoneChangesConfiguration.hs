{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKFetchRecordZoneChangesConfiguration@.
module ObjC.CloudKit.CKFetchRecordZoneChangesConfiguration
  ( CKFetchRecordZoneChangesConfiguration
  , IsCKFetchRecordZoneChangesConfiguration(..)
  , previousServerChangeToken
  , setPreviousServerChangeToken
  , resultsLimit
  , setResultsLimit
  , desiredKeys
  , setDesiredKeys
  , previousServerChangeTokenSelector
  , setPreviousServerChangeTokenSelector
  , resultsLimitSelector
  , setResultsLimitSelector
  , desiredKeysSelector
  , setDesiredKeysSelector


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

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- previousServerChangeToken@
previousServerChangeToken :: IsCKFetchRecordZoneChangesConfiguration ckFetchRecordZoneChangesConfiguration => ckFetchRecordZoneChangesConfiguration -> IO (Id CKServerChangeToken)
previousServerChangeToken ckFetchRecordZoneChangesConfiguration  =
  sendMsg ckFetchRecordZoneChangesConfiguration (mkSelector "previousServerChangeToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreviousServerChangeToken:@
setPreviousServerChangeToken :: (IsCKFetchRecordZoneChangesConfiguration ckFetchRecordZoneChangesConfiguration, IsCKServerChangeToken value) => ckFetchRecordZoneChangesConfiguration -> value -> IO ()
setPreviousServerChangeToken ckFetchRecordZoneChangesConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckFetchRecordZoneChangesConfiguration (mkSelector "setPreviousServerChangeToken:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- resultsLimit@
resultsLimit :: IsCKFetchRecordZoneChangesConfiguration ckFetchRecordZoneChangesConfiguration => ckFetchRecordZoneChangesConfiguration -> IO CULong
resultsLimit ckFetchRecordZoneChangesConfiguration  =
  sendMsg ckFetchRecordZoneChangesConfiguration (mkSelector "resultsLimit") retCULong []

-- | @- setResultsLimit:@
setResultsLimit :: IsCKFetchRecordZoneChangesConfiguration ckFetchRecordZoneChangesConfiguration => ckFetchRecordZoneChangesConfiguration -> CULong -> IO ()
setResultsLimit ckFetchRecordZoneChangesConfiguration  value =
  sendMsg ckFetchRecordZoneChangesConfiguration (mkSelector "setResultsLimit:") retVoid [argCULong (fromIntegral value)]

-- | Declares which user-defined keys should be fetched and added to the resulting CKRecords.
--
-- If nil, declares the entire record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.  Defaults to @nil.@
--
-- ObjC selector: @- desiredKeys@
desiredKeys :: IsCKFetchRecordZoneChangesConfiguration ckFetchRecordZoneChangesConfiguration => ckFetchRecordZoneChangesConfiguration -> IO (Id NSArray)
desiredKeys ckFetchRecordZoneChangesConfiguration  =
  sendMsg ckFetchRecordZoneChangesConfiguration (mkSelector "desiredKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Declares which user-defined keys should be fetched and added to the resulting CKRecords.
--
-- If nil, declares the entire record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.  Defaults to @nil.@
--
-- ObjC selector: @- setDesiredKeys:@
setDesiredKeys :: (IsCKFetchRecordZoneChangesConfiguration ckFetchRecordZoneChangesConfiguration, IsNSArray value) => ckFetchRecordZoneChangesConfiguration -> value -> IO ()
setDesiredKeys ckFetchRecordZoneChangesConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckFetchRecordZoneChangesConfiguration (mkSelector "setDesiredKeys:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousServerChangeToken@
previousServerChangeTokenSelector :: Selector
previousServerChangeTokenSelector = mkSelector "previousServerChangeToken"

-- | @Selector@ for @setPreviousServerChangeToken:@
setPreviousServerChangeTokenSelector :: Selector
setPreviousServerChangeTokenSelector = mkSelector "setPreviousServerChangeToken:"

-- | @Selector@ for @resultsLimit@
resultsLimitSelector :: Selector
resultsLimitSelector = mkSelector "resultsLimit"

-- | @Selector@ for @setResultsLimit:@
setResultsLimitSelector :: Selector
setResultsLimitSelector = mkSelector "setResultsLimit:"

-- | @Selector@ for @desiredKeys@
desiredKeysSelector :: Selector
desiredKeysSelector = mkSelector "desiredKeys"

-- | @Selector@ for @setDesiredKeys:@
setDesiredKeysSelector :: Selector
setDesiredKeysSelector = mkSelector "setDesiredKeys:"

