{-# LANGUAGE DataKinds #-}
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
  , desiredKeysSelector
  , previousServerChangeTokenSelector
  , resultsLimitSelector
  , setDesiredKeysSelector
  , setPreviousServerChangeTokenSelector
  , setResultsLimitSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- previousServerChangeToken@
previousServerChangeToken :: IsCKFetchRecordZoneChangesConfiguration ckFetchRecordZoneChangesConfiguration => ckFetchRecordZoneChangesConfiguration -> IO (Id CKServerChangeToken)
previousServerChangeToken ckFetchRecordZoneChangesConfiguration =
  sendMessage ckFetchRecordZoneChangesConfiguration previousServerChangeTokenSelector

-- | @- setPreviousServerChangeToken:@
setPreviousServerChangeToken :: (IsCKFetchRecordZoneChangesConfiguration ckFetchRecordZoneChangesConfiguration, IsCKServerChangeToken value) => ckFetchRecordZoneChangesConfiguration -> value -> IO ()
setPreviousServerChangeToken ckFetchRecordZoneChangesConfiguration value =
  sendMessage ckFetchRecordZoneChangesConfiguration setPreviousServerChangeTokenSelector (toCKServerChangeToken value)

-- | @- resultsLimit@
resultsLimit :: IsCKFetchRecordZoneChangesConfiguration ckFetchRecordZoneChangesConfiguration => ckFetchRecordZoneChangesConfiguration -> IO CULong
resultsLimit ckFetchRecordZoneChangesConfiguration =
  sendMessage ckFetchRecordZoneChangesConfiguration resultsLimitSelector

-- | @- setResultsLimit:@
setResultsLimit :: IsCKFetchRecordZoneChangesConfiguration ckFetchRecordZoneChangesConfiguration => ckFetchRecordZoneChangesConfiguration -> CULong -> IO ()
setResultsLimit ckFetchRecordZoneChangesConfiguration value =
  sendMessage ckFetchRecordZoneChangesConfiguration setResultsLimitSelector value

-- | Declares which user-defined keys should be fetched and added to the resulting CKRecords.
--
-- If nil, declares the entire record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.  Defaults to @nil.@
--
-- ObjC selector: @- desiredKeys@
desiredKeys :: IsCKFetchRecordZoneChangesConfiguration ckFetchRecordZoneChangesConfiguration => ckFetchRecordZoneChangesConfiguration -> IO (Id NSArray)
desiredKeys ckFetchRecordZoneChangesConfiguration =
  sendMessage ckFetchRecordZoneChangesConfiguration desiredKeysSelector

-- | Declares which user-defined keys should be fetched and added to the resulting CKRecords.
--
-- If nil, declares the entire record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.  Defaults to @nil.@
--
-- ObjC selector: @- setDesiredKeys:@
setDesiredKeys :: (IsCKFetchRecordZoneChangesConfiguration ckFetchRecordZoneChangesConfiguration, IsNSArray value) => ckFetchRecordZoneChangesConfiguration -> value -> IO ()
setDesiredKeys ckFetchRecordZoneChangesConfiguration value =
  sendMessage ckFetchRecordZoneChangesConfiguration setDesiredKeysSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @previousServerChangeToken@
previousServerChangeTokenSelector :: Selector '[] (Id CKServerChangeToken)
previousServerChangeTokenSelector = mkSelector "previousServerChangeToken"

-- | @Selector@ for @setPreviousServerChangeToken:@
setPreviousServerChangeTokenSelector :: Selector '[Id CKServerChangeToken] ()
setPreviousServerChangeTokenSelector = mkSelector "setPreviousServerChangeToken:"

-- | @Selector@ for @resultsLimit@
resultsLimitSelector :: Selector '[] CULong
resultsLimitSelector = mkSelector "resultsLimit"

-- | @Selector@ for @setResultsLimit:@
setResultsLimitSelector :: Selector '[CULong] ()
setResultsLimitSelector = mkSelector "setResultsLimit:"

-- | @Selector@ for @desiredKeys@
desiredKeysSelector :: Selector '[] (Id NSArray)
desiredKeysSelector = mkSelector "desiredKeys"

-- | @Selector@ for @setDesiredKeys:@
setDesiredKeysSelector :: Selector '[Id NSArray] ()
setDesiredKeysSelector = mkSelector "setDesiredKeys:"

