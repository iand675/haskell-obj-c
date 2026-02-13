{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKFetchRecordZoneChangesOptions@.
module ObjC.CloudKit.CKFetchRecordZoneChangesOptions
  ( CKFetchRecordZoneChangesOptions
  , IsCKFetchRecordZoneChangesOptions(..)
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
previousServerChangeToken :: IsCKFetchRecordZoneChangesOptions ckFetchRecordZoneChangesOptions => ckFetchRecordZoneChangesOptions -> IO (Id CKServerChangeToken)
previousServerChangeToken ckFetchRecordZoneChangesOptions =
  sendMessage ckFetchRecordZoneChangesOptions previousServerChangeTokenSelector

-- | @- setPreviousServerChangeToken:@
setPreviousServerChangeToken :: (IsCKFetchRecordZoneChangesOptions ckFetchRecordZoneChangesOptions, IsCKServerChangeToken value) => ckFetchRecordZoneChangesOptions -> value -> IO ()
setPreviousServerChangeToken ckFetchRecordZoneChangesOptions value =
  sendMessage ckFetchRecordZoneChangesOptions setPreviousServerChangeTokenSelector (toCKServerChangeToken value)

-- | @- resultsLimit@
resultsLimit :: IsCKFetchRecordZoneChangesOptions ckFetchRecordZoneChangesOptions => ckFetchRecordZoneChangesOptions -> IO CULong
resultsLimit ckFetchRecordZoneChangesOptions =
  sendMessage ckFetchRecordZoneChangesOptions resultsLimitSelector

-- | @- setResultsLimit:@
setResultsLimit :: IsCKFetchRecordZoneChangesOptions ckFetchRecordZoneChangesOptions => ckFetchRecordZoneChangesOptions -> CULong -> IO ()
setResultsLimit ckFetchRecordZoneChangesOptions value =
  sendMessage ckFetchRecordZoneChangesOptions setResultsLimitSelector value

-- | @- desiredKeys@
desiredKeys :: IsCKFetchRecordZoneChangesOptions ckFetchRecordZoneChangesOptions => ckFetchRecordZoneChangesOptions -> IO (Id NSArray)
desiredKeys ckFetchRecordZoneChangesOptions =
  sendMessage ckFetchRecordZoneChangesOptions desiredKeysSelector

-- | @- setDesiredKeys:@
setDesiredKeys :: (IsCKFetchRecordZoneChangesOptions ckFetchRecordZoneChangesOptions, IsNSArray value) => ckFetchRecordZoneChangesOptions -> value -> IO ()
setDesiredKeys ckFetchRecordZoneChangesOptions value =
  sendMessage ckFetchRecordZoneChangesOptions setDesiredKeysSelector (toNSArray value)

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

