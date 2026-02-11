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
previousServerChangeToken :: IsCKFetchRecordZoneChangesOptions ckFetchRecordZoneChangesOptions => ckFetchRecordZoneChangesOptions -> IO (Id CKServerChangeToken)
previousServerChangeToken ckFetchRecordZoneChangesOptions  =
  sendMsg ckFetchRecordZoneChangesOptions (mkSelector "previousServerChangeToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreviousServerChangeToken:@
setPreviousServerChangeToken :: (IsCKFetchRecordZoneChangesOptions ckFetchRecordZoneChangesOptions, IsCKServerChangeToken value) => ckFetchRecordZoneChangesOptions -> value -> IO ()
setPreviousServerChangeToken ckFetchRecordZoneChangesOptions  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckFetchRecordZoneChangesOptions (mkSelector "setPreviousServerChangeToken:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- resultsLimit@
resultsLimit :: IsCKFetchRecordZoneChangesOptions ckFetchRecordZoneChangesOptions => ckFetchRecordZoneChangesOptions -> IO CULong
resultsLimit ckFetchRecordZoneChangesOptions  =
  sendMsg ckFetchRecordZoneChangesOptions (mkSelector "resultsLimit") retCULong []

-- | @- setResultsLimit:@
setResultsLimit :: IsCKFetchRecordZoneChangesOptions ckFetchRecordZoneChangesOptions => ckFetchRecordZoneChangesOptions -> CULong -> IO ()
setResultsLimit ckFetchRecordZoneChangesOptions  value =
  sendMsg ckFetchRecordZoneChangesOptions (mkSelector "setResultsLimit:") retVoid [argCULong (fromIntegral value)]

-- | @- desiredKeys@
desiredKeys :: IsCKFetchRecordZoneChangesOptions ckFetchRecordZoneChangesOptions => ckFetchRecordZoneChangesOptions -> IO (Id NSArray)
desiredKeys ckFetchRecordZoneChangesOptions  =
  sendMsg ckFetchRecordZoneChangesOptions (mkSelector "desiredKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDesiredKeys:@
setDesiredKeys :: (IsCKFetchRecordZoneChangesOptions ckFetchRecordZoneChangesOptions, IsNSArray value) => ckFetchRecordZoneChangesOptions -> value -> IO ()
setDesiredKeys ckFetchRecordZoneChangesOptions  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckFetchRecordZoneChangesOptions (mkSelector "setDesiredKeys:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

