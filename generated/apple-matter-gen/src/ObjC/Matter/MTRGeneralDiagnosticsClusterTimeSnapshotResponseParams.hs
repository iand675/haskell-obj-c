{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRGeneralDiagnosticsClusterTimeSnapshotResponseParams@.
module ObjC.Matter.MTRGeneralDiagnosticsClusterTimeSnapshotResponseParams
  ( MTRGeneralDiagnosticsClusterTimeSnapshotResponseParams
  , IsMTRGeneralDiagnosticsClusterTimeSnapshotResponseParams(..)
  , initWithResponseValue_error
  , systemTimeMs
  , setSystemTimeMs
  , posixTimeMs
  , setPosixTimeMs
  , initWithResponseValue_errorSelector
  , posixTimeMsSelector
  , setPosixTimeMsSelector
  , setSystemTimeMsSelector
  , systemTimeMsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRGeneralDiagnosticsClusterTimeSnapshotResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRGeneralDiagnosticsClusterTimeSnapshotResponseParams mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams -> responseValue -> error_ -> IO (Id MTRGeneralDiagnosticsClusterTimeSnapshotResponseParams)
initWithResponseValue_error mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams responseValue error_ =
  sendOwnedMessage mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- systemTimeMs@
systemTimeMs :: IsMTRGeneralDiagnosticsClusterTimeSnapshotResponseParams mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams => mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams -> IO (Id NSNumber)
systemTimeMs mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams =
  sendMessage mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams systemTimeMsSelector

-- | @- setSystemTimeMs:@
setSystemTimeMs :: (IsMTRGeneralDiagnosticsClusterTimeSnapshotResponseParams mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams, IsNSNumber value) => mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams -> value -> IO ()
setSystemTimeMs mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams value =
  sendMessage mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams setSystemTimeMsSelector (toNSNumber value)

-- | @- posixTimeMs@
posixTimeMs :: IsMTRGeneralDiagnosticsClusterTimeSnapshotResponseParams mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams => mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams -> IO (Id NSNumber)
posixTimeMs mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams =
  sendMessage mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams posixTimeMsSelector

-- | @- setPosixTimeMs:@
setPosixTimeMs :: (IsMTRGeneralDiagnosticsClusterTimeSnapshotResponseParams mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams, IsNSNumber value) => mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams -> value -> IO ()
setPosixTimeMs mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams value =
  sendMessage mtrGeneralDiagnosticsClusterTimeSnapshotResponseParams setPosixTimeMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRGeneralDiagnosticsClusterTimeSnapshotResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @systemTimeMs@
systemTimeMsSelector :: Selector '[] (Id NSNumber)
systemTimeMsSelector = mkSelector "systemTimeMs"

-- | @Selector@ for @setSystemTimeMs:@
setSystemTimeMsSelector :: Selector '[Id NSNumber] ()
setSystemTimeMsSelector = mkSelector "setSystemTimeMs:"

-- | @Selector@ for @posixTimeMs@
posixTimeMsSelector :: Selector '[] (Id NSNumber)
posixTimeMsSelector = mkSelector "posixTimeMs"

-- | @Selector@ for @setPosixTimeMs:@
setPosixTimeMsSelector :: Selector '[Id NSNumber] ()
setPosixTimeMsSelector = mkSelector "setPosixTimeMs:"

