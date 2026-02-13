{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PKVehicleConnectionSession@.
module ObjC.PassKit.PKVehicleConnectionSession
  ( PKVehicleConnectionSession
  , IsPKVehicleConnectionSession(..)
  , init_
  , new
  , sessionForPass_delegate_completion
  , sendData_error
  , invalidate
  , delegate
  , connectionStatus
  , connectionStatusSelector
  , delegateSelector
  , initSelector
  , invalidateSelector
  , newSelector
  , sendData_errorSelector
  , sessionForPass_delegate_completionSelector

  -- * Enum types
  , PKVehicleConnectionSessionConnectionState(PKVehicleConnectionSessionConnectionState)
  , pattern PKVehicleConnectionSessionConnectionStateDisconnected
  , pattern PKVehicleConnectionSessionConnectionStateConnected
  , pattern PKVehicleConnectionSessionConnectionStateConnecting
  , pattern PKVehicleConnectionSessionConnectionStateFailedToConnect

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PassKit.Internal.Classes
import ObjC.PassKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPKVehicleConnectionSession pkVehicleConnectionSession => pkVehicleConnectionSession -> IO (Id PKVehicleConnectionSession)
init_ pkVehicleConnectionSession =
  sendOwnedMessage pkVehicleConnectionSession initSelector

-- | @+ new@
new :: IO (Id PKVehicleConnectionSession)
new  =
  do
    cls' <- getRequiredClass "PKVehicleConnectionSession"
    sendOwnedClassMessage cls' newSelector

-- | @+ sessionForPass:delegate:completion:@
sessionForPass_delegate_completion :: IsPKSecureElementPass pass => pass -> RawId -> Ptr () -> IO ()
sessionForPass_delegate_completion pass delegate completion =
  do
    cls' <- getRequiredClass "PKVehicleConnectionSession"
    sendClassMessage cls' sessionForPass_delegate_completionSelector (toPKSecureElementPass pass) delegate completion

-- | @- sendData:error:@
sendData_error :: (IsPKVehicleConnectionSession pkVehicleConnectionSession, IsNSData message, IsNSError error_) => pkVehicleConnectionSession -> message -> error_ -> IO Bool
sendData_error pkVehicleConnectionSession message error_ =
  sendMessage pkVehicleConnectionSession sendData_errorSelector (toNSData message) (toNSError error_)

-- | @- invalidate@
invalidate :: IsPKVehicleConnectionSession pkVehicleConnectionSession => pkVehicleConnectionSession -> IO ()
invalidate pkVehicleConnectionSession =
  sendMessage pkVehicleConnectionSession invalidateSelector

-- | @- delegate@
delegate :: IsPKVehicleConnectionSession pkVehicleConnectionSession => pkVehicleConnectionSession -> IO RawId
delegate pkVehicleConnectionSession =
  sendMessage pkVehicleConnectionSession delegateSelector

-- | @- connectionStatus@
connectionStatus :: IsPKVehicleConnectionSession pkVehicleConnectionSession => pkVehicleConnectionSession -> IO PKVehicleConnectionSessionConnectionState
connectionStatus pkVehicleConnectionSession =
  sendMessage pkVehicleConnectionSession connectionStatusSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PKVehicleConnectionSession)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PKVehicleConnectionSession)
newSelector = mkSelector "new"

-- | @Selector@ for @sessionForPass:delegate:completion:@
sessionForPass_delegate_completionSelector :: Selector '[Id PKSecureElementPass, RawId, Ptr ()] ()
sessionForPass_delegate_completionSelector = mkSelector "sessionForPass:delegate:completion:"

-- | @Selector@ for @sendData:error:@
sendData_errorSelector :: Selector '[Id NSData, Id NSError] Bool
sendData_errorSelector = mkSelector "sendData:error:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector '[] ()
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @connectionStatus@
connectionStatusSelector :: Selector '[] PKVehicleConnectionSessionConnectionState
connectionStatusSelector = mkSelector "connectionStatus"

