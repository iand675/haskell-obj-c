{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , sessionForPass_delegate_completionSelector
  , sendData_errorSelector
  , invalidateSelector
  , delegateSelector
  , connectionStatusSelector

  -- * Enum types
  , PKVehicleConnectionSessionConnectionState(PKVehicleConnectionSessionConnectionState)
  , pattern PKVehicleConnectionSessionConnectionStateDisconnected
  , pattern PKVehicleConnectionSessionConnectionStateConnected
  , pattern PKVehicleConnectionSessionConnectionStateConnecting
  , pattern PKVehicleConnectionSessionConnectionStateFailedToConnect

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

import ObjC.PassKit.Internal.Classes
import ObjC.PassKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPKVehicleConnectionSession pkVehicleConnectionSession => pkVehicleConnectionSession -> IO (Id PKVehicleConnectionSession)
init_ pkVehicleConnectionSession  =
    sendMsg pkVehicleConnectionSession (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PKVehicleConnectionSession)
new  =
  do
    cls' <- getRequiredClass "PKVehicleConnectionSession"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ sessionForPass:delegate:completion:@
sessionForPass_delegate_completion :: IsPKSecureElementPass pass => pass -> RawId -> Ptr () -> IO ()
sessionForPass_delegate_completion pass delegate completion =
  do
    cls' <- getRequiredClass "PKVehicleConnectionSession"
    withObjCPtr pass $ \raw_pass ->
      sendClassMsg cls' (mkSelector "sessionForPass:delegate:completion:") retVoid [argPtr (castPtr raw_pass :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | @- sendData:error:@
sendData_error :: (IsPKVehicleConnectionSession pkVehicleConnectionSession, IsNSData message, IsNSError error_) => pkVehicleConnectionSession -> message -> error_ -> IO Bool
sendData_error pkVehicleConnectionSession  message error_ =
  withObjCPtr message $ \raw_message ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkVehicleConnectionSession (mkSelector "sendData:error:") retCULong [argPtr (castPtr raw_message :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- invalidate@
invalidate :: IsPKVehicleConnectionSession pkVehicleConnectionSession => pkVehicleConnectionSession -> IO ()
invalidate pkVehicleConnectionSession  =
    sendMsg pkVehicleConnectionSession (mkSelector "invalidate") retVoid []

-- | @- delegate@
delegate :: IsPKVehicleConnectionSession pkVehicleConnectionSession => pkVehicleConnectionSession -> IO RawId
delegate pkVehicleConnectionSession  =
    fmap (RawId . castPtr) $ sendMsg pkVehicleConnectionSession (mkSelector "delegate") (retPtr retVoid) []

-- | @- connectionStatus@
connectionStatus :: IsPKVehicleConnectionSession pkVehicleConnectionSession => pkVehicleConnectionSession -> IO PKVehicleConnectionSessionConnectionState
connectionStatus pkVehicleConnectionSession  =
    fmap (coerce :: CLong -> PKVehicleConnectionSessionConnectionState) $ sendMsg pkVehicleConnectionSession (mkSelector "connectionStatus") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @sessionForPass:delegate:completion:@
sessionForPass_delegate_completionSelector :: Selector
sessionForPass_delegate_completionSelector = mkSelector "sessionForPass:delegate:completion:"

-- | @Selector@ for @sendData:error:@
sendData_errorSelector :: Selector
sendData_errorSelector = mkSelector "sendData:error:"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector
invalidateSelector = mkSelector "invalidate"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @connectionStatus@
connectionStatusSelector :: Selector
connectionStatusSelector = mkSelector "connectionStatus"

