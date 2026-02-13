{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWiFiNetworkManagementClusterNetworkPassphraseResponseParams@.
module ObjC.Matter.MTRWiFiNetworkManagementClusterNetworkPassphraseResponseParams
  ( MTRWiFiNetworkManagementClusterNetworkPassphraseResponseParams
  , IsMTRWiFiNetworkManagementClusterNetworkPassphraseResponseParams(..)
  , initWithResponseValue_error
  , passphrase
  , setPassphrase
  , initWithResponseValue_errorSelector
  , passphraseSelector
  , setPassphraseSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRWiFiNetworkManagementClusterNetworkPassphraseResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRWiFiNetworkManagementClusterNetworkPassphraseResponseParams mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams -> responseValue -> error_ -> IO (Id MTRWiFiNetworkManagementClusterNetworkPassphraseResponseParams)
initWithResponseValue_error mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams responseValue error_ =
  sendOwnedMessage mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- passphrase@
passphrase :: IsMTRWiFiNetworkManagementClusterNetworkPassphraseResponseParams mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams => mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams -> IO (Id NSData)
passphrase mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams =
  sendMessage mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams passphraseSelector

-- | @- setPassphrase:@
setPassphrase :: (IsMTRWiFiNetworkManagementClusterNetworkPassphraseResponseParams mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams, IsNSData value) => mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams -> value -> IO ()
setPassphrase mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams value =
  sendMessage mtrWiFiNetworkManagementClusterNetworkPassphraseResponseParams setPassphraseSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRWiFiNetworkManagementClusterNetworkPassphraseResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @passphrase@
passphraseSelector :: Selector '[] (Id NSData)
passphraseSelector = mkSelector "passphrase"

-- | @Selector@ for @setPassphrase:@
setPassphraseSelector :: Selector '[Id NSData] ()
setPassphraseSelector = mkSelector "setPassphrase:"

