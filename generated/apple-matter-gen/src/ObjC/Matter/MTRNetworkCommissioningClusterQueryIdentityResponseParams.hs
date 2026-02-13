{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterQueryIdentityResponseParams@.
module ObjC.Matter.MTRNetworkCommissioningClusterQueryIdentityResponseParams
  ( MTRNetworkCommissioningClusterQueryIdentityResponseParams
  , IsMTRNetworkCommissioningClusterQueryIdentityResponseParams(..)
  , initWithResponseValue_error
  , identity
  , setIdentity
  , possessionSignature
  , setPossessionSignature
  , identitySelector
  , initWithResponseValue_errorSelector
  , possessionSignatureSelector
  , setIdentitySelector
  , setPossessionSignatureSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRNetworkCommissioningClusterQueryIdentityResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRNetworkCommissioningClusterQueryIdentityResponseParams mtrNetworkCommissioningClusterQueryIdentityResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrNetworkCommissioningClusterQueryIdentityResponseParams -> responseValue -> error_ -> IO (Id MTRNetworkCommissioningClusterQueryIdentityResponseParams)
initWithResponseValue_error mtrNetworkCommissioningClusterQueryIdentityResponseParams responseValue error_ =
  sendOwnedMessage mtrNetworkCommissioningClusterQueryIdentityResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- identity@
identity :: IsMTRNetworkCommissioningClusterQueryIdentityResponseParams mtrNetworkCommissioningClusterQueryIdentityResponseParams => mtrNetworkCommissioningClusterQueryIdentityResponseParams -> IO (Id NSData)
identity mtrNetworkCommissioningClusterQueryIdentityResponseParams =
  sendMessage mtrNetworkCommissioningClusterQueryIdentityResponseParams identitySelector

-- | @- setIdentity:@
setIdentity :: (IsMTRNetworkCommissioningClusterQueryIdentityResponseParams mtrNetworkCommissioningClusterQueryIdentityResponseParams, IsNSData value) => mtrNetworkCommissioningClusterQueryIdentityResponseParams -> value -> IO ()
setIdentity mtrNetworkCommissioningClusterQueryIdentityResponseParams value =
  sendMessage mtrNetworkCommissioningClusterQueryIdentityResponseParams setIdentitySelector (toNSData value)

-- | @- possessionSignature@
possessionSignature :: IsMTRNetworkCommissioningClusterQueryIdentityResponseParams mtrNetworkCommissioningClusterQueryIdentityResponseParams => mtrNetworkCommissioningClusterQueryIdentityResponseParams -> IO (Id NSData)
possessionSignature mtrNetworkCommissioningClusterQueryIdentityResponseParams =
  sendMessage mtrNetworkCommissioningClusterQueryIdentityResponseParams possessionSignatureSelector

-- | @- setPossessionSignature:@
setPossessionSignature :: (IsMTRNetworkCommissioningClusterQueryIdentityResponseParams mtrNetworkCommissioningClusterQueryIdentityResponseParams, IsNSData value) => mtrNetworkCommissioningClusterQueryIdentityResponseParams -> value -> IO ()
setPossessionSignature mtrNetworkCommissioningClusterQueryIdentityResponseParams value =
  sendMessage mtrNetworkCommissioningClusterQueryIdentityResponseParams setPossessionSignatureSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRNetworkCommissioningClusterQueryIdentityResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @identity@
identitySelector :: Selector '[] (Id NSData)
identitySelector = mkSelector "identity"

-- | @Selector@ for @setIdentity:@
setIdentitySelector :: Selector '[Id NSData] ()
setIdentitySelector = mkSelector "setIdentity:"

-- | @Selector@ for @possessionSignature@
possessionSignatureSelector :: Selector '[] (Id NSData)
possessionSignatureSelector = mkSelector "possessionSignature"

-- | @Selector@ for @setPossessionSignature:@
setPossessionSignatureSelector :: Selector '[Id NSData] ()
setPossessionSignatureSelector = mkSelector "setPossessionSignature:"

