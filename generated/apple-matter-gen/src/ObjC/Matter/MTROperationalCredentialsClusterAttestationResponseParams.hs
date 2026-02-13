{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterAttestationResponseParams@.
module ObjC.Matter.MTROperationalCredentialsClusterAttestationResponseParams
  ( MTROperationalCredentialsClusterAttestationResponseParams
  , IsMTROperationalCredentialsClusterAttestationResponseParams(..)
  , initWithResponseValue_error
  , attestationElements
  , setAttestationElements
  , attestationSignature
  , setAttestationSignature
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , signature
  , setSignature
  , attestationElementsSelector
  , attestationSignatureSelector
  , initWithResponseValue_errorSelector
  , setAttestationElementsSelector
  , setAttestationSignatureSelector
  , setSignatureSelector
  , setTimedInvokeTimeoutMsSelector
  , signatureSelector
  , timedInvokeTimeoutMsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTROperationalCredentialsClusterAttestationResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTROperationalCredentialsClusterAttestationResponseParams mtrOperationalCredentialsClusterAttestationResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrOperationalCredentialsClusterAttestationResponseParams -> responseValue -> error_ -> IO (Id MTROperationalCredentialsClusterAttestationResponseParams)
initWithResponseValue_error mtrOperationalCredentialsClusterAttestationResponseParams responseValue error_ =
  sendOwnedMessage mtrOperationalCredentialsClusterAttestationResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- attestationElements@
attestationElements :: IsMTROperationalCredentialsClusterAttestationResponseParams mtrOperationalCredentialsClusterAttestationResponseParams => mtrOperationalCredentialsClusterAttestationResponseParams -> IO (Id NSData)
attestationElements mtrOperationalCredentialsClusterAttestationResponseParams =
  sendMessage mtrOperationalCredentialsClusterAttestationResponseParams attestationElementsSelector

-- | @- setAttestationElements:@
setAttestationElements :: (IsMTROperationalCredentialsClusterAttestationResponseParams mtrOperationalCredentialsClusterAttestationResponseParams, IsNSData value) => mtrOperationalCredentialsClusterAttestationResponseParams -> value -> IO ()
setAttestationElements mtrOperationalCredentialsClusterAttestationResponseParams value =
  sendMessage mtrOperationalCredentialsClusterAttestationResponseParams setAttestationElementsSelector (toNSData value)

-- | @- attestationSignature@
attestationSignature :: IsMTROperationalCredentialsClusterAttestationResponseParams mtrOperationalCredentialsClusterAttestationResponseParams => mtrOperationalCredentialsClusterAttestationResponseParams -> IO (Id NSData)
attestationSignature mtrOperationalCredentialsClusterAttestationResponseParams =
  sendMessage mtrOperationalCredentialsClusterAttestationResponseParams attestationSignatureSelector

-- | @- setAttestationSignature:@
setAttestationSignature :: (IsMTROperationalCredentialsClusterAttestationResponseParams mtrOperationalCredentialsClusterAttestationResponseParams, IsNSData value) => mtrOperationalCredentialsClusterAttestationResponseParams -> value -> IO ()
setAttestationSignature mtrOperationalCredentialsClusterAttestationResponseParams value =
  sendMessage mtrOperationalCredentialsClusterAttestationResponseParams setAttestationSignatureSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROperationalCredentialsClusterAttestationResponseParams mtrOperationalCredentialsClusterAttestationResponseParams => mtrOperationalCredentialsClusterAttestationResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOperationalCredentialsClusterAttestationResponseParams =
  sendMessage mtrOperationalCredentialsClusterAttestationResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROperationalCredentialsClusterAttestationResponseParams mtrOperationalCredentialsClusterAttestationResponseParams, IsNSNumber value) => mtrOperationalCredentialsClusterAttestationResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOperationalCredentialsClusterAttestationResponseParams value =
  sendMessage mtrOperationalCredentialsClusterAttestationResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | @- signature@
signature :: IsMTROperationalCredentialsClusterAttestationResponseParams mtrOperationalCredentialsClusterAttestationResponseParams => mtrOperationalCredentialsClusterAttestationResponseParams -> IO (Id NSData)
signature mtrOperationalCredentialsClusterAttestationResponseParams =
  sendMessage mtrOperationalCredentialsClusterAttestationResponseParams signatureSelector

-- | @- setSignature:@
setSignature :: (IsMTROperationalCredentialsClusterAttestationResponseParams mtrOperationalCredentialsClusterAttestationResponseParams, IsNSData value) => mtrOperationalCredentialsClusterAttestationResponseParams -> value -> IO ()
setSignature mtrOperationalCredentialsClusterAttestationResponseParams value =
  sendMessage mtrOperationalCredentialsClusterAttestationResponseParams setSignatureSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTROperationalCredentialsClusterAttestationResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @attestationElements@
attestationElementsSelector :: Selector '[] (Id NSData)
attestationElementsSelector = mkSelector "attestationElements"

-- | @Selector@ for @setAttestationElements:@
setAttestationElementsSelector :: Selector '[Id NSData] ()
setAttestationElementsSelector = mkSelector "setAttestationElements:"

-- | @Selector@ for @attestationSignature@
attestationSignatureSelector :: Selector '[] (Id NSData)
attestationSignatureSelector = mkSelector "attestationSignature"

-- | @Selector@ for @setAttestationSignature:@
setAttestationSignatureSelector :: Selector '[Id NSData] ()
setAttestationSignatureSelector = mkSelector "setAttestationSignature:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @signature@
signatureSelector :: Selector '[] (Id NSData)
signatureSelector = mkSelector "signature"

-- | @Selector@ for @setSignature:@
setSignatureSelector :: Selector '[Id NSData] ()
setSignatureSelector = mkSelector "setSignature:"

