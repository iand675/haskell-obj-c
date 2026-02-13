{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterCertificateChainResponseParams@.
module ObjC.Matter.MTROperationalCredentialsClusterCertificateChainResponseParams
  ( MTROperationalCredentialsClusterCertificateChainResponseParams
  , IsMTROperationalCredentialsClusterCertificateChainResponseParams(..)
  , initWithResponseValue_error
  , certificate
  , setCertificate
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , certificateSelector
  , initWithResponseValue_errorSelector
  , setCertificateSelector
  , setTimedInvokeTimeoutMsSelector
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

-- | Initialize an MTROperationalCredentialsClusterCertificateChainResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTROperationalCredentialsClusterCertificateChainResponseParams mtrOperationalCredentialsClusterCertificateChainResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrOperationalCredentialsClusterCertificateChainResponseParams -> responseValue -> error_ -> IO (Id MTROperationalCredentialsClusterCertificateChainResponseParams)
initWithResponseValue_error mtrOperationalCredentialsClusterCertificateChainResponseParams responseValue error_ =
  sendOwnedMessage mtrOperationalCredentialsClusterCertificateChainResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- certificate@
certificate :: IsMTROperationalCredentialsClusterCertificateChainResponseParams mtrOperationalCredentialsClusterCertificateChainResponseParams => mtrOperationalCredentialsClusterCertificateChainResponseParams -> IO (Id NSData)
certificate mtrOperationalCredentialsClusterCertificateChainResponseParams =
  sendMessage mtrOperationalCredentialsClusterCertificateChainResponseParams certificateSelector

-- | @- setCertificate:@
setCertificate :: (IsMTROperationalCredentialsClusterCertificateChainResponseParams mtrOperationalCredentialsClusterCertificateChainResponseParams, IsNSData value) => mtrOperationalCredentialsClusterCertificateChainResponseParams -> value -> IO ()
setCertificate mtrOperationalCredentialsClusterCertificateChainResponseParams value =
  sendMessage mtrOperationalCredentialsClusterCertificateChainResponseParams setCertificateSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROperationalCredentialsClusterCertificateChainResponseParams mtrOperationalCredentialsClusterCertificateChainResponseParams => mtrOperationalCredentialsClusterCertificateChainResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOperationalCredentialsClusterCertificateChainResponseParams =
  sendMessage mtrOperationalCredentialsClusterCertificateChainResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROperationalCredentialsClusterCertificateChainResponseParams mtrOperationalCredentialsClusterCertificateChainResponseParams, IsNSNumber value) => mtrOperationalCredentialsClusterCertificateChainResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOperationalCredentialsClusterCertificateChainResponseParams value =
  sendMessage mtrOperationalCredentialsClusterCertificateChainResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTROperationalCredentialsClusterCertificateChainResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @certificate@
certificateSelector :: Selector '[] (Id NSData)
certificateSelector = mkSelector "certificate"

-- | @Selector@ for @setCertificate:@
setCertificateSelector :: Selector '[Id NSData] ()
setCertificateSelector = mkSelector "setCertificate:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

