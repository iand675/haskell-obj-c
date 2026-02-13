{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterNetworkConfigResponseParams@.
module ObjC.Matter.MTRNetworkCommissioningClusterNetworkConfigResponseParams
  ( MTRNetworkCommissioningClusterNetworkConfigResponseParams
  , IsMTRNetworkCommissioningClusterNetworkConfigResponseParams(..)
  , initWithResponseValue_error
  , networkingStatus
  , setNetworkingStatus
  , debugText
  , setDebugText
  , networkIndex
  , setNetworkIndex
  , clientIdentity
  , setClientIdentity
  , possessionSignature
  , setPossessionSignature
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , clientIdentitySelector
  , debugTextSelector
  , initWithResponseValue_errorSelector
  , networkIndexSelector
  , networkingStatusSelector
  , possessionSignatureSelector
  , setClientIdentitySelector
  , setDebugTextSelector
  , setNetworkIndexSelector
  , setNetworkingStatusSelector
  , setPossessionSignatureSelector
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

-- | Initialize an MTRNetworkCommissioningClusterNetworkConfigResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRNetworkCommissioningClusterNetworkConfigResponseParams mtrNetworkCommissioningClusterNetworkConfigResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrNetworkCommissioningClusterNetworkConfigResponseParams -> responseValue -> error_ -> IO (Id MTRNetworkCommissioningClusterNetworkConfigResponseParams)
initWithResponseValue_error mtrNetworkCommissioningClusterNetworkConfigResponseParams responseValue error_ =
  sendOwnedMessage mtrNetworkCommissioningClusterNetworkConfigResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- networkingStatus@
networkingStatus :: IsMTRNetworkCommissioningClusterNetworkConfigResponseParams mtrNetworkCommissioningClusterNetworkConfigResponseParams => mtrNetworkCommissioningClusterNetworkConfigResponseParams -> IO (Id NSNumber)
networkingStatus mtrNetworkCommissioningClusterNetworkConfigResponseParams =
  sendMessage mtrNetworkCommissioningClusterNetworkConfigResponseParams networkingStatusSelector

-- | @- setNetworkingStatus:@
setNetworkingStatus :: (IsMTRNetworkCommissioningClusterNetworkConfigResponseParams mtrNetworkCommissioningClusterNetworkConfigResponseParams, IsNSNumber value) => mtrNetworkCommissioningClusterNetworkConfigResponseParams -> value -> IO ()
setNetworkingStatus mtrNetworkCommissioningClusterNetworkConfigResponseParams value =
  sendMessage mtrNetworkCommissioningClusterNetworkConfigResponseParams setNetworkingStatusSelector (toNSNumber value)

-- | @- debugText@
debugText :: IsMTRNetworkCommissioningClusterNetworkConfigResponseParams mtrNetworkCommissioningClusterNetworkConfigResponseParams => mtrNetworkCommissioningClusterNetworkConfigResponseParams -> IO (Id NSString)
debugText mtrNetworkCommissioningClusterNetworkConfigResponseParams =
  sendMessage mtrNetworkCommissioningClusterNetworkConfigResponseParams debugTextSelector

-- | @- setDebugText:@
setDebugText :: (IsMTRNetworkCommissioningClusterNetworkConfigResponseParams mtrNetworkCommissioningClusterNetworkConfigResponseParams, IsNSString value) => mtrNetworkCommissioningClusterNetworkConfigResponseParams -> value -> IO ()
setDebugText mtrNetworkCommissioningClusterNetworkConfigResponseParams value =
  sendMessage mtrNetworkCommissioningClusterNetworkConfigResponseParams setDebugTextSelector (toNSString value)

-- | @- networkIndex@
networkIndex :: IsMTRNetworkCommissioningClusterNetworkConfigResponseParams mtrNetworkCommissioningClusterNetworkConfigResponseParams => mtrNetworkCommissioningClusterNetworkConfigResponseParams -> IO (Id NSNumber)
networkIndex mtrNetworkCommissioningClusterNetworkConfigResponseParams =
  sendMessage mtrNetworkCommissioningClusterNetworkConfigResponseParams networkIndexSelector

-- | @- setNetworkIndex:@
setNetworkIndex :: (IsMTRNetworkCommissioningClusterNetworkConfigResponseParams mtrNetworkCommissioningClusterNetworkConfigResponseParams, IsNSNumber value) => mtrNetworkCommissioningClusterNetworkConfigResponseParams -> value -> IO ()
setNetworkIndex mtrNetworkCommissioningClusterNetworkConfigResponseParams value =
  sendMessage mtrNetworkCommissioningClusterNetworkConfigResponseParams setNetworkIndexSelector (toNSNumber value)

-- | @- clientIdentity@
clientIdentity :: IsMTRNetworkCommissioningClusterNetworkConfigResponseParams mtrNetworkCommissioningClusterNetworkConfigResponseParams => mtrNetworkCommissioningClusterNetworkConfigResponseParams -> IO (Id NSData)
clientIdentity mtrNetworkCommissioningClusterNetworkConfigResponseParams =
  sendMessage mtrNetworkCommissioningClusterNetworkConfigResponseParams clientIdentitySelector

-- | @- setClientIdentity:@
setClientIdentity :: (IsMTRNetworkCommissioningClusterNetworkConfigResponseParams mtrNetworkCommissioningClusterNetworkConfigResponseParams, IsNSData value) => mtrNetworkCommissioningClusterNetworkConfigResponseParams -> value -> IO ()
setClientIdentity mtrNetworkCommissioningClusterNetworkConfigResponseParams value =
  sendMessage mtrNetworkCommissioningClusterNetworkConfigResponseParams setClientIdentitySelector (toNSData value)

-- | @- possessionSignature@
possessionSignature :: IsMTRNetworkCommissioningClusterNetworkConfigResponseParams mtrNetworkCommissioningClusterNetworkConfigResponseParams => mtrNetworkCommissioningClusterNetworkConfigResponseParams -> IO (Id NSData)
possessionSignature mtrNetworkCommissioningClusterNetworkConfigResponseParams =
  sendMessage mtrNetworkCommissioningClusterNetworkConfigResponseParams possessionSignatureSelector

-- | @- setPossessionSignature:@
setPossessionSignature :: (IsMTRNetworkCommissioningClusterNetworkConfigResponseParams mtrNetworkCommissioningClusterNetworkConfigResponseParams, IsNSData value) => mtrNetworkCommissioningClusterNetworkConfigResponseParams -> value -> IO ()
setPossessionSignature mtrNetworkCommissioningClusterNetworkConfigResponseParams value =
  sendMessage mtrNetworkCommissioningClusterNetworkConfigResponseParams setPossessionSignatureSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRNetworkCommissioningClusterNetworkConfigResponseParams mtrNetworkCommissioningClusterNetworkConfigResponseParams => mtrNetworkCommissioningClusterNetworkConfigResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrNetworkCommissioningClusterNetworkConfigResponseParams =
  sendMessage mtrNetworkCommissioningClusterNetworkConfigResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRNetworkCommissioningClusterNetworkConfigResponseParams mtrNetworkCommissioningClusterNetworkConfigResponseParams, IsNSNumber value) => mtrNetworkCommissioningClusterNetworkConfigResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrNetworkCommissioningClusterNetworkConfigResponseParams value =
  sendMessage mtrNetworkCommissioningClusterNetworkConfigResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRNetworkCommissioningClusterNetworkConfigResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @networkingStatus@
networkingStatusSelector :: Selector '[] (Id NSNumber)
networkingStatusSelector = mkSelector "networkingStatus"

-- | @Selector@ for @setNetworkingStatus:@
setNetworkingStatusSelector :: Selector '[Id NSNumber] ()
setNetworkingStatusSelector = mkSelector "setNetworkingStatus:"

-- | @Selector@ for @debugText@
debugTextSelector :: Selector '[] (Id NSString)
debugTextSelector = mkSelector "debugText"

-- | @Selector@ for @setDebugText:@
setDebugTextSelector :: Selector '[Id NSString] ()
setDebugTextSelector = mkSelector "setDebugText:"

-- | @Selector@ for @networkIndex@
networkIndexSelector :: Selector '[] (Id NSNumber)
networkIndexSelector = mkSelector "networkIndex"

-- | @Selector@ for @setNetworkIndex:@
setNetworkIndexSelector :: Selector '[Id NSNumber] ()
setNetworkIndexSelector = mkSelector "setNetworkIndex:"

-- | @Selector@ for @clientIdentity@
clientIdentitySelector :: Selector '[] (Id NSData)
clientIdentitySelector = mkSelector "clientIdentity"

-- | @Selector@ for @setClientIdentity:@
setClientIdentitySelector :: Selector '[Id NSData] ()
setClientIdentitySelector = mkSelector "setClientIdentity:"

-- | @Selector@ for @possessionSignature@
possessionSignatureSelector :: Selector '[] (Id NSData)
possessionSignatureSelector = mkSelector "possessionSignature"

-- | @Selector@ for @setPossessionSignature:@
setPossessionSignatureSelector :: Selector '[Id NSData] ()
setPossessionSignatureSelector = mkSelector "setPossessionSignature:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

