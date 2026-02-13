{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWebRTCTransportRequestorClusterICECandidatesParams@.
module ObjC.Matter.MTRWebRTCTransportRequestorClusterICECandidatesParams
  ( MTRWebRTCTransportRequestorClusterICECandidatesParams
  , IsMTRWebRTCTransportRequestorClusterICECandidatesParams(..)
  , webRTCSessionID
  , setWebRTCSessionID
  , iceCandidates
  , setIceCandidates
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , iceCandidatesSelector
  , serverSideProcessingTimeoutSelector
  , setIceCandidatesSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setWebRTCSessionIDSelector
  , timedInvokeTimeoutMsSelector
  , webRTCSessionIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- webRTCSessionID@
webRTCSessionID :: IsMTRWebRTCTransportRequestorClusterICECandidatesParams mtrWebRTCTransportRequestorClusterICECandidatesParams => mtrWebRTCTransportRequestorClusterICECandidatesParams -> IO (Id NSNumber)
webRTCSessionID mtrWebRTCTransportRequestorClusterICECandidatesParams =
  sendMessage mtrWebRTCTransportRequestorClusterICECandidatesParams webRTCSessionIDSelector

-- | @- setWebRTCSessionID:@
setWebRTCSessionID :: (IsMTRWebRTCTransportRequestorClusterICECandidatesParams mtrWebRTCTransportRequestorClusterICECandidatesParams, IsNSNumber value) => mtrWebRTCTransportRequestorClusterICECandidatesParams -> value -> IO ()
setWebRTCSessionID mtrWebRTCTransportRequestorClusterICECandidatesParams value =
  sendMessage mtrWebRTCTransportRequestorClusterICECandidatesParams setWebRTCSessionIDSelector (toNSNumber value)

-- | @- iceCandidates@
iceCandidates :: IsMTRWebRTCTransportRequestorClusterICECandidatesParams mtrWebRTCTransportRequestorClusterICECandidatesParams => mtrWebRTCTransportRequestorClusterICECandidatesParams -> IO (Id NSArray)
iceCandidates mtrWebRTCTransportRequestorClusterICECandidatesParams =
  sendMessage mtrWebRTCTransportRequestorClusterICECandidatesParams iceCandidatesSelector

-- | @- setIceCandidates:@
setIceCandidates :: (IsMTRWebRTCTransportRequestorClusterICECandidatesParams mtrWebRTCTransportRequestorClusterICECandidatesParams, IsNSArray value) => mtrWebRTCTransportRequestorClusterICECandidatesParams -> value -> IO ()
setIceCandidates mtrWebRTCTransportRequestorClusterICECandidatesParams value =
  sendMessage mtrWebRTCTransportRequestorClusterICECandidatesParams setIceCandidatesSelector (toNSArray value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRWebRTCTransportRequestorClusterICECandidatesParams mtrWebRTCTransportRequestorClusterICECandidatesParams => mtrWebRTCTransportRequestorClusterICECandidatesParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrWebRTCTransportRequestorClusterICECandidatesParams =
  sendMessage mtrWebRTCTransportRequestorClusterICECandidatesParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRWebRTCTransportRequestorClusterICECandidatesParams mtrWebRTCTransportRequestorClusterICECandidatesParams, IsNSNumber value) => mtrWebRTCTransportRequestorClusterICECandidatesParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrWebRTCTransportRequestorClusterICECandidatesParams value =
  sendMessage mtrWebRTCTransportRequestorClusterICECandidatesParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRWebRTCTransportRequestorClusterICECandidatesParams mtrWebRTCTransportRequestorClusterICECandidatesParams => mtrWebRTCTransportRequestorClusterICECandidatesParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrWebRTCTransportRequestorClusterICECandidatesParams =
  sendMessage mtrWebRTCTransportRequestorClusterICECandidatesParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRWebRTCTransportRequestorClusterICECandidatesParams mtrWebRTCTransportRequestorClusterICECandidatesParams, IsNSNumber value) => mtrWebRTCTransportRequestorClusterICECandidatesParams -> value -> IO ()
setServerSideProcessingTimeout mtrWebRTCTransportRequestorClusterICECandidatesParams value =
  sendMessage mtrWebRTCTransportRequestorClusterICECandidatesParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @webRTCSessionID@
webRTCSessionIDSelector :: Selector '[] (Id NSNumber)
webRTCSessionIDSelector = mkSelector "webRTCSessionID"

-- | @Selector@ for @setWebRTCSessionID:@
setWebRTCSessionIDSelector :: Selector '[Id NSNumber] ()
setWebRTCSessionIDSelector = mkSelector "setWebRTCSessionID:"

-- | @Selector@ for @iceCandidates@
iceCandidatesSelector :: Selector '[] (Id NSArray)
iceCandidatesSelector = mkSelector "iceCandidates"

-- | @Selector@ for @setIceCandidates:@
setIceCandidatesSelector :: Selector '[Id NSArray] ()
setIceCandidatesSelector = mkSelector "setIceCandidates:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector '[] (Id NSNumber)
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector '[Id NSNumber] ()
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"

