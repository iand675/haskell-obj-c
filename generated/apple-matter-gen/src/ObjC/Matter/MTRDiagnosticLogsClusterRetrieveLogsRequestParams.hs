{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDiagnosticLogsClusterRetrieveLogsRequestParams@.
module ObjC.Matter.MTRDiagnosticLogsClusterRetrieveLogsRequestParams
  ( MTRDiagnosticLogsClusterRetrieveLogsRequestParams
  , IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams(..)
  , intent
  , setIntent
  , requestedProtocol
  , setRequestedProtocol
  , transferFileDesignator
  , setTransferFileDesignator
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , intentSelector
  , requestedProtocolSelector
  , serverSideProcessingTimeoutSelector
  , setIntentSelector
  , setRequestedProtocolSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setTransferFileDesignatorSelector
  , timedInvokeTimeoutMsSelector
  , transferFileDesignatorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- intent@
intent :: IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams mtrDiagnosticLogsClusterRetrieveLogsRequestParams => mtrDiagnosticLogsClusterRetrieveLogsRequestParams -> IO (Id NSNumber)
intent mtrDiagnosticLogsClusterRetrieveLogsRequestParams =
  sendMessage mtrDiagnosticLogsClusterRetrieveLogsRequestParams intentSelector

-- | @- setIntent:@
setIntent :: (IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams mtrDiagnosticLogsClusterRetrieveLogsRequestParams, IsNSNumber value) => mtrDiagnosticLogsClusterRetrieveLogsRequestParams -> value -> IO ()
setIntent mtrDiagnosticLogsClusterRetrieveLogsRequestParams value =
  sendMessage mtrDiagnosticLogsClusterRetrieveLogsRequestParams setIntentSelector (toNSNumber value)

-- | @- requestedProtocol@
requestedProtocol :: IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams mtrDiagnosticLogsClusterRetrieveLogsRequestParams => mtrDiagnosticLogsClusterRetrieveLogsRequestParams -> IO (Id NSNumber)
requestedProtocol mtrDiagnosticLogsClusterRetrieveLogsRequestParams =
  sendMessage mtrDiagnosticLogsClusterRetrieveLogsRequestParams requestedProtocolSelector

-- | @- setRequestedProtocol:@
setRequestedProtocol :: (IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams mtrDiagnosticLogsClusterRetrieveLogsRequestParams, IsNSNumber value) => mtrDiagnosticLogsClusterRetrieveLogsRequestParams -> value -> IO ()
setRequestedProtocol mtrDiagnosticLogsClusterRetrieveLogsRequestParams value =
  sendMessage mtrDiagnosticLogsClusterRetrieveLogsRequestParams setRequestedProtocolSelector (toNSNumber value)

-- | @- transferFileDesignator@
transferFileDesignator :: IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams mtrDiagnosticLogsClusterRetrieveLogsRequestParams => mtrDiagnosticLogsClusterRetrieveLogsRequestParams -> IO (Id NSString)
transferFileDesignator mtrDiagnosticLogsClusterRetrieveLogsRequestParams =
  sendMessage mtrDiagnosticLogsClusterRetrieveLogsRequestParams transferFileDesignatorSelector

-- | @- setTransferFileDesignator:@
setTransferFileDesignator :: (IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams mtrDiagnosticLogsClusterRetrieveLogsRequestParams, IsNSString value) => mtrDiagnosticLogsClusterRetrieveLogsRequestParams -> value -> IO ()
setTransferFileDesignator mtrDiagnosticLogsClusterRetrieveLogsRequestParams value =
  sendMessage mtrDiagnosticLogsClusterRetrieveLogsRequestParams setTransferFileDesignatorSelector (toNSString value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams mtrDiagnosticLogsClusterRetrieveLogsRequestParams => mtrDiagnosticLogsClusterRetrieveLogsRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDiagnosticLogsClusterRetrieveLogsRequestParams =
  sendMessage mtrDiagnosticLogsClusterRetrieveLogsRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams mtrDiagnosticLogsClusterRetrieveLogsRequestParams, IsNSNumber value) => mtrDiagnosticLogsClusterRetrieveLogsRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDiagnosticLogsClusterRetrieveLogsRequestParams value =
  sendMessage mtrDiagnosticLogsClusterRetrieveLogsRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams mtrDiagnosticLogsClusterRetrieveLogsRequestParams => mtrDiagnosticLogsClusterRetrieveLogsRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDiagnosticLogsClusterRetrieveLogsRequestParams =
  sendMessage mtrDiagnosticLogsClusterRetrieveLogsRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDiagnosticLogsClusterRetrieveLogsRequestParams mtrDiagnosticLogsClusterRetrieveLogsRequestParams, IsNSNumber value) => mtrDiagnosticLogsClusterRetrieveLogsRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrDiagnosticLogsClusterRetrieveLogsRequestParams value =
  sendMessage mtrDiagnosticLogsClusterRetrieveLogsRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @intent@
intentSelector :: Selector '[] (Id NSNumber)
intentSelector = mkSelector "intent"

-- | @Selector@ for @setIntent:@
setIntentSelector :: Selector '[Id NSNumber] ()
setIntentSelector = mkSelector "setIntent:"

-- | @Selector@ for @requestedProtocol@
requestedProtocolSelector :: Selector '[] (Id NSNumber)
requestedProtocolSelector = mkSelector "requestedProtocol"

-- | @Selector@ for @setRequestedProtocol:@
setRequestedProtocolSelector :: Selector '[Id NSNumber] ()
setRequestedProtocolSelector = mkSelector "setRequestedProtocol:"

-- | @Selector@ for @transferFileDesignator@
transferFileDesignatorSelector :: Selector '[] (Id NSString)
transferFileDesignatorSelector = mkSelector "transferFileDesignator"

-- | @Selector@ for @setTransferFileDesignator:@
setTransferFileDesignatorSelector :: Selector '[Id NSString] ()
setTransferFileDesignatorSelector = mkSelector "setTransferFileDesignator:"

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

