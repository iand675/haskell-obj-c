{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterUpdateFabricLabelParams@.
module ObjC.Matter.MTROperationalCredentialsClusterUpdateFabricLabelParams
  ( MTROperationalCredentialsClusterUpdateFabricLabelParams
  , IsMTROperationalCredentialsClusterUpdateFabricLabelParams(..)
  , label
  , setLabel
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , labelSelector
  , serverSideProcessingTimeoutSelector
  , setLabelSelector
  , setServerSideProcessingTimeoutSelector
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

-- | @- label@
label :: IsMTROperationalCredentialsClusterUpdateFabricLabelParams mtrOperationalCredentialsClusterUpdateFabricLabelParams => mtrOperationalCredentialsClusterUpdateFabricLabelParams -> IO (Id NSString)
label mtrOperationalCredentialsClusterUpdateFabricLabelParams =
  sendMessage mtrOperationalCredentialsClusterUpdateFabricLabelParams labelSelector

-- | @- setLabel:@
setLabel :: (IsMTROperationalCredentialsClusterUpdateFabricLabelParams mtrOperationalCredentialsClusterUpdateFabricLabelParams, IsNSString value) => mtrOperationalCredentialsClusterUpdateFabricLabelParams -> value -> IO ()
setLabel mtrOperationalCredentialsClusterUpdateFabricLabelParams value =
  sendMessage mtrOperationalCredentialsClusterUpdateFabricLabelParams setLabelSelector (toNSString value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROperationalCredentialsClusterUpdateFabricLabelParams mtrOperationalCredentialsClusterUpdateFabricLabelParams => mtrOperationalCredentialsClusterUpdateFabricLabelParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOperationalCredentialsClusterUpdateFabricLabelParams =
  sendMessage mtrOperationalCredentialsClusterUpdateFabricLabelParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROperationalCredentialsClusterUpdateFabricLabelParams mtrOperationalCredentialsClusterUpdateFabricLabelParams, IsNSNumber value) => mtrOperationalCredentialsClusterUpdateFabricLabelParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOperationalCredentialsClusterUpdateFabricLabelParams value =
  sendMessage mtrOperationalCredentialsClusterUpdateFabricLabelParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROperationalCredentialsClusterUpdateFabricLabelParams mtrOperationalCredentialsClusterUpdateFabricLabelParams => mtrOperationalCredentialsClusterUpdateFabricLabelParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrOperationalCredentialsClusterUpdateFabricLabelParams =
  sendMessage mtrOperationalCredentialsClusterUpdateFabricLabelParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROperationalCredentialsClusterUpdateFabricLabelParams mtrOperationalCredentialsClusterUpdateFabricLabelParams, IsNSNumber value) => mtrOperationalCredentialsClusterUpdateFabricLabelParams -> value -> IO ()
setServerSideProcessingTimeout mtrOperationalCredentialsClusterUpdateFabricLabelParams value =
  sendMessage mtrOperationalCredentialsClusterUpdateFabricLabelParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

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

