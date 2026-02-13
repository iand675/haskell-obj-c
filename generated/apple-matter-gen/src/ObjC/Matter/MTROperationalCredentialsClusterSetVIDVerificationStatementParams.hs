{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterSetVIDVerificationStatementParams@.
module ObjC.Matter.MTROperationalCredentialsClusterSetVIDVerificationStatementParams
  ( MTROperationalCredentialsClusterSetVIDVerificationStatementParams
  , IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams(..)
  , vendorID
  , setVendorID
  , vidVerificationStatement
  , setVidVerificationStatement
  , vvsc
  , setVvsc
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setVendorIDSelector
  , setVidVerificationStatementSelector
  , setVvscSelector
  , timedInvokeTimeoutMsSelector
  , vendorIDSelector
  , vidVerificationStatementSelector
  , vvscSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- vendorID@
vendorID :: IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams mtrOperationalCredentialsClusterSetVIDVerificationStatementParams => mtrOperationalCredentialsClusterSetVIDVerificationStatementParams -> IO (Id NSNumber)
vendorID mtrOperationalCredentialsClusterSetVIDVerificationStatementParams =
  sendMessage mtrOperationalCredentialsClusterSetVIDVerificationStatementParams vendorIDSelector

-- | @- setVendorID:@
setVendorID :: (IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams mtrOperationalCredentialsClusterSetVIDVerificationStatementParams, IsNSNumber value) => mtrOperationalCredentialsClusterSetVIDVerificationStatementParams -> value -> IO ()
setVendorID mtrOperationalCredentialsClusterSetVIDVerificationStatementParams value =
  sendMessage mtrOperationalCredentialsClusterSetVIDVerificationStatementParams setVendorIDSelector (toNSNumber value)

-- | @- vidVerificationStatement@
vidVerificationStatement :: IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams mtrOperationalCredentialsClusterSetVIDVerificationStatementParams => mtrOperationalCredentialsClusterSetVIDVerificationStatementParams -> IO (Id NSData)
vidVerificationStatement mtrOperationalCredentialsClusterSetVIDVerificationStatementParams =
  sendMessage mtrOperationalCredentialsClusterSetVIDVerificationStatementParams vidVerificationStatementSelector

-- | @- setVidVerificationStatement:@
setVidVerificationStatement :: (IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams mtrOperationalCredentialsClusterSetVIDVerificationStatementParams, IsNSData value) => mtrOperationalCredentialsClusterSetVIDVerificationStatementParams -> value -> IO ()
setVidVerificationStatement mtrOperationalCredentialsClusterSetVIDVerificationStatementParams value =
  sendMessage mtrOperationalCredentialsClusterSetVIDVerificationStatementParams setVidVerificationStatementSelector (toNSData value)

-- | @- vvsc@
vvsc :: IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams mtrOperationalCredentialsClusterSetVIDVerificationStatementParams => mtrOperationalCredentialsClusterSetVIDVerificationStatementParams -> IO (Id NSData)
vvsc mtrOperationalCredentialsClusterSetVIDVerificationStatementParams =
  sendMessage mtrOperationalCredentialsClusterSetVIDVerificationStatementParams vvscSelector

-- | @- setVvsc:@
setVvsc :: (IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams mtrOperationalCredentialsClusterSetVIDVerificationStatementParams, IsNSData value) => mtrOperationalCredentialsClusterSetVIDVerificationStatementParams -> value -> IO ()
setVvsc mtrOperationalCredentialsClusterSetVIDVerificationStatementParams value =
  sendMessage mtrOperationalCredentialsClusterSetVIDVerificationStatementParams setVvscSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams mtrOperationalCredentialsClusterSetVIDVerificationStatementParams => mtrOperationalCredentialsClusterSetVIDVerificationStatementParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOperationalCredentialsClusterSetVIDVerificationStatementParams =
  sendMessage mtrOperationalCredentialsClusterSetVIDVerificationStatementParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams mtrOperationalCredentialsClusterSetVIDVerificationStatementParams, IsNSNumber value) => mtrOperationalCredentialsClusterSetVIDVerificationStatementParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOperationalCredentialsClusterSetVIDVerificationStatementParams value =
  sendMessage mtrOperationalCredentialsClusterSetVIDVerificationStatementParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams mtrOperationalCredentialsClusterSetVIDVerificationStatementParams => mtrOperationalCredentialsClusterSetVIDVerificationStatementParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrOperationalCredentialsClusterSetVIDVerificationStatementParams =
  sendMessage mtrOperationalCredentialsClusterSetVIDVerificationStatementParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROperationalCredentialsClusterSetVIDVerificationStatementParams mtrOperationalCredentialsClusterSetVIDVerificationStatementParams, IsNSNumber value) => mtrOperationalCredentialsClusterSetVIDVerificationStatementParams -> value -> IO ()
setServerSideProcessingTimeout mtrOperationalCredentialsClusterSetVIDVerificationStatementParams value =
  sendMessage mtrOperationalCredentialsClusterSetVIDVerificationStatementParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector '[] (Id NSNumber)
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @setVendorID:@
setVendorIDSelector :: Selector '[Id NSNumber] ()
setVendorIDSelector = mkSelector "setVendorID:"

-- | @Selector@ for @vidVerificationStatement@
vidVerificationStatementSelector :: Selector '[] (Id NSData)
vidVerificationStatementSelector = mkSelector "vidVerificationStatement"

-- | @Selector@ for @setVidVerificationStatement:@
setVidVerificationStatementSelector :: Selector '[Id NSData] ()
setVidVerificationStatementSelector = mkSelector "setVidVerificationStatement:"

-- | @Selector@ for @vvsc@
vvscSelector :: Selector '[] (Id NSData)
vvscSelector = mkSelector "vvsc"

-- | @Selector@ for @setVvsc:@
setVvscSelector :: Selector '[Id NSData] ()
setVvscSelector = mkSelector "setVvsc:"

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

