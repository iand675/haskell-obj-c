{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalCredentialsClusterAddNOCParams@.
module ObjC.Matter.MTROperationalCredentialsClusterAddNOCParams
  ( MTROperationalCredentialsClusterAddNOCParams
  , IsMTROperationalCredentialsClusterAddNOCParams(..)
  , nocValue
  , setNocValue
  , icacValue
  , setIcacValue
  , ipkValue
  , setIpkValue
  , caseAdminSubject
  , setCaseAdminSubject
  , adminVendorId
  , setAdminVendorId
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , adminVendorIdSelector
  , caseAdminSubjectSelector
  , icacValueSelector
  , ipkValueSelector
  , nocValueSelector
  , serverSideProcessingTimeoutSelector
  , setAdminVendorIdSelector
  , setCaseAdminSubjectSelector
  , setIcacValueSelector
  , setIpkValueSelector
  , setNocValueSelector
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

-- | @- nocValue@
nocValue :: IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams => mtrOperationalCredentialsClusterAddNOCParams -> IO (Id NSData)
nocValue mtrOperationalCredentialsClusterAddNOCParams =
  sendMessage mtrOperationalCredentialsClusterAddNOCParams nocValueSelector

-- | @- setNocValue:@
setNocValue :: (IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams, IsNSData value) => mtrOperationalCredentialsClusterAddNOCParams -> value -> IO ()
setNocValue mtrOperationalCredentialsClusterAddNOCParams value =
  sendMessage mtrOperationalCredentialsClusterAddNOCParams setNocValueSelector (toNSData value)

-- | @- icacValue@
icacValue :: IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams => mtrOperationalCredentialsClusterAddNOCParams -> IO (Id NSData)
icacValue mtrOperationalCredentialsClusterAddNOCParams =
  sendMessage mtrOperationalCredentialsClusterAddNOCParams icacValueSelector

-- | @- setIcacValue:@
setIcacValue :: (IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams, IsNSData value) => mtrOperationalCredentialsClusterAddNOCParams -> value -> IO ()
setIcacValue mtrOperationalCredentialsClusterAddNOCParams value =
  sendMessage mtrOperationalCredentialsClusterAddNOCParams setIcacValueSelector (toNSData value)

-- | @- ipkValue@
ipkValue :: IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams => mtrOperationalCredentialsClusterAddNOCParams -> IO (Id NSData)
ipkValue mtrOperationalCredentialsClusterAddNOCParams =
  sendMessage mtrOperationalCredentialsClusterAddNOCParams ipkValueSelector

-- | @- setIpkValue:@
setIpkValue :: (IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams, IsNSData value) => mtrOperationalCredentialsClusterAddNOCParams -> value -> IO ()
setIpkValue mtrOperationalCredentialsClusterAddNOCParams value =
  sendMessage mtrOperationalCredentialsClusterAddNOCParams setIpkValueSelector (toNSData value)

-- | @- caseAdminSubject@
caseAdminSubject :: IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams => mtrOperationalCredentialsClusterAddNOCParams -> IO (Id NSNumber)
caseAdminSubject mtrOperationalCredentialsClusterAddNOCParams =
  sendMessage mtrOperationalCredentialsClusterAddNOCParams caseAdminSubjectSelector

-- | @- setCaseAdminSubject:@
setCaseAdminSubject :: (IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams, IsNSNumber value) => mtrOperationalCredentialsClusterAddNOCParams -> value -> IO ()
setCaseAdminSubject mtrOperationalCredentialsClusterAddNOCParams value =
  sendMessage mtrOperationalCredentialsClusterAddNOCParams setCaseAdminSubjectSelector (toNSNumber value)

-- | @- adminVendorId@
adminVendorId :: IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams => mtrOperationalCredentialsClusterAddNOCParams -> IO (Id NSNumber)
adminVendorId mtrOperationalCredentialsClusterAddNOCParams =
  sendMessage mtrOperationalCredentialsClusterAddNOCParams adminVendorIdSelector

-- | @- setAdminVendorId:@
setAdminVendorId :: (IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams, IsNSNumber value) => mtrOperationalCredentialsClusterAddNOCParams -> value -> IO ()
setAdminVendorId mtrOperationalCredentialsClusterAddNOCParams value =
  sendMessage mtrOperationalCredentialsClusterAddNOCParams setAdminVendorIdSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams => mtrOperationalCredentialsClusterAddNOCParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrOperationalCredentialsClusterAddNOCParams =
  sendMessage mtrOperationalCredentialsClusterAddNOCParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams, IsNSNumber value) => mtrOperationalCredentialsClusterAddNOCParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrOperationalCredentialsClusterAddNOCParams value =
  sendMessage mtrOperationalCredentialsClusterAddNOCParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams => mtrOperationalCredentialsClusterAddNOCParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrOperationalCredentialsClusterAddNOCParams =
  sendMessage mtrOperationalCredentialsClusterAddNOCParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTROperationalCredentialsClusterAddNOCParams mtrOperationalCredentialsClusterAddNOCParams, IsNSNumber value) => mtrOperationalCredentialsClusterAddNOCParams -> value -> IO ()
setServerSideProcessingTimeout mtrOperationalCredentialsClusterAddNOCParams value =
  sendMessage mtrOperationalCredentialsClusterAddNOCParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nocValue@
nocValueSelector :: Selector '[] (Id NSData)
nocValueSelector = mkSelector "nocValue"

-- | @Selector@ for @setNocValue:@
setNocValueSelector :: Selector '[Id NSData] ()
setNocValueSelector = mkSelector "setNocValue:"

-- | @Selector@ for @icacValue@
icacValueSelector :: Selector '[] (Id NSData)
icacValueSelector = mkSelector "icacValue"

-- | @Selector@ for @setIcacValue:@
setIcacValueSelector :: Selector '[Id NSData] ()
setIcacValueSelector = mkSelector "setIcacValue:"

-- | @Selector@ for @ipkValue@
ipkValueSelector :: Selector '[] (Id NSData)
ipkValueSelector = mkSelector "ipkValue"

-- | @Selector@ for @setIpkValue:@
setIpkValueSelector :: Selector '[Id NSData] ()
setIpkValueSelector = mkSelector "setIpkValue:"

-- | @Selector@ for @caseAdminSubject@
caseAdminSubjectSelector :: Selector '[] (Id NSNumber)
caseAdminSubjectSelector = mkSelector "caseAdminSubject"

-- | @Selector@ for @setCaseAdminSubject:@
setCaseAdminSubjectSelector :: Selector '[Id NSNumber] ()
setCaseAdminSubjectSelector = mkSelector "setCaseAdminSubject:"

-- | @Selector@ for @adminVendorId@
adminVendorIdSelector :: Selector '[] (Id NSNumber)
adminVendorIdSelector = mkSelector "adminVendorId"

-- | @Selector@ for @setAdminVendorId:@
setAdminVendorIdSelector :: Selector '[Id NSNumber] ()
setAdminVendorIdSelector = mkSelector "setAdminVendorId:"

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

