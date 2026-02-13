{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommissionerControlClusterRequestCommissioningApprovalParams@.
module ObjC.Matter.MTRCommissionerControlClusterRequestCommissioningApprovalParams
  ( MTRCommissionerControlClusterRequestCommissioningApprovalParams
  , IsMTRCommissionerControlClusterRequestCommissioningApprovalParams(..)
  , requestID
  , setRequestID
  , vendorID
  , setVendorID
  , productID
  , setProductID
  , label
  , setLabel
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , labelSelector
  , productIDSelector
  , requestIDSelector
  , serverSideProcessingTimeoutSelector
  , setLabelSelector
  , setProductIDSelector
  , setRequestIDSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setVendorIDSelector
  , timedInvokeTimeoutMsSelector
  , vendorIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- requestID@
requestID :: IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> IO (Id NSNumber)
requestID mtrCommissionerControlClusterRequestCommissioningApprovalParams =
  sendMessage mtrCommissionerControlClusterRequestCommissioningApprovalParams requestIDSelector

-- | @- setRequestID:@
setRequestID :: (IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams, IsNSNumber value) => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> value -> IO ()
setRequestID mtrCommissionerControlClusterRequestCommissioningApprovalParams value =
  sendMessage mtrCommissionerControlClusterRequestCommissioningApprovalParams setRequestIDSelector (toNSNumber value)

-- | @- vendorID@
vendorID :: IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> IO (Id NSNumber)
vendorID mtrCommissionerControlClusterRequestCommissioningApprovalParams =
  sendMessage mtrCommissionerControlClusterRequestCommissioningApprovalParams vendorIDSelector

-- | @- setVendorID:@
setVendorID :: (IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams, IsNSNumber value) => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> value -> IO ()
setVendorID mtrCommissionerControlClusterRequestCommissioningApprovalParams value =
  sendMessage mtrCommissionerControlClusterRequestCommissioningApprovalParams setVendorIDSelector (toNSNumber value)

-- | @- productID@
productID :: IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> IO (Id NSNumber)
productID mtrCommissionerControlClusterRequestCommissioningApprovalParams =
  sendMessage mtrCommissionerControlClusterRequestCommissioningApprovalParams productIDSelector

-- | @- setProductID:@
setProductID :: (IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams, IsNSNumber value) => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> value -> IO ()
setProductID mtrCommissionerControlClusterRequestCommissioningApprovalParams value =
  sendMessage mtrCommissionerControlClusterRequestCommissioningApprovalParams setProductIDSelector (toNSNumber value)

-- | @- label@
label :: IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> IO (Id NSString)
label mtrCommissionerControlClusterRequestCommissioningApprovalParams =
  sendMessage mtrCommissionerControlClusterRequestCommissioningApprovalParams labelSelector

-- | @- setLabel:@
setLabel :: (IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams, IsNSString value) => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> value -> IO ()
setLabel mtrCommissionerControlClusterRequestCommissioningApprovalParams value =
  sendMessage mtrCommissionerControlClusterRequestCommissioningApprovalParams setLabelSelector (toNSString value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCommissionerControlClusterRequestCommissioningApprovalParams =
  sendMessage mtrCommissionerControlClusterRequestCommissioningApprovalParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams, IsNSNumber value) => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCommissionerControlClusterRequestCommissioningApprovalParams value =
  sendMessage mtrCommissionerControlClusterRequestCommissioningApprovalParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCommissionerControlClusterRequestCommissioningApprovalParams =
  sendMessage mtrCommissionerControlClusterRequestCommissioningApprovalParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCommissionerControlClusterRequestCommissioningApprovalParams mtrCommissionerControlClusterRequestCommissioningApprovalParams, IsNSNumber value) => mtrCommissionerControlClusterRequestCommissioningApprovalParams -> value -> IO ()
setServerSideProcessingTimeout mtrCommissionerControlClusterRequestCommissioningApprovalParams value =
  sendMessage mtrCommissionerControlClusterRequestCommissioningApprovalParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestID@
requestIDSelector :: Selector '[] (Id NSNumber)
requestIDSelector = mkSelector "requestID"

-- | @Selector@ for @setRequestID:@
setRequestIDSelector :: Selector '[Id NSNumber] ()
setRequestIDSelector = mkSelector "setRequestID:"

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector '[] (Id NSNumber)
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @setVendorID:@
setVendorIDSelector :: Selector '[Id NSNumber] ()
setVendorIDSelector = mkSelector "setVendorID:"

-- | @Selector@ for @productID@
productIDSelector :: Selector '[] (Id NSNumber)
productIDSelector = mkSelector "productID"

-- | @Selector@ for @setProductID:@
setProductIDSelector :: Selector '[Id NSNumber] ()
setProductIDSelector = mkSelector "setProductID:"

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

