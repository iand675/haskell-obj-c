{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommodityTariffClusterGetDayEntryParams@.
module ObjC.Matter.MTRCommodityTariffClusterGetDayEntryParams
  ( MTRCommodityTariffClusterGetDayEntryParams
  , IsMTRCommodityTariffClusterGetDayEntryParams(..)
  , dayEntryID
  , setDayEntryID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , dayEntryIDSelector
  , serverSideProcessingTimeoutSelector
  , setDayEntryIDSelector
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

-- | @- dayEntryID@
dayEntryID :: IsMTRCommodityTariffClusterGetDayEntryParams mtrCommodityTariffClusterGetDayEntryParams => mtrCommodityTariffClusterGetDayEntryParams -> IO (Id NSNumber)
dayEntryID mtrCommodityTariffClusterGetDayEntryParams =
  sendMessage mtrCommodityTariffClusterGetDayEntryParams dayEntryIDSelector

-- | @- setDayEntryID:@
setDayEntryID :: (IsMTRCommodityTariffClusterGetDayEntryParams mtrCommodityTariffClusterGetDayEntryParams, IsNSNumber value) => mtrCommodityTariffClusterGetDayEntryParams -> value -> IO ()
setDayEntryID mtrCommodityTariffClusterGetDayEntryParams value =
  sendMessage mtrCommodityTariffClusterGetDayEntryParams setDayEntryIDSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRCommodityTariffClusterGetDayEntryParams mtrCommodityTariffClusterGetDayEntryParams => mtrCommodityTariffClusterGetDayEntryParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrCommodityTariffClusterGetDayEntryParams =
  sendMessage mtrCommodityTariffClusterGetDayEntryParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRCommodityTariffClusterGetDayEntryParams mtrCommodityTariffClusterGetDayEntryParams, IsNSNumber value) => mtrCommodityTariffClusterGetDayEntryParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrCommodityTariffClusterGetDayEntryParams value =
  sendMessage mtrCommodityTariffClusterGetDayEntryParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRCommodityTariffClusterGetDayEntryParams mtrCommodityTariffClusterGetDayEntryParams => mtrCommodityTariffClusterGetDayEntryParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrCommodityTariffClusterGetDayEntryParams =
  sendMessage mtrCommodityTariffClusterGetDayEntryParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRCommodityTariffClusterGetDayEntryParams mtrCommodityTariffClusterGetDayEntryParams, IsNSNumber value) => mtrCommodityTariffClusterGetDayEntryParams -> value -> IO ()
setServerSideProcessingTimeout mtrCommodityTariffClusterGetDayEntryParams value =
  sendMessage mtrCommodityTariffClusterGetDayEntryParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dayEntryID@
dayEntryIDSelector :: Selector '[] (Id NSNumber)
dayEntryIDSelector = mkSelector "dayEntryID"

-- | @Selector@ for @setDayEntryID:@
setDayEntryIDSelector :: Selector '[Id NSNumber] ()
setDayEntryIDSelector = mkSelector "setDayEntryID:"

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

