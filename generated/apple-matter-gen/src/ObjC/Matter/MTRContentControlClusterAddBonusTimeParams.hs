{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentControlClusterAddBonusTimeParams@.
module ObjC.Matter.MTRContentControlClusterAddBonusTimeParams
  ( MTRContentControlClusterAddBonusTimeParams
  , IsMTRContentControlClusterAddBonusTimeParams(..)
  , pinCode
  , setPinCode
  , bonusTime
  , setBonusTime
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , bonusTimeSelector
  , pinCodeSelector
  , serverSideProcessingTimeoutSelector
  , setBonusTimeSelector
  , setPinCodeSelector
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

-- | @- pinCode@
pinCode :: IsMTRContentControlClusterAddBonusTimeParams mtrContentControlClusterAddBonusTimeParams => mtrContentControlClusterAddBonusTimeParams -> IO (Id NSString)
pinCode mtrContentControlClusterAddBonusTimeParams =
  sendMessage mtrContentControlClusterAddBonusTimeParams pinCodeSelector

-- | @- setPinCode:@
setPinCode :: (IsMTRContentControlClusterAddBonusTimeParams mtrContentControlClusterAddBonusTimeParams, IsNSString value) => mtrContentControlClusterAddBonusTimeParams -> value -> IO ()
setPinCode mtrContentControlClusterAddBonusTimeParams value =
  sendMessage mtrContentControlClusterAddBonusTimeParams setPinCodeSelector (toNSString value)

-- | @- bonusTime@
bonusTime :: IsMTRContentControlClusterAddBonusTimeParams mtrContentControlClusterAddBonusTimeParams => mtrContentControlClusterAddBonusTimeParams -> IO (Id NSNumber)
bonusTime mtrContentControlClusterAddBonusTimeParams =
  sendMessage mtrContentControlClusterAddBonusTimeParams bonusTimeSelector

-- | @- setBonusTime:@
setBonusTime :: (IsMTRContentControlClusterAddBonusTimeParams mtrContentControlClusterAddBonusTimeParams, IsNSNumber value) => mtrContentControlClusterAddBonusTimeParams -> value -> IO ()
setBonusTime mtrContentControlClusterAddBonusTimeParams value =
  sendMessage mtrContentControlClusterAddBonusTimeParams setBonusTimeSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRContentControlClusterAddBonusTimeParams mtrContentControlClusterAddBonusTimeParams => mtrContentControlClusterAddBonusTimeParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrContentControlClusterAddBonusTimeParams =
  sendMessage mtrContentControlClusterAddBonusTimeParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRContentControlClusterAddBonusTimeParams mtrContentControlClusterAddBonusTimeParams, IsNSNumber value) => mtrContentControlClusterAddBonusTimeParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrContentControlClusterAddBonusTimeParams value =
  sendMessage mtrContentControlClusterAddBonusTimeParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRContentControlClusterAddBonusTimeParams mtrContentControlClusterAddBonusTimeParams => mtrContentControlClusterAddBonusTimeParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrContentControlClusterAddBonusTimeParams =
  sendMessage mtrContentControlClusterAddBonusTimeParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRContentControlClusterAddBonusTimeParams mtrContentControlClusterAddBonusTimeParams, IsNSNumber value) => mtrContentControlClusterAddBonusTimeParams -> value -> IO ()
setServerSideProcessingTimeout mtrContentControlClusterAddBonusTimeParams value =
  sendMessage mtrContentControlClusterAddBonusTimeParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pinCode@
pinCodeSelector :: Selector '[] (Id NSString)
pinCodeSelector = mkSelector "pinCode"

-- | @Selector@ for @setPinCode:@
setPinCodeSelector :: Selector '[Id NSString] ()
setPinCodeSelector = mkSelector "setPinCode:"

-- | @Selector@ for @bonusTime@
bonusTimeSelector :: Selector '[] (Id NSNumber)
bonusTimeSelector = mkSelector "bonusTime"

-- | @Selector@ for @setBonusTime:@
setBonusTimeSelector :: Selector '[Id NSNumber] ()
setBonusTimeSelector = mkSelector "setBonusTime:"

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

