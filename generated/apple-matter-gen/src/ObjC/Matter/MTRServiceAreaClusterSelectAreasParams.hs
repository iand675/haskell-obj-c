{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRServiceAreaClusterSelectAreasParams@.
module ObjC.Matter.MTRServiceAreaClusterSelectAreasParams
  ( MTRServiceAreaClusterSelectAreasParams
  , IsMTRServiceAreaClusterSelectAreasParams(..)
  , newAreas
  , setNewAreas
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , newAreasSelector
  , serverSideProcessingTimeoutSelector
  , setNewAreasSelector
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

-- | @- newAreas@
newAreas :: IsMTRServiceAreaClusterSelectAreasParams mtrServiceAreaClusterSelectAreasParams => mtrServiceAreaClusterSelectAreasParams -> IO (Id NSArray)
newAreas mtrServiceAreaClusterSelectAreasParams =
  sendOwnedMessage mtrServiceAreaClusterSelectAreasParams newAreasSelector

-- | @- setNewAreas:@
setNewAreas :: (IsMTRServiceAreaClusterSelectAreasParams mtrServiceAreaClusterSelectAreasParams, IsNSArray value) => mtrServiceAreaClusterSelectAreasParams -> value -> IO ()
setNewAreas mtrServiceAreaClusterSelectAreasParams value =
  sendMessage mtrServiceAreaClusterSelectAreasParams setNewAreasSelector (toNSArray value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRServiceAreaClusterSelectAreasParams mtrServiceAreaClusterSelectAreasParams => mtrServiceAreaClusterSelectAreasParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrServiceAreaClusterSelectAreasParams =
  sendMessage mtrServiceAreaClusterSelectAreasParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRServiceAreaClusterSelectAreasParams mtrServiceAreaClusterSelectAreasParams, IsNSNumber value) => mtrServiceAreaClusterSelectAreasParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrServiceAreaClusterSelectAreasParams value =
  sendMessage mtrServiceAreaClusterSelectAreasParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRServiceAreaClusterSelectAreasParams mtrServiceAreaClusterSelectAreasParams => mtrServiceAreaClusterSelectAreasParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrServiceAreaClusterSelectAreasParams =
  sendMessage mtrServiceAreaClusterSelectAreasParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRServiceAreaClusterSelectAreasParams mtrServiceAreaClusterSelectAreasParams, IsNSNumber value) => mtrServiceAreaClusterSelectAreasParams -> value -> IO ()
setServerSideProcessingTimeout mtrServiceAreaClusterSelectAreasParams value =
  sendMessage mtrServiceAreaClusterSelectAreasParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newAreas@
newAreasSelector :: Selector '[] (Id NSArray)
newAreasSelector = mkSelector "newAreas"

-- | @Selector@ for @setNewAreas:@
setNewAreasSelector :: Selector '[Id NSArray] ()
setNewAreasSelector = mkSelector "setNewAreas:"

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

