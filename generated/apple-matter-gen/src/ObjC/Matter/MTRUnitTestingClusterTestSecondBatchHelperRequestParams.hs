{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestSecondBatchHelperRequestParams@.
module ObjC.Matter.MTRUnitTestingClusterTestSecondBatchHelperRequestParams
  ( MTRUnitTestingClusterTestSecondBatchHelperRequestParams
  , IsMTRUnitTestingClusterTestSecondBatchHelperRequestParams(..)
  , sleepBeforeResponseTimeMs
  , setSleepBeforeResponseTimeMs
  , sizeOfResponseBuffer
  , setSizeOfResponseBuffer
  , fillCharacter
  , setFillCharacter
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , fillCharacterSelector
  , serverSideProcessingTimeoutSelector
  , setFillCharacterSelector
  , setServerSideProcessingTimeoutSelector
  , setSizeOfResponseBufferSelector
  , setSleepBeforeResponseTimeMsSelector
  , setTimedInvokeTimeoutMsSelector
  , sizeOfResponseBufferSelector
  , sleepBeforeResponseTimeMsSelector
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

-- | @- sleepBeforeResponseTimeMs@
sleepBeforeResponseTimeMs :: IsMTRUnitTestingClusterTestSecondBatchHelperRequestParams mtrUnitTestingClusterTestSecondBatchHelperRequestParams => mtrUnitTestingClusterTestSecondBatchHelperRequestParams -> IO (Id NSNumber)
sleepBeforeResponseTimeMs mtrUnitTestingClusterTestSecondBatchHelperRequestParams =
  sendMessage mtrUnitTestingClusterTestSecondBatchHelperRequestParams sleepBeforeResponseTimeMsSelector

-- | @- setSleepBeforeResponseTimeMs:@
setSleepBeforeResponseTimeMs :: (IsMTRUnitTestingClusterTestSecondBatchHelperRequestParams mtrUnitTestingClusterTestSecondBatchHelperRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestSecondBatchHelperRequestParams -> value -> IO ()
setSleepBeforeResponseTimeMs mtrUnitTestingClusterTestSecondBatchHelperRequestParams value =
  sendMessage mtrUnitTestingClusterTestSecondBatchHelperRequestParams setSleepBeforeResponseTimeMsSelector (toNSNumber value)

-- | @- sizeOfResponseBuffer@
sizeOfResponseBuffer :: IsMTRUnitTestingClusterTestSecondBatchHelperRequestParams mtrUnitTestingClusterTestSecondBatchHelperRequestParams => mtrUnitTestingClusterTestSecondBatchHelperRequestParams -> IO (Id NSNumber)
sizeOfResponseBuffer mtrUnitTestingClusterTestSecondBatchHelperRequestParams =
  sendMessage mtrUnitTestingClusterTestSecondBatchHelperRequestParams sizeOfResponseBufferSelector

-- | @- setSizeOfResponseBuffer:@
setSizeOfResponseBuffer :: (IsMTRUnitTestingClusterTestSecondBatchHelperRequestParams mtrUnitTestingClusterTestSecondBatchHelperRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestSecondBatchHelperRequestParams -> value -> IO ()
setSizeOfResponseBuffer mtrUnitTestingClusterTestSecondBatchHelperRequestParams value =
  sendMessage mtrUnitTestingClusterTestSecondBatchHelperRequestParams setSizeOfResponseBufferSelector (toNSNumber value)

-- | @- fillCharacter@
fillCharacter :: IsMTRUnitTestingClusterTestSecondBatchHelperRequestParams mtrUnitTestingClusterTestSecondBatchHelperRequestParams => mtrUnitTestingClusterTestSecondBatchHelperRequestParams -> IO (Id NSNumber)
fillCharacter mtrUnitTestingClusterTestSecondBatchHelperRequestParams =
  sendMessage mtrUnitTestingClusterTestSecondBatchHelperRequestParams fillCharacterSelector

-- | @- setFillCharacter:@
setFillCharacter :: (IsMTRUnitTestingClusterTestSecondBatchHelperRequestParams mtrUnitTestingClusterTestSecondBatchHelperRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestSecondBatchHelperRequestParams -> value -> IO ()
setFillCharacter mtrUnitTestingClusterTestSecondBatchHelperRequestParams value =
  sendMessage mtrUnitTestingClusterTestSecondBatchHelperRequestParams setFillCharacterSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterTestSecondBatchHelperRequestParams mtrUnitTestingClusterTestSecondBatchHelperRequestParams => mtrUnitTestingClusterTestSecondBatchHelperRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterTestSecondBatchHelperRequestParams =
  sendMessage mtrUnitTestingClusterTestSecondBatchHelperRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterTestSecondBatchHelperRequestParams mtrUnitTestingClusterTestSecondBatchHelperRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestSecondBatchHelperRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterTestSecondBatchHelperRequestParams value =
  sendMessage mtrUnitTestingClusterTestSecondBatchHelperRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRUnitTestingClusterTestSecondBatchHelperRequestParams mtrUnitTestingClusterTestSecondBatchHelperRequestParams => mtrUnitTestingClusterTestSecondBatchHelperRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrUnitTestingClusterTestSecondBatchHelperRequestParams =
  sendMessage mtrUnitTestingClusterTestSecondBatchHelperRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRUnitTestingClusterTestSecondBatchHelperRequestParams mtrUnitTestingClusterTestSecondBatchHelperRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestSecondBatchHelperRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrUnitTestingClusterTestSecondBatchHelperRequestParams value =
  sendMessage mtrUnitTestingClusterTestSecondBatchHelperRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sleepBeforeResponseTimeMs@
sleepBeforeResponseTimeMsSelector :: Selector '[] (Id NSNumber)
sleepBeforeResponseTimeMsSelector = mkSelector "sleepBeforeResponseTimeMs"

-- | @Selector@ for @setSleepBeforeResponseTimeMs:@
setSleepBeforeResponseTimeMsSelector :: Selector '[Id NSNumber] ()
setSleepBeforeResponseTimeMsSelector = mkSelector "setSleepBeforeResponseTimeMs:"

-- | @Selector@ for @sizeOfResponseBuffer@
sizeOfResponseBufferSelector :: Selector '[] (Id NSNumber)
sizeOfResponseBufferSelector = mkSelector "sizeOfResponseBuffer"

-- | @Selector@ for @setSizeOfResponseBuffer:@
setSizeOfResponseBufferSelector :: Selector '[Id NSNumber] ()
setSizeOfResponseBufferSelector = mkSelector "setSizeOfResponseBuffer:"

-- | @Selector@ for @fillCharacter@
fillCharacterSelector :: Selector '[] (Id NSNumber)
fillCharacterSelector = mkSelector "fillCharacter"

-- | @Selector@ for @setFillCharacter:@
setFillCharacterSelector :: Selector '[Id NSNumber] ()
setFillCharacterSelector = mkSelector "setFillCharacter:"

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

