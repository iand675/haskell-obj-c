{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestBatchHelperRequestParams@.
module ObjC.Matter.MTRUnitTestingClusterTestBatchHelperRequestParams
  ( MTRUnitTestingClusterTestBatchHelperRequestParams
  , IsMTRUnitTestingClusterTestBatchHelperRequestParams(..)
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
sleepBeforeResponseTimeMs :: IsMTRUnitTestingClusterTestBatchHelperRequestParams mtrUnitTestingClusterTestBatchHelperRequestParams => mtrUnitTestingClusterTestBatchHelperRequestParams -> IO (Id NSNumber)
sleepBeforeResponseTimeMs mtrUnitTestingClusterTestBatchHelperRequestParams =
  sendMessage mtrUnitTestingClusterTestBatchHelperRequestParams sleepBeforeResponseTimeMsSelector

-- | @- setSleepBeforeResponseTimeMs:@
setSleepBeforeResponseTimeMs :: (IsMTRUnitTestingClusterTestBatchHelperRequestParams mtrUnitTestingClusterTestBatchHelperRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestBatchHelperRequestParams -> value -> IO ()
setSleepBeforeResponseTimeMs mtrUnitTestingClusterTestBatchHelperRequestParams value =
  sendMessage mtrUnitTestingClusterTestBatchHelperRequestParams setSleepBeforeResponseTimeMsSelector (toNSNumber value)

-- | @- sizeOfResponseBuffer@
sizeOfResponseBuffer :: IsMTRUnitTestingClusterTestBatchHelperRequestParams mtrUnitTestingClusterTestBatchHelperRequestParams => mtrUnitTestingClusterTestBatchHelperRequestParams -> IO (Id NSNumber)
sizeOfResponseBuffer mtrUnitTestingClusterTestBatchHelperRequestParams =
  sendMessage mtrUnitTestingClusterTestBatchHelperRequestParams sizeOfResponseBufferSelector

-- | @- setSizeOfResponseBuffer:@
setSizeOfResponseBuffer :: (IsMTRUnitTestingClusterTestBatchHelperRequestParams mtrUnitTestingClusterTestBatchHelperRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestBatchHelperRequestParams -> value -> IO ()
setSizeOfResponseBuffer mtrUnitTestingClusterTestBatchHelperRequestParams value =
  sendMessage mtrUnitTestingClusterTestBatchHelperRequestParams setSizeOfResponseBufferSelector (toNSNumber value)

-- | @- fillCharacter@
fillCharacter :: IsMTRUnitTestingClusterTestBatchHelperRequestParams mtrUnitTestingClusterTestBatchHelperRequestParams => mtrUnitTestingClusterTestBatchHelperRequestParams -> IO (Id NSNumber)
fillCharacter mtrUnitTestingClusterTestBatchHelperRequestParams =
  sendMessage mtrUnitTestingClusterTestBatchHelperRequestParams fillCharacterSelector

-- | @- setFillCharacter:@
setFillCharacter :: (IsMTRUnitTestingClusterTestBatchHelperRequestParams mtrUnitTestingClusterTestBatchHelperRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestBatchHelperRequestParams -> value -> IO ()
setFillCharacter mtrUnitTestingClusterTestBatchHelperRequestParams value =
  sendMessage mtrUnitTestingClusterTestBatchHelperRequestParams setFillCharacterSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterTestBatchHelperRequestParams mtrUnitTestingClusterTestBatchHelperRequestParams => mtrUnitTestingClusterTestBatchHelperRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterTestBatchHelperRequestParams =
  sendMessage mtrUnitTestingClusterTestBatchHelperRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterTestBatchHelperRequestParams mtrUnitTestingClusterTestBatchHelperRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestBatchHelperRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterTestBatchHelperRequestParams value =
  sendMessage mtrUnitTestingClusterTestBatchHelperRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRUnitTestingClusterTestBatchHelperRequestParams mtrUnitTestingClusterTestBatchHelperRequestParams => mtrUnitTestingClusterTestBatchHelperRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrUnitTestingClusterTestBatchHelperRequestParams =
  sendMessage mtrUnitTestingClusterTestBatchHelperRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRUnitTestingClusterTestBatchHelperRequestParams mtrUnitTestingClusterTestBatchHelperRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestBatchHelperRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrUnitTestingClusterTestBatchHelperRequestParams value =
  sendMessage mtrUnitTestingClusterTestBatchHelperRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

