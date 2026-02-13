{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentAppObserverClusterContentAppMessageParams@.
module ObjC.Matter.MTRContentAppObserverClusterContentAppMessageParams
  ( MTRContentAppObserverClusterContentAppMessageParams
  , IsMTRContentAppObserverClusterContentAppMessageParams(..)
  , data_
  , setData
  , encodingHint
  , setEncodingHint
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , dataSelector
  , encodingHintSelector
  , serverSideProcessingTimeoutSelector
  , setDataSelector
  , setEncodingHintSelector
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

-- | @- data@
data_ :: IsMTRContentAppObserverClusterContentAppMessageParams mtrContentAppObserverClusterContentAppMessageParams => mtrContentAppObserverClusterContentAppMessageParams -> IO (Id NSString)
data_ mtrContentAppObserverClusterContentAppMessageParams =
  sendMessage mtrContentAppObserverClusterContentAppMessageParams dataSelector

-- | @- setData:@
setData :: (IsMTRContentAppObserverClusterContentAppMessageParams mtrContentAppObserverClusterContentAppMessageParams, IsNSString value) => mtrContentAppObserverClusterContentAppMessageParams -> value -> IO ()
setData mtrContentAppObserverClusterContentAppMessageParams value =
  sendMessage mtrContentAppObserverClusterContentAppMessageParams setDataSelector (toNSString value)

-- | @- encodingHint@
encodingHint :: IsMTRContentAppObserverClusterContentAppMessageParams mtrContentAppObserverClusterContentAppMessageParams => mtrContentAppObserverClusterContentAppMessageParams -> IO (Id NSString)
encodingHint mtrContentAppObserverClusterContentAppMessageParams =
  sendMessage mtrContentAppObserverClusterContentAppMessageParams encodingHintSelector

-- | @- setEncodingHint:@
setEncodingHint :: (IsMTRContentAppObserverClusterContentAppMessageParams mtrContentAppObserverClusterContentAppMessageParams, IsNSString value) => mtrContentAppObserverClusterContentAppMessageParams -> value -> IO ()
setEncodingHint mtrContentAppObserverClusterContentAppMessageParams value =
  sendMessage mtrContentAppObserverClusterContentAppMessageParams setEncodingHintSelector (toNSString value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRContentAppObserverClusterContentAppMessageParams mtrContentAppObserverClusterContentAppMessageParams => mtrContentAppObserverClusterContentAppMessageParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrContentAppObserverClusterContentAppMessageParams =
  sendMessage mtrContentAppObserverClusterContentAppMessageParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRContentAppObserverClusterContentAppMessageParams mtrContentAppObserverClusterContentAppMessageParams, IsNSNumber value) => mtrContentAppObserverClusterContentAppMessageParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrContentAppObserverClusterContentAppMessageParams value =
  sendMessage mtrContentAppObserverClusterContentAppMessageParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRContentAppObserverClusterContentAppMessageParams mtrContentAppObserverClusterContentAppMessageParams => mtrContentAppObserverClusterContentAppMessageParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrContentAppObserverClusterContentAppMessageParams =
  sendMessage mtrContentAppObserverClusterContentAppMessageParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRContentAppObserverClusterContentAppMessageParams mtrContentAppObserverClusterContentAppMessageParams, IsNSNumber value) => mtrContentAppObserverClusterContentAppMessageParams -> value -> IO ()
setServerSideProcessingTimeout mtrContentAppObserverClusterContentAppMessageParams value =
  sendMessage mtrContentAppObserverClusterContentAppMessageParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSString)
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector '[Id NSString] ()
setDataSelector = mkSelector "setData:"

-- | @Selector@ for @encodingHint@
encodingHintSelector :: Selector '[] (Id NSString)
encodingHintSelector = mkSelector "encodingHint"

-- | @Selector@ for @setEncodingHint:@
setEncodingHintSelector :: Selector '[Id NSString] ()
setEncodingHintSelector = mkSelector "setEncodingHint:"

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

