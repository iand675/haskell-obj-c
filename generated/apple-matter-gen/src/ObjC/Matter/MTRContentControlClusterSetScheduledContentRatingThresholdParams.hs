{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentControlClusterSetScheduledContentRatingThresholdParams@.
module ObjC.Matter.MTRContentControlClusterSetScheduledContentRatingThresholdParams
  ( MTRContentControlClusterSetScheduledContentRatingThresholdParams
  , IsMTRContentControlClusterSetScheduledContentRatingThresholdParams(..)
  , rating
  , setRating
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , ratingSelector
  , serverSideProcessingTimeoutSelector
  , setRatingSelector
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

-- | @- rating@
rating :: IsMTRContentControlClusterSetScheduledContentRatingThresholdParams mtrContentControlClusterSetScheduledContentRatingThresholdParams => mtrContentControlClusterSetScheduledContentRatingThresholdParams -> IO (Id NSString)
rating mtrContentControlClusterSetScheduledContentRatingThresholdParams =
  sendMessage mtrContentControlClusterSetScheduledContentRatingThresholdParams ratingSelector

-- | @- setRating:@
setRating :: (IsMTRContentControlClusterSetScheduledContentRatingThresholdParams mtrContentControlClusterSetScheduledContentRatingThresholdParams, IsNSString value) => mtrContentControlClusterSetScheduledContentRatingThresholdParams -> value -> IO ()
setRating mtrContentControlClusterSetScheduledContentRatingThresholdParams value =
  sendMessage mtrContentControlClusterSetScheduledContentRatingThresholdParams setRatingSelector (toNSString value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRContentControlClusterSetScheduledContentRatingThresholdParams mtrContentControlClusterSetScheduledContentRatingThresholdParams => mtrContentControlClusterSetScheduledContentRatingThresholdParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrContentControlClusterSetScheduledContentRatingThresholdParams =
  sendMessage mtrContentControlClusterSetScheduledContentRatingThresholdParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRContentControlClusterSetScheduledContentRatingThresholdParams mtrContentControlClusterSetScheduledContentRatingThresholdParams, IsNSNumber value) => mtrContentControlClusterSetScheduledContentRatingThresholdParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrContentControlClusterSetScheduledContentRatingThresholdParams value =
  sendMessage mtrContentControlClusterSetScheduledContentRatingThresholdParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRContentControlClusterSetScheduledContentRatingThresholdParams mtrContentControlClusterSetScheduledContentRatingThresholdParams => mtrContentControlClusterSetScheduledContentRatingThresholdParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrContentControlClusterSetScheduledContentRatingThresholdParams =
  sendMessage mtrContentControlClusterSetScheduledContentRatingThresholdParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRContentControlClusterSetScheduledContentRatingThresholdParams mtrContentControlClusterSetScheduledContentRatingThresholdParams, IsNSNumber value) => mtrContentControlClusterSetScheduledContentRatingThresholdParams -> value -> IO ()
setServerSideProcessingTimeout mtrContentControlClusterSetScheduledContentRatingThresholdParams value =
  sendMessage mtrContentControlClusterSetScheduledContentRatingThresholdParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rating@
ratingSelector :: Selector '[] (Id NSString)
ratingSelector = mkSelector "rating"

-- | @Selector@ for @setRating:@
setRatingSelector :: Selector '[Id NSString] ()
setRatingSelector = mkSelector "setRating:"

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

