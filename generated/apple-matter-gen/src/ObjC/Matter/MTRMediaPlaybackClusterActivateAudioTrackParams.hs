{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMediaPlaybackClusterActivateAudioTrackParams@.
module ObjC.Matter.MTRMediaPlaybackClusterActivateAudioTrackParams
  ( MTRMediaPlaybackClusterActivateAudioTrackParams
  , IsMTRMediaPlaybackClusterActivateAudioTrackParams(..)
  , trackID
  , setTrackID
  , audioOutputIndex
  , setAudioOutputIndex
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , audioOutputIndexSelector
  , serverSideProcessingTimeoutSelector
  , setAudioOutputIndexSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setTrackIDSelector
  , timedInvokeTimeoutMsSelector
  , trackIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- trackID@
trackID :: IsMTRMediaPlaybackClusterActivateAudioTrackParams mtrMediaPlaybackClusterActivateAudioTrackParams => mtrMediaPlaybackClusterActivateAudioTrackParams -> IO (Id NSString)
trackID mtrMediaPlaybackClusterActivateAudioTrackParams =
  sendMessage mtrMediaPlaybackClusterActivateAudioTrackParams trackIDSelector

-- | @- setTrackID:@
setTrackID :: (IsMTRMediaPlaybackClusterActivateAudioTrackParams mtrMediaPlaybackClusterActivateAudioTrackParams, IsNSString value) => mtrMediaPlaybackClusterActivateAudioTrackParams -> value -> IO ()
setTrackID mtrMediaPlaybackClusterActivateAudioTrackParams value =
  sendMessage mtrMediaPlaybackClusterActivateAudioTrackParams setTrackIDSelector (toNSString value)

-- | @- audioOutputIndex@
audioOutputIndex :: IsMTRMediaPlaybackClusterActivateAudioTrackParams mtrMediaPlaybackClusterActivateAudioTrackParams => mtrMediaPlaybackClusterActivateAudioTrackParams -> IO (Id NSNumber)
audioOutputIndex mtrMediaPlaybackClusterActivateAudioTrackParams =
  sendMessage mtrMediaPlaybackClusterActivateAudioTrackParams audioOutputIndexSelector

-- | @- setAudioOutputIndex:@
setAudioOutputIndex :: (IsMTRMediaPlaybackClusterActivateAudioTrackParams mtrMediaPlaybackClusterActivateAudioTrackParams, IsNSNumber value) => mtrMediaPlaybackClusterActivateAudioTrackParams -> value -> IO ()
setAudioOutputIndex mtrMediaPlaybackClusterActivateAudioTrackParams value =
  sendMessage mtrMediaPlaybackClusterActivateAudioTrackParams setAudioOutputIndexSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRMediaPlaybackClusterActivateAudioTrackParams mtrMediaPlaybackClusterActivateAudioTrackParams => mtrMediaPlaybackClusterActivateAudioTrackParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrMediaPlaybackClusterActivateAudioTrackParams =
  sendMessage mtrMediaPlaybackClusterActivateAudioTrackParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRMediaPlaybackClusterActivateAudioTrackParams mtrMediaPlaybackClusterActivateAudioTrackParams, IsNSNumber value) => mtrMediaPlaybackClusterActivateAudioTrackParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrMediaPlaybackClusterActivateAudioTrackParams value =
  sendMessage mtrMediaPlaybackClusterActivateAudioTrackParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRMediaPlaybackClusterActivateAudioTrackParams mtrMediaPlaybackClusterActivateAudioTrackParams => mtrMediaPlaybackClusterActivateAudioTrackParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrMediaPlaybackClusterActivateAudioTrackParams =
  sendMessage mtrMediaPlaybackClusterActivateAudioTrackParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRMediaPlaybackClusterActivateAudioTrackParams mtrMediaPlaybackClusterActivateAudioTrackParams, IsNSNumber value) => mtrMediaPlaybackClusterActivateAudioTrackParams -> value -> IO ()
setServerSideProcessingTimeout mtrMediaPlaybackClusterActivateAudioTrackParams value =
  sendMessage mtrMediaPlaybackClusterActivateAudioTrackParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @trackID@
trackIDSelector :: Selector '[] (Id NSString)
trackIDSelector = mkSelector "trackID"

-- | @Selector@ for @setTrackID:@
setTrackIDSelector :: Selector '[Id NSString] ()
setTrackIDSelector = mkSelector "setTrackID:"

-- | @Selector@ for @audioOutputIndex@
audioOutputIndexSelector :: Selector '[] (Id NSNumber)
audioOutputIndexSelector = mkSelector "audioOutputIndex"

-- | @Selector@ for @setAudioOutputIndex:@
setAudioOutputIndexSelector :: Selector '[Id NSNumber] ()
setAudioOutputIndexSelector = mkSelector "setAudioOutputIndex:"

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

