{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRContentLauncherClusterLaunchContentParams@.
module ObjC.Matter.MTRContentLauncherClusterLaunchContentParams
  ( MTRContentLauncherClusterLaunchContentParams
  , IsMTRContentLauncherClusterLaunchContentParams(..)
  , search
  , setSearch
  , autoPlay
  , setAutoPlay
  , data_
  , setData
  , playbackPreferences
  , setPlaybackPreferences
  , useCurrentContext
  , setUseCurrentContext
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , autoPlaySelector
  , dataSelector
  , playbackPreferencesSelector
  , searchSelector
  , serverSideProcessingTimeoutSelector
  , setAutoPlaySelector
  , setDataSelector
  , setPlaybackPreferencesSelector
  , setSearchSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setUseCurrentContextSelector
  , timedInvokeTimeoutMsSelector
  , useCurrentContextSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- search@
search :: IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams => mtrContentLauncherClusterLaunchContentParams -> IO (Id MTRContentLauncherClusterContentSearchStruct)
search mtrContentLauncherClusterLaunchContentParams =
  sendMessage mtrContentLauncherClusterLaunchContentParams searchSelector

-- | @- setSearch:@
setSearch :: (IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams, IsMTRContentLauncherClusterContentSearchStruct value) => mtrContentLauncherClusterLaunchContentParams -> value -> IO ()
setSearch mtrContentLauncherClusterLaunchContentParams value =
  sendMessage mtrContentLauncherClusterLaunchContentParams setSearchSelector (toMTRContentLauncherClusterContentSearchStruct value)

-- | @- autoPlay@
autoPlay :: IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams => mtrContentLauncherClusterLaunchContentParams -> IO (Id NSNumber)
autoPlay mtrContentLauncherClusterLaunchContentParams =
  sendMessage mtrContentLauncherClusterLaunchContentParams autoPlaySelector

-- | @- setAutoPlay:@
setAutoPlay :: (IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams, IsNSNumber value) => mtrContentLauncherClusterLaunchContentParams -> value -> IO ()
setAutoPlay mtrContentLauncherClusterLaunchContentParams value =
  sendMessage mtrContentLauncherClusterLaunchContentParams setAutoPlaySelector (toNSNumber value)

-- | @- data@
data_ :: IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams => mtrContentLauncherClusterLaunchContentParams -> IO (Id NSString)
data_ mtrContentLauncherClusterLaunchContentParams =
  sendMessage mtrContentLauncherClusterLaunchContentParams dataSelector

-- | @- setData:@
setData :: (IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams, IsNSString value) => mtrContentLauncherClusterLaunchContentParams -> value -> IO ()
setData mtrContentLauncherClusterLaunchContentParams value =
  sendMessage mtrContentLauncherClusterLaunchContentParams setDataSelector (toNSString value)

-- | @- playbackPreferences@
playbackPreferences :: IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams => mtrContentLauncherClusterLaunchContentParams -> IO (Id MTRContentLauncherClusterPlaybackPreferencesStruct)
playbackPreferences mtrContentLauncherClusterLaunchContentParams =
  sendMessage mtrContentLauncherClusterLaunchContentParams playbackPreferencesSelector

-- | @- setPlaybackPreferences:@
setPlaybackPreferences :: (IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams, IsMTRContentLauncherClusterPlaybackPreferencesStruct value) => mtrContentLauncherClusterLaunchContentParams -> value -> IO ()
setPlaybackPreferences mtrContentLauncherClusterLaunchContentParams value =
  sendMessage mtrContentLauncherClusterLaunchContentParams setPlaybackPreferencesSelector (toMTRContentLauncherClusterPlaybackPreferencesStruct value)

-- | @- useCurrentContext@
useCurrentContext :: IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams => mtrContentLauncherClusterLaunchContentParams -> IO (Id NSNumber)
useCurrentContext mtrContentLauncherClusterLaunchContentParams =
  sendMessage mtrContentLauncherClusterLaunchContentParams useCurrentContextSelector

-- | @- setUseCurrentContext:@
setUseCurrentContext :: (IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams, IsNSNumber value) => mtrContentLauncherClusterLaunchContentParams -> value -> IO ()
setUseCurrentContext mtrContentLauncherClusterLaunchContentParams value =
  sendMessage mtrContentLauncherClusterLaunchContentParams setUseCurrentContextSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams => mtrContentLauncherClusterLaunchContentParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrContentLauncherClusterLaunchContentParams =
  sendMessage mtrContentLauncherClusterLaunchContentParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams, IsNSNumber value) => mtrContentLauncherClusterLaunchContentParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrContentLauncherClusterLaunchContentParams value =
  sendMessage mtrContentLauncherClusterLaunchContentParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams => mtrContentLauncherClusterLaunchContentParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrContentLauncherClusterLaunchContentParams =
  sendMessage mtrContentLauncherClusterLaunchContentParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRContentLauncherClusterLaunchContentParams mtrContentLauncherClusterLaunchContentParams, IsNSNumber value) => mtrContentLauncherClusterLaunchContentParams -> value -> IO ()
setServerSideProcessingTimeout mtrContentLauncherClusterLaunchContentParams value =
  sendMessage mtrContentLauncherClusterLaunchContentParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @search@
searchSelector :: Selector '[] (Id MTRContentLauncherClusterContentSearchStruct)
searchSelector = mkSelector "search"

-- | @Selector@ for @setSearch:@
setSearchSelector :: Selector '[Id MTRContentLauncherClusterContentSearchStruct] ()
setSearchSelector = mkSelector "setSearch:"

-- | @Selector@ for @autoPlay@
autoPlaySelector :: Selector '[] (Id NSNumber)
autoPlaySelector = mkSelector "autoPlay"

-- | @Selector@ for @setAutoPlay:@
setAutoPlaySelector :: Selector '[Id NSNumber] ()
setAutoPlaySelector = mkSelector "setAutoPlay:"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSString)
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector '[Id NSString] ()
setDataSelector = mkSelector "setData:"

-- | @Selector@ for @playbackPreferences@
playbackPreferencesSelector :: Selector '[] (Id MTRContentLauncherClusterPlaybackPreferencesStruct)
playbackPreferencesSelector = mkSelector "playbackPreferences"

-- | @Selector@ for @setPlaybackPreferences:@
setPlaybackPreferencesSelector :: Selector '[Id MTRContentLauncherClusterPlaybackPreferencesStruct] ()
setPlaybackPreferencesSelector = mkSelector "setPlaybackPreferences:"

-- | @Selector@ for @useCurrentContext@
useCurrentContextSelector :: Selector '[] (Id NSNumber)
useCurrentContextSelector = mkSelector "useCurrentContext"

-- | @Selector@ for @setUseCurrentContext:@
setUseCurrentContextSelector :: Selector '[Id NSNumber] ()
setUseCurrentContextSelector = mkSelector "setUseCurrentContext:"

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

