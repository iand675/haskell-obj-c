{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterGetProgramGuideParams@.
module ObjC.Matter.MTRChannelClusterGetProgramGuideParams
  ( MTRChannelClusterGetProgramGuideParams
  , IsMTRChannelClusterGetProgramGuideParams(..)
  , startTime
  , setStartTime
  , endTime
  , setEndTime
  , channelList
  , setChannelList
  , pageToken
  , setPageToken
  , recordingFlag
  , setRecordingFlag
  , externalIDList
  , setExternalIDList
  , data_
  , setData
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , channelListSelector
  , dataSelector
  , endTimeSelector
  , externalIDListSelector
  , pageTokenSelector
  , recordingFlagSelector
  , serverSideProcessingTimeoutSelector
  , setChannelListSelector
  , setDataSelector
  , setEndTimeSelector
  , setExternalIDListSelector
  , setPageTokenSelector
  , setRecordingFlagSelector
  , setServerSideProcessingTimeoutSelector
  , setStartTimeSelector
  , setTimedInvokeTimeoutMsSelector
  , startTimeSelector
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

-- | @- startTime@
startTime :: IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams => mtrChannelClusterGetProgramGuideParams -> IO (Id NSNumber)
startTime mtrChannelClusterGetProgramGuideParams =
  sendMessage mtrChannelClusterGetProgramGuideParams startTimeSelector

-- | @- setStartTime:@
setStartTime :: (IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams, IsNSNumber value) => mtrChannelClusterGetProgramGuideParams -> value -> IO ()
setStartTime mtrChannelClusterGetProgramGuideParams value =
  sendMessage mtrChannelClusterGetProgramGuideParams setStartTimeSelector (toNSNumber value)

-- | @- endTime@
endTime :: IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams => mtrChannelClusterGetProgramGuideParams -> IO (Id NSNumber)
endTime mtrChannelClusterGetProgramGuideParams =
  sendMessage mtrChannelClusterGetProgramGuideParams endTimeSelector

-- | @- setEndTime:@
setEndTime :: (IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams, IsNSNumber value) => mtrChannelClusterGetProgramGuideParams -> value -> IO ()
setEndTime mtrChannelClusterGetProgramGuideParams value =
  sendMessage mtrChannelClusterGetProgramGuideParams setEndTimeSelector (toNSNumber value)

-- | @- channelList@
channelList :: IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams => mtrChannelClusterGetProgramGuideParams -> IO (Id NSArray)
channelList mtrChannelClusterGetProgramGuideParams =
  sendMessage mtrChannelClusterGetProgramGuideParams channelListSelector

-- | @- setChannelList:@
setChannelList :: (IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams, IsNSArray value) => mtrChannelClusterGetProgramGuideParams -> value -> IO ()
setChannelList mtrChannelClusterGetProgramGuideParams value =
  sendMessage mtrChannelClusterGetProgramGuideParams setChannelListSelector (toNSArray value)

-- | @- pageToken@
pageToken :: IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams => mtrChannelClusterGetProgramGuideParams -> IO (Id MTRChannelClusterPageTokenStruct)
pageToken mtrChannelClusterGetProgramGuideParams =
  sendMessage mtrChannelClusterGetProgramGuideParams pageTokenSelector

-- | @- setPageToken:@
setPageToken :: (IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams, IsMTRChannelClusterPageTokenStruct value) => mtrChannelClusterGetProgramGuideParams -> value -> IO ()
setPageToken mtrChannelClusterGetProgramGuideParams value =
  sendMessage mtrChannelClusterGetProgramGuideParams setPageTokenSelector (toMTRChannelClusterPageTokenStruct value)

-- | @- recordingFlag@
recordingFlag :: IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams => mtrChannelClusterGetProgramGuideParams -> IO (Id NSNumber)
recordingFlag mtrChannelClusterGetProgramGuideParams =
  sendMessage mtrChannelClusterGetProgramGuideParams recordingFlagSelector

-- | @- setRecordingFlag:@
setRecordingFlag :: (IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams, IsNSNumber value) => mtrChannelClusterGetProgramGuideParams -> value -> IO ()
setRecordingFlag mtrChannelClusterGetProgramGuideParams value =
  sendMessage mtrChannelClusterGetProgramGuideParams setRecordingFlagSelector (toNSNumber value)

-- | @- externalIDList@
externalIDList :: IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams => mtrChannelClusterGetProgramGuideParams -> IO (Id NSArray)
externalIDList mtrChannelClusterGetProgramGuideParams =
  sendMessage mtrChannelClusterGetProgramGuideParams externalIDListSelector

-- | @- setExternalIDList:@
setExternalIDList :: (IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams, IsNSArray value) => mtrChannelClusterGetProgramGuideParams -> value -> IO ()
setExternalIDList mtrChannelClusterGetProgramGuideParams value =
  sendMessage mtrChannelClusterGetProgramGuideParams setExternalIDListSelector (toNSArray value)

-- | @- data@
data_ :: IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams => mtrChannelClusterGetProgramGuideParams -> IO (Id NSData)
data_ mtrChannelClusterGetProgramGuideParams =
  sendMessage mtrChannelClusterGetProgramGuideParams dataSelector

-- | @- setData:@
setData :: (IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams, IsNSData value) => mtrChannelClusterGetProgramGuideParams -> value -> IO ()
setData mtrChannelClusterGetProgramGuideParams value =
  sendMessage mtrChannelClusterGetProgramGuideParams setDataSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams => mtrChannelClusterGetProgramGuideParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrChannelClusterGetProgramGuideParams =
  sendMessage mtrChannelClusterGetProgramGuideParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams, IsNSNumber value) => mtrChannelClusterGetProgramGuideParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrChannelClusterGetProgramGuideParams value =
  sendMessage mtrChannelClusterGetProgramGuideParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams => mtrChannelClusterGetProgramGuideParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrChannelClusterGetProgramGuideParams =
  sendMessage mtrChannelClusterGetProgramGuideParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRChannelClusterGetProgramGuideParams mtrChannelClusterGetProgramGuideParams, IsNSNumber value) => mtrChannelClusterGetProgramGuideParams -> value -> IO ()
setServerSideProcessingTimeout mtrChannelClusterGetProgramGuideParams value =
  sendMessage mtrChannelClusterGetProgramGuideParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startTime@
startTimeSelector :: Selector '[] (Id NSNumber)
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @setStartTime:@
setStartTimeSelector :: Selector '[Id NSNumber] ()
setStartTimeSelector = mkSelector "setStartTime:"

-- | @Selector@ for @endTime@
endTimeSelector :: Selector '[] (Id NSNumber)
endTimeSelector = mkSelector "endTime"

-- | @Selector@ for @setEndTime:@
setEndTimeSelector :: Selector '[Id NSNumber] ()
setEndTimeSelector = mkSelector "setEndTime:"

-- | @Selector@ for @channelList@
channelListSelector :: Selector '[] (Id NSArray)
channelListSelector = mkSelector "channelList"

-- | @Selector@ for @setChannelList:@
setChannelListSelector :: Selector '[Id NSArray] ()
setChannelListSelector = mkSelector "setChannelList:"

-- | @Selector@ for @pageToken@
pageTokenSelector :: Selector '[] (Id MTRChannelClusterPageTokenStruct)
pageTokenSelector = mkSelector "pageToken"

-- | @Selector@ for @setPageToken:@
setPageTokenSelector :: Selector '[Id MTRChannelClusterPageTokenStruct] ()
setPageTokenSelector = mkSelector "setPageToken:"

-- | @Selector@ for @recordingFlag@
recordingFlagSelector :: Selector '[] (Id NSNumber)
recordingFlagSelector = mkSelector "recordingFlag"

-- | @Selector@ for @setRecordingFlag:@
setRecordingFlagSelector :: Selector '[Id NSNumber] ()
setRecordingFlagSelector = mkSelector "setRecordingFlag:"

-- | @Selector@ for @externalIDList@
externalIDListSelector :: Selector '[] (Id NSArray)
externalIDListSelector = mkSelector "externalIDList"

-- | @Selector@ for @setExternalIDList:@
setExternalIDListSelector :: Selector '[Id NSArray] ()
setExternalIDListSelector = mkSelector "setExternalIDList:"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSData)
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector '[Id NSData] ()
setDataSelector = mkSelector "setData:"

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

