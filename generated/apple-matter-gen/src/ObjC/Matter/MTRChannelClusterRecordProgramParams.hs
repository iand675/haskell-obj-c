{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterRecordProgramParams@.
module ObjC.Matter.MTRChannelClusterRecordProgramParams
  ( MTRChannelClusterRecordProgramParams
  , IsMTRChannelClusterRecordProgramParams(..)
  , programIdentifier
  , setProgramIdentifier
  , shouldRecordSeries
  , setShouldRecordSeries
  , externalIDList
  , setExternalIDList
  , data_
  , setData
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , dataSelector
  , externalIDListSelector
  , programIdentifierSelector
  , serverSideProcessingTimeoutSelector
  , setDataSelector
  , setExternalIDListSelector
  , setProgramIdentifierSelector
  , setServerSideProcessingTimeoutSelector
  , setShouldRecordSeriesSelector
  , setTimedInvokeTimeoutMsSelector
  , shouldRecordSeriesSelector
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

-- | @- programIdentifier@
programIdentifier :: IsMTRChannelClusterRecordProgramParams mtrChannelClusterRecordProgramParams => mtrChannelClusterRecordProgramParams -> IO (Id NSString)
programIdentifier mtrChannelClusterRecordProgramParams =
  sendMessage mtrChannelClusterRecordProgramParams programIdentifierSelector

-- | @- setProgramIdentifier:@
setProgramIdentifier :: (IsMTRChannelClusterRecordProgramParams mtrChannelClusterRecordProgramParams, IsNSString value) => mtrChannelClusterRecordProgramParams -> value -> IO ()
setProgramIdentifier mtrChannelClusterRecordProgramParams value =
  sendMessage mtrChannelClusterRecordProgramParams setProgramIdentifierSelector (toNSString value)

-- | @- shouldRecordSeries@
shouldRecordSeries :: IsMTRChannelClusterRecordProgramParams mtrChannelClusterRecordProgramParams => mtrChannelClusterRecordProgramParams -> IO (Id NSNumber)
shouldRecordSeries mtrChannelClusterRecordProgramParams =
  sendMessage mtrChannelClusterRecordProgramParams shouldRecordSeriesSelector

-- | @- setShouldRecordSeries:@
setShouldRecordSeries :: (IsMTRChannelClusterRecordProgramParams mtrChannelClusterRecordProgramParams, IsNSNumber value) => mtrChannelClusterRecordProgramParams -> value -> IO ()
setShouldRecordSeries mtrChannelClusterRecordProgramParams value =
  sendMessage mtrChannelClusterRecordProgramParams setShouldRecordSeriesSelector (toNSNumber value)

-- | @- externalIDList@
externalIDList :: IsMTRChannelClusterRecordProgramParams mtrChannelClusterRecordProgramParams => mtrChannelClusterRecordProgramParams -> IO (Id NSArray)
externalIDList mtrChannelClusterRecordProgramParams =
  sendMessage mtrChannelClusterRecordProgramParams externalIDListSelector

-- | @- setExternalIDList:@
setExternalIDList :: (IsMTRChannelClusterRecordProgramParams mtrChannelClusterRecordProgramParams, IsNSArray value) => mtrChannelClusterRecordProgramParams -> value -> IO ()
setExternalIDList mtrChannelClusterRecordProgramParams value =
  sendMessage mtrChannelClusterRecordProgramParams setExternalIDListSelector (toNSArray value)

-- | @- data@
data_ :: IsMTRChannelClusterRecordProgramParams mtrChannelClusterRecordProgramParams => mtrChannelClusterRecordProgramParams -> IO (Id NSData)
data_ mtrChannelClusterRecordProgramParams =
  sendMessage mtrChannelClusterRecordProgramParams dataSelector

-- | @- setData:@
setData :: (IsMTRChannelClusterRecordProgramParams mtrChannelClusterRecordProgramParams, IsNSData value) => mtrChannelClusterRecordProgramParams -> value -> IO ()
setData mtrChannelClusterRecordProgramParams value =
  sendMessage mtrChannelClusterRecordProgramParams setDataSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRChannelClusterRecordProgramParams mtrChannelClusterRecordProgramParams => mtrChannelClusterRecordProgramParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrChannelClusterRecordProgramParams =
  sendMessage mtrChannelClusterRecordProgramParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRChannelClusterRecordProgramParams mtrChannelClusterRecordProgramParams, IsNSNumber value) => mtrChannelClusterRecordProgramParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrChannelClusterRecordProgramParams value =
  sendMessage mtrChannelClusterRecordProgramParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRChannelClusterRecordProgramParams mtrChannelClusterRecordProgramParams => mtrChannelClusterRecordProgramParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrChannelClusterRecordProgramParams =
  sendMessage mtrChannelClusterRecordProgramParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRChannelClusterRecordProgramParams mtrChannelClusterRecordProgramParams, IsNSNumber value) => mtrChannelClusterRecordProgramParams -> value -> IO ()
setServerSideProcessingTimeout mtrChannelClusterRecordProgramParams value =
  sendMessage mtrChannelClusterRecordProgramParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @programIdentifier@
programIdentifierSelector :: Selector '[] (Id NSString)
programIdentifierSelector = mkSelector "programIdentifier"

-- | @Selector@ for @setProgramIdentifier:@
setProgramIdentifierSelector :: Selector '[Id NSString] ()
setProgramIdentifierSelector = mkSelector "setProgramIdentifier:"

-- | @Selector@ for @shouldRecordSeries@
shouldRecordSeriesSelector :: Selector '[] (Id NSNumber)
shouldRecordSeriesSelector = mkSelector "shouldRecordSeries"

-- | @Selector@ for @setShouldRecordSeries:@
setShouldRecordSeriesSelector :: Selector '[Id NSNumber] ()
setShouldRecordSeriesSelector = mkSelector "setShouldRecordSeries:"

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

