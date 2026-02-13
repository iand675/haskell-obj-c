{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRAudioOutputClusterRenameOutputParams@.
module ObjC.Matter.MTRAudioOutputClusterRenameOutputParams
  ( MTRAudioOutputClusterRenameOutputParams
  , IsMTRAudioOutputClusterRenameOutputParams(..)
  , index
  , setIndex
  , name
  , setName
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , indexSelector
  , nameSelector
  , serverSideProcessingTimeoutSelector
  , setIndexSelector
  , setNameSelector
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

-- | @- index@
index :: IsMTRAudioOutputClusterRenameOutputParams mtrAudioOutputClusterRenameOutputParams => mtrAudioOutputClusterRenameOutputParams -> IO (Id NSNumber)
index mtrAudioOutputClusterRenameOutputParams =
  sendMessage mtrAudioOutputClusterRenameOutputParams indexSelector

-- | @- setIndex:@
setIndex :: (IsMTRAudioOutputClusterRenameOutputParams mtrAudioOutputClusterRenameOutputParams, IsNSNumber value) => mtrAudioOutputClusterRenameOutputParams -> value -> IO ()
setIndex mtrAudioOutputClusterRenameOutputParams value =
  sendMessage mtrAudioOutputClusterRenameOutputParams setIndexSelector (toNSNumber value)

-- | @- name@
name :: IsMTRAudioOutputClusterRenameOutputParams mtrAudioOutputClusterRenameOutputParams => mtrAudioOutputClusterRenameOutputParams -> IO (Id NSString)
name mtrAudioOutputClusterRenameOutputParams =
  sendMessage mtrAudioOutputClusterRenameOutputParams nameSelector

-- | @- setName:@
setName :: (IsMTRAudioOutputClusterRenameOutputParams mtrAudioOutputClusterRenameOutputParams, IsNSString value) => mtrAudioOutputClusterRenameOutputParams -> value -> IO ()
setName mtrAudioOutputClusterRenameOutputParams value =
  sendMessage mtrAudioOutputClusterRenameOutputParams setNameSelector (toNSString value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRAudioOutputClusterRenameOutputParams mtrAudioOutputClusterRenameOutputParams => mtrAudioOutputClusterRenameOutputParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrAudioOutputClusterRenameOutputParams =
  sendMessage mtrAudioOutputClusterRenameOutputParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRAudioOutputClusterRenameOutputParams mtrAudioOutputClusterRenameOutputParams, IsNSNumber value) => mtrAudioOutputClusterRenameOutputParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrAudioOutputClusterRenameOutputParams value =
  sendMessage mtrAudioOutputClusterRenameOutputParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRAudioOutputClusterRenameOutputParams mtrAudioOutputClusterRenameOutputParams => mtrAudioOutputClusterRenameOutputParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrAudioOutputClusterRenameOutputParams =
  sendMessage mtrAudioOutputClusterRenameOutputParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRAudioOutputClusterRenameOutputParams mtrAudioOutputClusterRenameOutputParams, IsNSNumber value) => mtrAudioOutputClusterRenameOutputParams -> value -> IO ()
setServerSideProcessingTimeout mtrAudioOutputClusterRenameOutputParams value =
  sendMessage mtrAudioOutputClusterRenameOutputParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @index@
indexSelector :: Selector '[] (Id NSNumber)
indexSelector = mkSelector "index"

-- | @Selector@ for @setIndex:@
setIndexSelector :: Selector '[Id NSNumber] ()
setIndexSelector = mkSelector "setIndex:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

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

