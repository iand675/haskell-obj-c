{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMediaInputClusterRenameInputParams@.
module ObjC.Matter.MTRMediaInputClusterRenameInputParams
  ( MTRMediaInputClusterRenameInputParams
  , IsMTRMediaInputClusterRenameInputParams(..)
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
index :: IsMTRMediaInputClusterRenameInputParams mtrMediaInputClusterRenameInputParams => mtrMediaInputClusterRenameInputParams -> IO (Id NSNumber)
index mtrMediaInputClusterRenameInputParams =
  sendMessage mtrMediaInputClusterRenameInputParams indexSelector

-- | @- setIndex:@
setIndex :: (IsMTRMediaInputClusterRenameInputParams mtrMediaInputClusterRenameInputParams, IsNSNumber value) => mtrMediaInputClusterRenameInputParams -> value -> IO ()
setIndex mtrMediaInputClusterRenameInputParams value =
  sendMessage mtrMediaInputClusterRenameInputParams setIndexSelector (toNSNumber value)

-- | @- name@
name :: IsMTRMediaInputClusterRenameInputParams mtrMediaInputClusterRenameInputParams => mtrMediaInputClusterRenameInputParams -> IO (Id NSString)
name mtrMediaInputClusterRenameInputParams =
  sendMessage mtrMediaInputClusterRenameInputParams nameSelector

-- | @- setName:@
setName :: (IsMTRMediaInputClusterRenameInputParams mtrMediaInputClusterRenameInputParams, IsNSString value) => mtrMediaInputClusterRenameInputParams -> value -> IO ()
setName mtrMediaInputClusterRenameInputParams value =
  sendMessage mtrMediaInputClusterRenameInputParams setNameSelector (toNSString value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRMediaInputClusterRenameInputParams mtrMediaInputClusterRenameInputParams => mtrMediaInputClusterRenameInputParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrMediaInputClusterRenameInputParams =
  sendMessage mtrMediaInputClusterRenameInputParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRMediaInputClusterRenameInputParams mtrMediaInputClusterRenameInputParams, IsNSNumber value) => mtrMediaInputClusterRenameInputParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrMediaInputClusterRenameInputParams value =
  sendMessage mtrMediaInputClusterRenameInputParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRMediaInputClusterRenameInputParams mtrMediaInputClusterRenameInputParams => mtrMediaInputClusterRenameInputParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrMediaInputClusterRenameInputParams =
  sendMessage mtrMediaInputClusterRenameInputParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRMediaInputClusterRenameInputParams mtrMediaInputClusterRenameInputParams, IsNSNumber value) => mtrMediaInputClusterRenameInputParams -> value -> IO ()
setServerSideProcessingTimeout mtrMediaInputClusterRenameInputParams value =
  sendMessage mtrMediaInputClusterRenameInputParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

