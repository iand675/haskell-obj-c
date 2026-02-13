{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRIdentifyClusterTriggerEffectParams@.
module ObjC.Matter.MTRIdentifyClusterTriggerEffectParams
  ( MTRIdentifyClusterTriggerEffectParams
  , IsMTRIdentifyClusterTriggerEffectParams(..)
  , effectIdentifier
  , setEffectIdentifier
  , effectVariant
  , setEffectVariant
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , effectIdentifierSelector
  , effectVariantSelector
  , serverSideProcessingTimeoutSelector
  , setEffectIdentifierSelector
  , setEffectVariantSelector
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

-- | @- effectIdentifier@
effectIdentifier :: IsMTRIdentifyClusterTriggerEffectParams mtrIdentifyClusterTriggerEffectParams => mtrIdentifyClusterTriggerEffectParams -> IO (Id NSNumber)
effectIdentifier mtrIdentifyClusterTriggerEffectParams =
  sendMessage mtrIdentifyClusterTriggerEffectParams effectIdentifierSelector

-- | @- setEffectIdentifier:@
setEffectIdentifier :: (IsMTRIdentifyClusterTriggerEffectParams mtrIdentifyClusterTriggerEffectParams, IsNSNumber value) => mtrIdentifyClusterTriggerEffectParams -> value -> IO ()
setEffectIdentifier mtrIdentifyClusterTriggerEffectParams value =
  sendMessage mtrIdentifyClusterTriggerEffectParams setEffectIdentifierSelector (toNSNumber value)

-- | @- effectVariant@
effectVariant :: IsMTRIdentifyClusterTriggerEffectParams mtrIdentifyClusterTriggerEffectParams => mtrIdentifyClusterTriggerEffectParams -> IO (Id NSNumber)
effectVariant mtrIdentifyClusterTriggerEffectParams =
  sendMessage mtrIdentifyClusterTriggerEffectParams effectVariantSelector

-- | @- setEffectVariant:@
setEffectVariant :: (IsMTRIdentifyClusterTriggerEffectParams mtrIdentifyClusterTriggerEffectParams, IsNSNumber value) => mtrIdentifyClusterTriggerEffectParams -> value -> IO ()
setEffectVariant mtrIdentifyClusterTriggerEffectParams value =
  sendMessage mtrIdentifyClusterTriggerEffectParams setEffectVariantSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRIdentifyClusterTriggerEffectParams mtrIdentifyClusterTriggerEffectParams => mtrIdentifyClusterTriggerEffectParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrIdentifyClusterTriggerEffectParams =
  sendMessage mtrIdentifyClusterTriggerEffectParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRIdentifyClusterTriggerEffectParams mtrIdentifyClusterTriggerEffectParams, IsNSNumber value) => mtrIdentifyClusterTriggerEffectParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrIdentifyClusterTriggerEffectParams value =
  sendMessage mtrIdentifyClusterTriggerEffectParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRIdentifyClusterTriggerEffectParams mtrIdentifyClusterTriggerEffectParams => mtrIdentifyClusterTriggerEffectParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrIdentifyClusterTriggerEffectParams =
  sendMessage mtrIdentifyClusterTriggerEffectParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRIdentifyClusterTriggerEffectParams mtrIdentifyClusterTriggerEffectParams, IsNSNumber value) => mtrIdentifyClusterTriggerEffectParams -> value -> IO ()
setServerSideProcessingTimeout mtrIdentifyClusterTriggerEffectParams value =
  sendMessage mtrIdentifyClusterTriggerEffectParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @effectIdentifier@
effectIdentifierSelector :: Selector '[] (Id NSNumber)
effectIdentifierSelector = mkSelector "effectIdentifier"

-- | @Selector@ for @setEffectIdentifier:@
setEffectIdentifierSelector :: Selector '[Id NSNumber] ()
setEffectIdentifierSelector = mkSelector "setEffectIdentifier:"

-- | @Selector@ for @effectVariant@
effectVariantSelector :: Selector '[] (Id NSNumber)
effectVariantSelector = mkSelector "effectVariant"

-- | @Selector@ for @setEffectVariant:@
setEffectVariantSelector :: Selector '[Id NSNumber] ()
setEffectVariantSelector = mkSelector "setEffectVariant:"

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

