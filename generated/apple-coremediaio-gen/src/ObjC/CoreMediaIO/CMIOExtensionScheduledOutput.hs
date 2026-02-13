{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CMIOExtensionScheduledOutput@.
module ObjC.CoreMediaIO.CMIOExtensionScheduledOutput
  ( CMIOExtensionScheduledOutput
  , IsCMIOExtensionScheduledOutput(..)
  , init_
  , new
  , scheduledOutputWithSequenceNumber_hostTimeInNanoseconds
  , initWithSequenceNumber_hostTimeInNanoseconds
  , sequenceNumber
  , hostTimeInNanoseconds
  , hostTimeInNanosecondsSelector
  , initSelector
  , initWithSequenceNumber_hostTimeInNanosecondsSelector
  , newSelector
  , scheduledOutputWithSequenceNumber_hostTimeInNanosecondsSelector
  , sequenceNumberSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMediaIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCMIOExtensionScheduledOutput cmioExtensionScheduledOutput => cmioExtensionScheduledOutput -> IO (Id CMIOExtensionScheduledOutput)
init_ cmioExtensionScheduledOutput =
  sendOwnedMessage cmioExtensionScheduledOutput initSelector

-- | @+ new@
new :: IO (Id CMIOExtensionScheduledOutput)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionScheduledOutput"
    sendOwnedClassMessage cls' newSelector

-- | scheduledOutputWithSequenceNumber:hostTimeInNanoseconds:
--
-- Return a stream scheduled output instance.
--
-- @sequenceNumber@ — The buffer sequence number that was output.
--
-- @hostTimeInNanoseconds@ — The host time in nanoseconds when the buffer was output.
--
-- Returns: A CMIOExtensionScheduledOutput instance that describes the state of the stream.
--
-- ObjC selector: @+ scheduledOutputWithSequenceNumber:hostTimeInNanoseconds:@
scheduledOutputWithSequenceNumber_hostTimeInNanoseconds :: CULong -> CULong -> IO (Id CMIOExtensionScheduledOutput)
scheduledOutputWithSequenceNumber_hostTimeInNanoseconds sequenceNumber hostTimeInNanoseconds =
  do
    cls' <- getRequiredClass "CMIOExtensionScheduledOutput"
    sendClassMessage cls' scheduledOutputWithSequenceNumber_hostTimeInNanosecondsSelector sequenceNumber hostTimeInNanoseconds

-- | initWithSequenceNumber:hostTimeInNanoseconds:
--
-- Initialize a stream scheduled output instance.
--
-- @sequenceNumber@ — The buffer sequence number that was output.
--
-- @hostTimeInNanoseconds@ — The host time in nanoseconds when the buffer was output.
--
-- Returns: A CMIOExtensionScheduledOutput instance that describes the state of the stream.
--
-- ObjC selector: @- initWithSequenceNumber:hostTimeInNanoseconds:@
initWithSequenceNumber_hostTimeInNanoseconds :: IsCMIOExtensionScheduledOutput cmioExtensionScheduledOutput => cmioExtensionScheduledOutput -> CULong -> CULong -> IO (Id CMIOExtensionScheduledOutput)
initWithSequenceNumber_hostTimeInNanoseconds cmioExtensionScheduledOutput sequenceNumber hostTimeInNanoseconds =
  sendOwnedMessage cmioExtensionScheduledOutput initWithSequenceNumber_hostTimeInNanosecondsSelector sequenceNumber hostTimeInNanoseconds

-- | sequenceNumber
--
-- The buffer sequence number that was output.
--
-- ObjC selector: @- sequenceNumber@
sequenceNumber :: IsCMIOExtensionScheduledOutput cmioExtensionScheduledOutput => cmioExtensionScheduledOutput -> IO CULong
sequenceNumber cmioExtensionScheduledOutput =
  sendMessage cmioExtensionScheduledOutput sequenceNumberSelector

-- | hostTimeInNanoseconds
--
-- The host time in nanoseconds when the buffer was output.
--
-- ObjC selector: @- hostTimeInNanoseconds@
hostTimeInNanoseconds :: IsCMIOExtensionScheduledOutput cmioExtensionScheduledOutput => cmioExtensionScheduledOutput -> IO CULong
hostTimeInNanoseconds cmioExtensionScheduledOutput =
  sendMessage cmioExtensionScheduledOutput hostTimeInNanosecondsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CMIOExtensionScheduledOutput)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CMIOExtensionScheduledOutput)
newSelector = mkSelector "new"

-- | @Selector@ for @scheduledOutputWithSequenceNumber:hostTimeInNanoseconds:@
scheduledOutputWithSequenceNumber_hostTimeInNanosecondsSelector :: Selector '[CULong, CULong] (Id CMIOExtensionScheduledOutput)
scheduledOutputWithSequenceNumber_hostTimeInNanosecondsSelector = mkSelector "scheduledOutputWithSequenceNumber:hostTimeInNanoseconds:"

-- | @Selector@ for @initWithSequenceNumber:hostTimeInNanoseconds:@
initWithSequenceNumber_hostTimeInNanosecondsSelector :: Selector '[CULong, CULong] (Id CMIOExtensionScheduledOutput)
initWithSequenceNumber_hostTimeInNanosecondsSelector = mkSelector "initWithSequenceNumber:hostTimeInNanoseconds:"

-- | @Selector@ for @sequenceNumber@
sequenceNumberSelector :: Selector '[] CULong
sequenceNumberSelector = mkSelector "sequenceNumber"

-- | @Selector@ for @hostTimeInNanoseconds@
hostTimeInNanosecondsSelector :: Selector '[] CULong
hostTimeInNanosecondsSelector = mkSelector "hostTimeInNanoseconds"

