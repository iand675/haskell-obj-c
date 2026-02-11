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
  , initSelector
  , newSelector
  , scheduledOutputWithSequenceNumber_hostTimeInNanosecondsSelector
  , initWithSequenceNumber_hostTimeInNanosecondsSelector
  , sequenceNumberSelector
  , hostTimeInNanosecondsSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMediaIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCMIOExtensionScheduledOutput cmioExtensionScheduledOutput => cmioExtensionScheduledOutput -> IO (Id CMIOExtensionScheduledOutput)
init_ cmioExtensionScheduledOutput  =
  sendMsg cmioExtensionScheduledOutput (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CMIOExtensionScheduledOutput)
new  =
  do
    cls' <- getRequiredClass "CMIOExtensionScheduledOutput"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    sendClassMsg cls' (mkSelector "scheduledOutputWithSequenceNumber:hostTimeInNanoseconds:") (retPtr retVoid) [argCULong (fromIntegral sequenceNumber), argCULong (fromIntegral hostTimeInNanoseconds)] >>= retainedObject . castPtr

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
initWithSequenceNumber_hostTimeInNanoseconds cmioExtensionScheduledOutput  sequenceNumber hostTimeInNanoseconds =
  sendMsg cmioExtensionScheduledOutput (mkSelector "initWithSequenceNumber:hostTimeInNanoseconds:") (retPtr retVoid) [argCULong (fromIntegral sequenceNumber), argCULong (fromIntegral hostTimeInNanoseconds)] >>= ownedObject . castPtr

-- | sequenceNumber
--
-- The buffer sequence number that was output.
--
-- ObjC selector: @- sequenceNumber@
sequenceNumber :: IsCMIOExtensionScheduledOutput cmioExtensionScheduledOutput => cmioExtensionScheduledOutput -> IO CULong
sequenceNumber cmioExtensionScheduledOutput  =
  sendMsg cmioExtensionScheduledOutput (mkSelector "sequenceNumber") retCULong []

-- | hostTimeInNanoseconds
--
-- The host time in nanoseconds when the buffer was output.
--
-- ObjC selector: @- hostTimeInNanoseconds@
hostTimeInNanoseconds :: IsCMIOExtensionScheduledOutput cmioExtensionScheduledOutput => cmioExtensionScheduledOutput -> IO CULong
hostTimeInNanoseconds cmioExtensionScheduledOutput  =
  sendMsg cmioExtensionScheduledOutput (mkSelector "hostTimeInNanoseconds") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @scheduledOutputWithSequenceNumber:hostTimeInNanoseconds:@
scheduledOutputWithSequenceNumber_hostTimeInNanosecondsSelector :: Selector
scheduledOutputWithSequenceNumber_hostTimeInNanosecondsSelector = mkSelector "scheduledOutputWithSequenceNumber:hostTimeInNanoseconds:"

-- | @Selector@ for @initWithSequenceNumber:hostTimeInNanoseconds:@
initWithSequenceNumber_hostTimeInNanosecondsSelector :: Selector
initWithSequenceNumber_hostTimeInNanosecondsSelector = mkSelector "initWithSequenceNumber:hostTimeInNanoseconds:"

-- | @Selector@ for @sequenceNumber@
sequenceNumberSelector :: Selector
sequenceNumberSelector = mkSelector "sequenceNumber"

-- | @Selector@ for @hostTimeInNanoseconds@
hostTimeInNanosecondsSelector :: Selector
hostTimeInNanosecondsSelector = mkSelector "hostTimeInNanoseconds"

