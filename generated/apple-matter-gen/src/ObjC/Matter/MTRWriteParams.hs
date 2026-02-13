{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTRWriteParams    This is used to control the behavior of cluster writes.    If not provided (i.e. nil passed for the CHIPWriteParams argument), will be    treated as if a default-initialized object was passed in.
--
-- Generated bindings for @MTRWriteParams@.
module ObjC.Matter.MTRWriteParams
  ( MTRWriteParams
  , IsMTRWriteParams(..)
  , timedWriteTimeout
  , setTimedWriteTimeout
  , dataVersion
  , setDataVersion
  , dataVersionSelector
  , setDataVersionSelector
  , setTimedWriteTimeoutSelector
  , timedWriteTimeoutSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Controls whether the write is a timed write.
--
-- If nil (the default value), a regular write is done for attributes that do not require a timed write and a timed write with some default timed request timeout is done for attributes that require a timed write.
--
-- If not nil, a timed write is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual write request) within the timeout window.
--
-- This value is specified in milliseconds.
--
-- ObjC selector: @- timedWriteTimeout@
timedWriteTimeout :: IsMTRWriteParams mtrWriteParams => mtrWriteParams -> IO (Id NSNumber)
timedWriteTimeout mtrWriteParams =
  sendMessage mtrWriteParams timedWriteTimeoutSelector

-- | Controls whether the write is a timed write.
--
-- If nil (the default value), a regular write is done for attributes that do not require a timed write and a timed write with some default timed request timeout is done for attributes that require a timed write.
--
-- If not nil, a timed write is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual write request) within the timeout window.
--
-- This value is specified in milliseconds.
--
-- ObjC selector: @- setTimedWriteTimeout:@
setTimedWriteTimeout :: (IsMTRWriteParams mtrWriteParams, IsNSNumber value) => mtrWriteParams -> value -> IO ()
setTimedWriteTimeout mtrWriteParams value =
  sendMessage mtrWriteParams setTimedWriteTimeoutSelector (toNSNumber value)

-- | Sets the data version for the Write Request for the interaction.
--
-- If not nil, the write will only succeed if the current data version of the cluster matches the provided data version.
--
-- ObjC selector: @- dataVersion@
dataVersion :: IsMTRWriteParams mtrWriteParams => mtrWriteParams -> IO (Id NSNumber)
dataVersion mtrWriteParams =
  sendMessage mtrWriteParams dataVersionSelector

-- | Sets the data version for the Write Request for the interaction.
--
-- If not nil, the write will only succeed if the current data version of the cluster matches the provided data version.
--
-- ObjC selector: @- setDataVersion:@
setDataVersion :: (IsMTRWriteParams mtrWriteParams, IsNSNumber value) => mtrWriteParams -> value -> IO ()
setDataVersion mtrWriteParams value =
  sendMessage mtrWriteParams setDataVersionSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @timedWriteTimeout@
timedWriteTimeoutSelector :: Selector '[] (Id NSNumber)
timedWriteTimeoutSelector = mkSelector "timedWriteTimeout"

-- | @Selector@ for @setTimedWriteTimeout:@
setTimedWriteTimeoutSelector :: Selector '[Id NSNumber] ()
setTimedWriteTimeoutSelector = mkSelector "setTimedWriteTimeout:"

-- | @Selector@ for @dataVersion@
dataVersionSelector :: Selector '[] (Id NSNumber)
dataVersionSelector = mkSelector "dataVersion"

-- | @Selector@ for @setDataVersion:@
setDataVersionSelector :: Selector '[Id NSNumber] ()
setDataVersionSelector = mkSelector "setDataVersion:"

