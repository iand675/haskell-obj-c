{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitDuration@.
module ObjC.Foundation.NSUnitDuration
  ( NSUnitDuration
  , IsNSUnitDuration(..)
  , hours
  , minutes
  , seconds
  , milliseconds
  , microseconds
  , nanoseconds
  , picoseconds
  , hoursSelector
  , minutesSelector
  , secondsSelector
  , millisecondsSelector
  , microsecondsSelector
  , nanosecondsSelector
  , picosecondsSelector


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

import ObjC.Foundation.Internal.Classes

-- | @+ hours@
hours :: IO (Id NSUnitDuration)
hours  =
  do
    cls' <- getRequiredClass "NSUnitDuration"
    sendClassMsg cls' (mkSelector "hours") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ minutes@
minutes :: IO (Id NSUnitDuration)
minutes  =
  do
    cls' <- getRequiredClass "NSUnitDuration"
    sendClassMsg cls' (mkSelector "minutes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ seconds@
seconds :: IO (Id NSUnitDuration)
seconds  =
  do
    cls' <- getRequiredClass "NSUnitDuration"
    sendClassMsg cls' (mkSelector "seconds") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ milliseconds@
milliseconds :: IO (Id NSUnitDuration)
milliseconds  =
  do
    cls' <- getRequiredClass "NSUnitDuration"
    sendClassMsg cls' (mkSelector "milliseconds") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ microseconds@
microseconds :: IO (Id NSUnitDuration)
microseconds  =
  do
    cls' <- getRequiredClass "NSUnitDuration"
    sendClassMsg cls' (mkSelector "microseconds") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ nanoseconds@
nanoseconds :: IO (Id NSUnitDuration)
nanoseconds  =
  do
    cls' <- getRequiredClass "NSUnitDuration"
    sendClassMsg cls' (mkSelector "nanoseconds") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ picoseconds@
picoseconds :: IO (Id NSUnitDuration)
picoseconds  =
  do
    cls' <- getRequiredClass "NSUnitDuration"
    sendClassMsg cls' (mkSelector "picoseconds") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hours@
hoursSelector :: Selector
hoursSelector = mkSelector "hours"

-- | @Selector@ for @minutes@
minutesSelector :: Selector
minutesSelector = mkSelector "minutes"

-- | @Selector@ for @seconds@
secondsSelector :: Selector
secondsSelector = mkSelector "seconds"

-- | @Selector@ for @milliseconds@
millisecondsSelector :: Selector
millisecondsSelector = mkSelector "milliseconds"

-- | @Selector@ for @microseconds@
microsecondsSelector :: Selector
microsecondsSelector = mkSelector "microseconds"

-- | @Selector@ for @nanoseconds@
nanosecondsSelector :: Selector
nanosecondsSelector = mkSelector "nanoseconds"

-- | @Selector@ for @picoseconds@
picosecondsSelector :: Selector
picosecondsSelector = mkSelector "picoseconds"

