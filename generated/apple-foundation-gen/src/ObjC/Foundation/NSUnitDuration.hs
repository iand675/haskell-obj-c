{-# LANGUAGE DataKinds #-}
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
  , microsecondsSelector
  , millisecondsSelector
  , minutesSelector
  , nanosecondsSelector
  , picosecondsSelector
  , secondsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ hours@
hours :: IO (Id NSUnitDuration)
hours  =
  do
    cls' <- getRequiredClass "NSUnitDuration"
    sendClassMessage cls' hoursSelector

-- | @+ minutes@
minutes :: IO (Id NSUnitDuration)
minutes  =
  do
    cls' <- getRequiredClass "NSUnitDuration"
    sendClassMessage cls' minutesSelector

-- | @+ seconds@
seconds :: IO (Id NSUnitDuration)
seconds  =
  do
    cls' <- getRequiredClass "NSUnitDuration"
    sendClassMessage cls' secondsSelector

-- | @+ milliseconds@
milliseconds :: IO (Id NSUnitDuration)
milliseconds  =
  do
    cls' <- getRequiredClass "NSUnitDuration"
    sendClassMessage cls' millisecondsSelector

-- | @+ microseconds@
microseconds :: IO (Id NSUnitDuration)
microseconds  =
  do
    cls' <- getRequiredClass "NSUnitDuration"
    sendClassMessage cls' microsecondsSelector

-- | @+ nanoseconds@
nanoseconds :: IO (Id NSUnitDuration)
nanoseconds  =
  do
    cls' <- getRequiredClass "NSUnitDuration"
    sendClassMessage cls' nanosecondsSelector

-- | @+ picoseconds@
picoseconds :: IO (Id NSUnitDuration)
picoseconds  =
  do
    cls' <- getRequiredClass "NSUnitDuration"
    sendClassMessage cls' picosecondsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hours@
hoursSelector :: Selector '[] (Id NSUnitDuration)
hoursSelector = mkSelector "hours"

-- | @Selector@ for @minutes@
minutesSelector :: Selector '[] (Id NSUnitDuration)
minutesSelector = mkSelector "minutes"

-- | @Selector@ for @seconds@
secondsSelector :: Selector '[] (Id NSUnitDuration)
secondsSelector = mkSelector "seconds"

-- | @Selector@ for @milliseconds@
millisecondsSelector :: Selector '[] (Id NSUnitDuration)
millisecondsSelector = mkSelector "milliseconds"

-- | @Selector@ for @microseconds@
microsecondsSelector :: Selector '[] (Id NSUnitDuration)
microsecondsSelector = mkSelector "microseconds"

-- | @Selector@ for @nanoseconds@
nanosecondsSelector :: Selector '[] (Id NSUnitDuration)
nanosecondsSelector = mkSelector "nanoseconds"

-- | @Selector@ for @picoseconds@
picosecondsSelector :: Selector '[] (Id NSUnitDuration)
picosecondsSelector = mkSelector "picoseconds"

