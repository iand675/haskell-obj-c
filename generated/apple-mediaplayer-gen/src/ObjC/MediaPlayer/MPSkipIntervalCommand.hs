{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSkipIntervalCommand@.
module ObjC.MediaPlayer.MPSkipIntervalCommand
  ( MPSkipIntervalCommand
  , IsMPSkipIntervalCommand(..)
  , preferredIntervals
  , setPreferredIntervals
  , preferredIntervalsSelector
  , setPreferredIntervalsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | An array of NSNumbers (NSTimeIntervals) that contain preferred skip intervals.
--
-- ObjC selector: @- preferredIntervals@
preferredIntervals :: IsMPSkipIntervalCommand mpSkipIntervalCommand => mpSkipIntervalCommand -> IO (Id NSArray)
preferredIntervals mpSkipIntervalCommand =
  sendMessage mpSkipIntervalCommand preferredIntervalsSelector

-- | An array of NSNumbers (NSTimeIntervals) that contain preferred skip intervals.
--
-- ObjC selector: @- setPreferredIntervals:@
setPreferredIntervals :: (IsMPSkipIntervalCommand mpSkipIntervalCommand, IsNSArray value) => mpSkipIntervalCommand -> value -> IO ()
setPreferredIntervals mpSkipIntervalCommand value =
  sendMessage mpSkipIntervalCommand setPreferredIntervalsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @preferredIntervals@
preferredIntervalsSelector :: Selector '[] (Id NSArray)
preferredIntervalsSelector = mkSelector "preferredIntervals"

-- | @Selector@ for @setPreferredIntervals:@
setPreferredIntervalsSelector :: Selector '[Id NSArray] ()
setPreferredIntervalsSelector = mkSelector "setPreferredIntervals:"

