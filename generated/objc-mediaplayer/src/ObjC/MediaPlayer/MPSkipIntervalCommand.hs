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

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | An array of NSNumbers (NSTimeIntervals) that contain preferred skip intervals.
--
-- ObjC selector: @- preferredIntervals@
preferredIntervals :: IsMPSkipIntervalCommand mpSkipIntervalCommand => mpSkipIntervalCommand -> IO (Id NSArray)
preferredIntervals mpSkipIntervalCommand  =
  sendMsg mpSkipIntervalCommand (mkSelector "preferredIntervals") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An array of NSNumbers (NSTimeIntervals) that contain preferred skip intervals.
--
-- ObjC selector: @- setPreferredIntervals:@
setPreferredIntervals :: (IsMPSkipIntervalCommand mpSkipIntervalCommand, IsNSArray value) => mpSkipIntervalCommand -> value -> IO ()
setPreferredIntervals mpSkipIntervalCommand  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpSkipIntervalCommand (mkSelector "setPreferredIntervals:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @preferredIntervals@
preferredIntervalsSelector :: Selector
preferredIntervalsSelector = mkSelector "preferredIntervals"

-- | @Selector@ for @setPreferredIntervals:@
setPreferredIntervalsSelector :: Selector
setPreferredIntervalsSelector = mkSelector "setPreferredIntervals:"

