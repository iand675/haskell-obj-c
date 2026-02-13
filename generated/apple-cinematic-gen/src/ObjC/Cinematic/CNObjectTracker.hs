{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Converts a normalized point or rectangle into a detection track that tracks an object over time.
--
-- Generated bindings for @CNObjectTracker@.
module ObjC.Cinematic.CNObjectTracker
  ( CNObjectTracker
  , IsCNObjectTracker(..)
  , initWithCommandQueue
  , finishDetectionTrack
  , resetDetectionTrack
  , init_
  , new
  , isSupported
  , finishDetectionTrackSelector
  , initSelector
  , initWithCommandQueueSelector
  , isSupportedSelector
  , newSelector
  , resetDetectionTrackSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Cinematic.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a new detection track builder. - Parameters:   - commandQueue: the command queue of a metal device to which commands should be submitted to perform work
--
-- ObjC selector: @- initWithCommandQueue:@
initWithCommandQueue :: IsCNObjectTracker cnObjectTracker => cnObjectTracker -> RawId -> IO (Id CNObjectTracker)
initWithCommandQueue cnObjectTracker commandQueue =
  sendOwnedMessage cnObjectTracker initWithCommandQueueSelector commandQueue

-- | Finish constructing the detection track and return it. - Returns: a detection track which tracks the object
--
-- ObjC selector: @- finishDetectionTrack@
finishDetectionTrack :: IsCNObjectTracker cnObjectTracker => cnObjectTracker -> IO (Id CNDetectionTrack)
finishDetectionTrack cnObjectTracker =
  sendMessage cnObjectTracker finishDetectionTrackSelector

-- | Reset the builder to construct a new detection track.
--
-- ObjC selector: @- resetDetectionTrack@
resetDetectionTrack :: IsCNObjectTracker cnObjectTracker => cnObjectTracker -> IO ()
resetDetectionTrack cnObjectTracker =
  sendMessage cnObjectTracker resetDetectionTrackSelector

-- | @- init@
init_ :: IsCNObjectTracker cnObjectTracker => cnObjectTracker -> IO (Id CNObjectTracker)
init_ cnObjectTracker =
  sendOwnedMessage cnObjectTracker initSelector

-- | @+ new@
new :: IO (Id CNObjectTracker)
new  =
  do
    cls' <- getRequiredClass "CNObjectTracker"
    sendOwnedClassMessage cls' newSelector

-- | Indicates whether the current device supports object detection and tracking.
--
-- ObjC selector: @+ isSupported@
isSupported :: IO Bool
isSupported  =
  do
    cls' <- getRequiredClass "CNObjectTracker"
    sendClassMessage cls' isSupportedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCommandQueue:@
initWithCommandQueueSelector :: Selector '[RawId] (Id CNObjectTracker)
initWithCommandQueueSelector = mkSelector "initWithCommandQueue:"

-- | @Selector@ for @finishDetectionTrack@
finishDetectionTrackSelector :: Selector '[] (Id CNDetectionTrack)
finishDetectionTrackSelector = mkSelector "finishDetectionTrack"

-- | @Selector@ for @resetDetectionTrack@
resetDetectionTrackSelector :: Selector '[] ()
resetDetectionTrackSelector = mkSelector "resetDetectionTrack"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CNObjectTracker)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CNObjectTracker)
newSelector = mkSelector "new"

-- | @Selector@ for @isSupported@
isSupportedSelector :: Selector '[] Bool
isSupportedSelector = mkSelector "isSupported"

