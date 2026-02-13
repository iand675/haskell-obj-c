{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPMusicPlayerPlayParametersQueueDescriptor@.
module ObjC.MediaPlayer.MPMusicPlayerPlayParametersQueueDescriptor
  ( MPMusicPlayerPlayParametersQueueDescriptor
  , IsMPMusicPlayerPlayParametersQueueDescriptor(..)
  , initWithPlayParametersQueue
  , setStartTime_forItemWithPlayParameters
  , setEndTime_forItemWithPlayParameters
  , playParametersQueue
  , setPlayParametersQueue
  , startItemPlayParameters
  , setStartItemPlayParameters
  , initWithPlayParametersQueueSelector
  , playParametersQueueSelector
  , setEndTime_forItemWithPlayParametersSelector
  , setPlayParametersQueueSelector
  , setStartItemPlayParametersSelector
  , setStartTime_forItemWithPlayParametersSelector
  , startItemPlayParametersSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPlayParametersQueue:@
initWithPlayParametersQueue :: (IsMPMusicPlayerPlayParametersQueueDescriptor mpMusicPlayerPlayParametersQueueDescriptor, IsNSArray playParametersQueue) => mpMusicPlayerPlayParametersQueueDescriptor -> playParametersQueue -> IO (Id MPMusicPlayerPlayParametersQueueDescriptor)
initWithPlayParametersQueue mpMusicPlayerPlayParametersQueueDescriptor playParametersQueue =
  sendOwnedMessage mpMusicPlayerPlayParametersQueueDescriptor initWithPlayParametersQueueSelector (toNSArray playParametersQueue)

-- | @- setStartTime:forItemWithPlayParameters:@
setStartTime_forItemWithPlayParameters :: (IsMPMusicPlayerPlayParametersQueueDescriptor mpMusicPlayerPlayParametersQueueDescriptor, IsMPMusicPlayerPlayParameters playParameters) => mpMusicPlayerPlayParametersQueueDescriptor -> CDouble -> playParameters -> IO ()
setStartTime_forItemWithPlayParameters mpMusicPlayerPlayParametersQueueDescriptor startTime playParameters =
  sendMessage mpMusicPlayerPlayParametersQueueDescriptor setStartTime_forItemWithPlayParametersSelector startTime (toMPMusicPlayerPlayParameters playParameters)

-- | @- setEndTime:forItemWithPlayParameters:@
setEndTime_forItemWithPlayParameters :: (IsMPMusicPlayerPlayParametersQueueDescriptor mpMusicPlayerPlayParametersQueueDescriptor, IsMPMusicPlayerPlayParameters playParameters) => mpMusicPlayerPlayParametersQueueDescriptor -> CDouble -> playParameters -> IO ()
setEndTime_forItemWithPlayParameters mpMusicPlayerPlayParametersQueueDescriptor endTime playParameters =
  sendMessage mpMusicPlayerPlayParametersQueueDescriptor setEndTime_forItemWithPlayParametersSelector endTime (toMPMusicPlayerPlayParameters playParameters)

-- | @- playParametersQueue@
playParametersQueue :: IsMPMusicPlayerPlayParametersQueueDescriptor mpMusicPlayerPlayParametersQueueDescriptor => mpMusicPlayerPlayParametersQueueDescriptor -> IO (Id NSArray)
playParametersQueue mpMusicPlayerPlayParametersQueueDescriptor =
  sendMessage mpMusicPlayerPlayParametersQueueDescriptor playParametersQueueSelector

-- | @- setPlayParametersQueue:@
setPlayParametersQueue :: (IsMPMusicPlayerPlayParametersQueueDescriptor mpMusicPlayerPlayParametersQueueDescriptor, IsNSArray value) => mpMusicPlayerPlayParametersQueueDescriptor -> value -> IO ()
setPlayParametersQueue mpMusicPlayerPlayParametersQueueDescriptor value =
  sendMessage mpMusicPlayerPlayParametersQueueDescriptor setPlayParametersQueueSelector (toNSArray value)

-- | @- startItemPlayParameters@
startItemPlayParameters :: IsMPMusicPlayerPlayParametersQueueDescriptor mpMusicPlayerPlayParametersQueueDescriptor => mpMusicPlayerPlayParametersQueueDescriptor -> IO (Id MPMusicPlayerPlayParameters)
startItemPlayParameters mpMusicPlayerPlayParametersQueueDescriptor =
  sendMessage mpMusicPlayerPlayParametersQueueDescriptor startItemPlayParametersSelector

-- | @- setStartItemPlayParameters:@
setStartItemPlayParameters :: (IsMPMusicPlayerPlayParametersQueueDescriptor mpMusicPlayerPlayParametersQueueDescriptor, IsMPMusicPlayerPlayParameters value) => mpMusicPlayerPlayParametersQueueDescriptor -> value -> IO ()
setStartItemPlayParameters mpMusicPlayerPlayParametersQueueDescriptor value =
  sendMessage mpMusicPlayerPlayParametersQueueDescriptor setStartItemPlayParametersSelector (toMPMusicPlayerPlayParameters value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPlayParametersQueue:@
initWithPlayParametersQueueSelector :: Selector '[Id NSArray] (Id MPMusicPlayerPlayParametersQueueDescriptor)
initWithPlayParametersQueueSelector = mkSelector "initWithPlayParametersQueue:"

-- | @Selector@ for @setStartTime:forItemWithPlayParameters:@
setStartTime_forItemWithPlayParametersSelector :: Selector '[CDouble, Id MPMusicPlayerPlayParameters] ()
setStartTime_forItemWithPlayParametersSelector = mkSelector "setStartTime:forItemWithPlayParameters:"

-- | @Selector@ for @setEndTime:forItemWithPlayParameters:@
setEndTime_forItemWithPlayParametersSelector :: Selector '[CDouble, Id MPMusicPlayerPlayParameters] ()
setEndTime_forItemWithPlayParametersSelector = mkSelector "setEndTime:forItemWithPlayParameters:"

-- | @Selector@ for @playParametersQueue@
playParametersQueueSelector :: Selector '[] (Id NSArray)
playParametersQueueSelector = mkSelector "playParametersQueue"

-- | @Selector@ for @setPlayParametersQueue:@
setPlayParametersQueueSelector :: Selector '[Id NSArray] ()
setPlayParametersQueueSelector = mkSelector "setPlayParametersQueue:"

-- | @Selector@ for @startItemPlayParameters@
startItemPlayParametersSelector :: Selector '[] (Id MPMusicPlayerPlayParameters)
startItemPlayParametersSelector = mkSelector "startItemPlayParameters"

-- | @Selector@ for @setStartItemPlayParameters:@
setStartItemPlayParametersSelector :: Selector '[Id MPMusicPlayerPlayParameters] ()
setStartItemPlayParametersSelector = mkSelector "setStartItemPlayParameters:"

