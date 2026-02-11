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
  , setStartTime_forItemWithPlayParametersSelector
  , setEndTime_forItemWithPlayParametersSelector
  , playParametersQueueSelector
  , setPlayParametersQueueSelector
  , startItemPlayParametersSelector
  , setStartItemPlayParametersSelector


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

-- | @- initWithPlayParametersQueue:@
initWithPlayParametersQueue :: (IsMPMusicPlayerPlayParametersQueueDescriptor mpMusicPlayerPlayParametersQueueDescriptor, IsNSArray playParametersQueue) => mpMusicPlayerPlayParametersQueueDescriptor -> playParametersQueue -> IO (Id MPMusicPlayerPlayParametersQueueDescriptor)
initWithPlayParametersQueue mpMusicPlayerPlayParametersQueueDescriptor  playParametersQueue =
withObjCPtr playParametersQueue $ \raw_playParametersQueue ->
    sendMsg mpMusicPlayerPlayParametersQueueDescriptor (mkSelector "initWithPlayParametersQueue:") (retPtr retVoid) [argPtr (castPtr raw_playParametersQueue :: Ptr ())] >>= ownedObject . castPtr

-- | @- setStartTime:forItemWithPlayParameters:@
setStartTime_forItemWithPlayParameters :: (IsMPMusicPlayerPlayParametersQueueDescriptor mpMusicPlayerPlayParametersQueueDescriptor, IsMPMusicPlayerPlayParameters playParameters) => mpMusicPlayerPlayParametersQueueDescriptor -> CDouble -> playParameters -> IO ()
setStartTime_forItemWithPlayParameters mpMusicPlayerPlayParametersQueueDescriptor  startTime playParameters =
withObjCPtr playParameters $ \raw_playParameters ->
    sendMsg mpMusicPlayerPlayParametersQueueDescriptor (mkSelector "setStartTime:forItemWithPlayParameters:") retVoid [argCDouble (fromIntegral startTime), argPtr (castPtr raw_playParameters :: Ptr ())]

-- | @- setEndTime:forItemWithPlayParameters:@
setEndTime_forItemWithPlayParameters :: (IsMPMusicPlayerPlayParametersQueueDescriptor mpMusicPlayerPlayParametersQueueDescriptor, IsMPMusicPlayerPlayParameters playParameters) => mpMusicPlayerPlayParametersQueueDescriptor -> CDouble -> playParameters -> IO ()
setEndTime_forItemWithPlayParameters mpMusicPlayerPlayParametersQueueDescriptor  endTime playParameters =
withObjCPtr playParameters $ \raw_playParameters ->
    sendMsg mpMusicPlayerPlayParametersQueueDescriptor (mkSelector "setEndTime:forItemWithPlayParameters:") retVoid [argCDouble (fromIntegral endTime), argPtr (castPtr raw_playParameters :: Ptr ())]

-- | @- playParametersQueue@
playParametersQueue :: IsMPMusicPlayerPlayParametersQueueDescriptor mpMusicPlayerPlayParametersQueueDescriptor => mpMusicPlayerPlayParametersQueueDescriptor -> IO (Id NSArray)
playParametersQueue mpMusicPlayerPlayParametersQueueDescriptor  =
  sendMsg mpMusicPlayerPlayParametersQueueDescriptor (mkSelector "playParametersQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPlayParametersQueue:@
setPlayParametersQueue :: (IsMPMusicPlayerPlayParametersQueueDescriptor mpMusicPlayerPlayParametersQueueDescriptor, IsNSArray value) => mpMusicPlayerPlayParametersQueueDescriptor -> value -> IO ()
setPlayParametersQueue mpMusicPlayerPlayParametersQueueDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpMusicPlayerPlayParametersQueueDescriptor (mkSelector "setPlayParametersQueue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- startItemPlayParameters@
startItemPlayParameters :: IsMPMusicPlayerPlayParametersQueueDescriptor mpMusicPlayerPlayParametersQueueDescriptor => mpMusicPlayerPlayParametersQueueDescriptor -> IO (Id MPMusicPlayerPlayParameters)
startItemPlayParameters mpMusicPlayerPlayParametersQueueDescriptor  =
  sendMsg mpMusicPlayerPlayParametersQueueDescriptor (mkSelector "startItemPlayParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStartItemPlayParameters:@
setStartItemPlayParameters :: (IsMPMusicPlayerPlayParametersQueueDescriptor mpMusicPlayerPlayParametersQueueDescriptor, IsMPMusicPlayerPlayParameters value) => mpMusicPlayerPlayParametersQueueDescriptor -> value -> IO ()
setStartItemPlayParameters mpMusicPlayerPlayParametersQueueDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpMusicPlayerPlayParametersQueueDescriptor (mkSelector "setStartItemPlayParameters:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPlayParametersQueue:@
initWithPlayParametersQueueSelector :: Selector
initWithPlayParametersQueueSelector = mkSelector "initWithPlayParametersQueue:"

-- | @Selector@ for @setStartTime:forItemWithPlayParameters:@
setStartTime_forItemWithPlayParametersSelector :: Selector
setStartTime_forItemWithPlayParametersSelector = mkSelector "setStartTime:forItemWithPlayParameters:"

-- | @Selector@ for @setEndTime:forItemWithPlayParameters:@
setEndTime_forItemWithPlayParametersSelector :: Selector
setEndTime_forItemWithPlayParametersSelector = mkSelector "setEndTime:forItemWithPlayParameters:"

-- | @Selector@ for @playParametersQueue@
playParametersQueueSelector :: Selector
playParametersQueueSelector = mkSelector "playParametersQueue"

-- | @Selector@ for @setPlayParametersQueue:@
setPlayParametersQueueSelector :: Selector
setPlayParametersQueueSelector = mkSelector "setPlayParametersQueue:"

-- | @Selector@ for @startItemPlayParameters@
startItemPlayParametersSelector :: Selector
startItemPlayParametersSelector = mkSelector "startItemPlayParameters"

-- | @Selector@ for @setStartItemPlayParameters:@
setStartItemPlayParametersSelector :: Selector
setStartItemPlayParametersSelector = mkSelector "setStartItemPlayParameters:"

