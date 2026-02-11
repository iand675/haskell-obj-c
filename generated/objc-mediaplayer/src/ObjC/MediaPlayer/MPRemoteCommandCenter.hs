{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPRemoteCommandCenter@.
module ObjC.MediaPlayer.MPRemoteCommandCenter
  ( MPRemoteCommandCenter
  , IsMPRemoteCommandCenter(..)
  , sharedCommandCenter
  , new
  , init_
  , pauseCommand
  , playCommand
  , stopCommand
  , togglePlayPauseCommand
  , enableLanguageOptionCommand
  , disableLanguageOptionCommand
  , changePlaybackRateCommand
  , changeRepeatModeCommand
  , changeShuffleModeCommand
  , nextTrackCommand
  , previousTrackCommand
  , skipForwardCommand
  , skipBackwardCommand
  , seekForwardCommand
  , seekBackwardCommand
  , changePlaybackPositionCommand
  , ratingCommand
  , likeCommand
  , dislikeCommand
  , bookmarkCommand
  , sharedCommandCenterSelector
  , newSelector
  , initSelector
  , pauseCommandSelector
  , playCommandSelector
  , stopCommandSelector
  , togglePlayPauseCommandSelector
  , enableLanguageOptionCommandSelector
  , disableLanguageOptionCommandSelector
  , changePlaybackRateCommandSelector
  , changeRepeatModeCommandSelector
  , changeShuffleModeCommandSelector
  , nextTrackCommandSelector
  , previousTrackCommandSelector
  , skipForwardCommandSelector
  , skipBackwardCommandSelector
  , seekForwardCommandSelector
  , seekBackwardCommandSelector
  , changePlaybackPositionCommandSelector
  , ratingCommandSelector
  , likeCommandSelector
  , dislikeCommandSelector
  , bookmarkCommandSelector


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

-- | @+ sharedCommandCenter@
sharedCommandCenter :: IO (Id MPRemoteCommandCenter)
sharedCommandCenter  =
  do
    cls' <- getRequiredClass "MPRemoteCommandCenter"
    sendClassMsg cls' (mkSelector "sharedCommandCenter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ new@
new :: IO (Id MPRemoteCommandCenter)
new  =
  do
    cls' <- getRequiredClass "MPRemoteCommandCenter"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRemoteCommandCenter)
init_ mpRemoteCommandCenter  =
  sendMsg mpRemoteCommandCenter (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- pauseCommand@
pauseCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRemoteCommand)
pauseCommand mpRemoteCommandCenter  =
  sendMsg mpRemoteCommandCenter (mkSelector "pauseCommand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- playCommand@
playCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRemoteCommand)
playCommand mpRemoteCommandCenter  =
  sendMsg mpRemoteCommandCenter (mkSelector "playCommand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- stopCommand@
stopCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRemoteCommand)
stopCommand mpRemoteCommandCenter  =
  sendMsg mpRemoteCommandCenter (mkSelector "stopCommand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- togglePlayPauseCommand@
togglePlayPauseCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRemoteCommand)
togglePlayPauseCommand mpRemoteCommandCenter  =
  sendMsg mpRemoteCommandCenter (mkSelector "togglePlayPauseCommand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- enableLanguageOptionCommand@
enableLanguageOptionCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRemoteCommand)
enableLanguageOptionCommand mpRemoteCommandCenter  =
  sendMsg mpRemoteCommandCenter (mkSelector "enableLanguageOptionCommand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- disableLanguageOptionCommand@
disableLanguageOptionCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRemoteCommand)
disableLanguageOptionCommand mpRemoteCommandCenter  =
  sendMsg mpRemoteCommandCenter (mkSelector "disableLanguageOptionCommand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- changePlaybackRateCommand@
changePlaybackRateCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPChangePlaybackRateCommand)
changePlaybackRateCommand mpRemoteCommandCenter  =
  sendMsg mpRemoteCommandCenter (mkSelector "changePlaybackRateCommand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- changeRepeatModeCommand@
changeRepeatModeCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPChangeRepeatModeCommand)
changeRepeatModeCommand mpRemoteCommandCenter  =
  sendMsg mpRemoteCommandCenter (mkSelector "changeRepeatModeCommand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- changeShuffleModeCommand@
changeShuffleModeCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPChangeShuffleModeCommand)
changeShuffleModeCommand mpRemoteCommandCenter  =
  sendMsg mpRemoteCommandCenter (mkSelector "changeShuffleModeCommand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- nextTrackCommand@
nextTrackCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRemoteCommand)
nextTrackCommand mpRemoteCommandCenter  =
  sendMsg mpRemoteCommandCenter (mkSelector "nextTrackCommand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- previousTrackCommand@
previousTrackCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRemoteCommand)
previousTrackCommand mpRemoteCommandCenter  =
  sendMsg mpRemoteCommandCenter (mkSelector "previousTrackCommand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- skipForwardCommand@
skipForwardCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPSkipIntervalCommand)
skipForwardCommand mpRemoteCommandCenter  =
  sendMsg mpRemoteCommandCenter (mkSelector "skipForwardCommand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- skipBackwardCommand@
skipBackwardCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPSkipIntervalCommand)
skipBackwardCommand mpRemoteCommandCenter  =
  sendMsg mpRemoteCommandCenter (mkSelector "skipBackwardCommand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- seekForwardCommand@
seekForwardCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRemoteCommand)
seekForwardCommand mpRemoteCommandCenter  =
  sendMsg mpRemoteCommandCenter (mkSelector "seekForwardCommand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- seekBackwardCommand@
seekBackwardCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRemoteCommand)
seekBackwardCommand mpRemoteCommandCenter  =
  sendMsg mpRemoteCommandCenter (mkSelector "seekBackwardCommand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- changePlaybackPositionCommand@
changePlaybackPositionCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPChangePlaybackPositionCommand)
changePlaybackPositionCommand mpRemoteCommandCenter  =
  sendMsg mpRemoteCommandCenter (mkSelector "changePlaybackPositionCommand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- ratingCommand@
ratingCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRatingCommand)
ratingCommand mpRemoteCommandCenter  =
  sendMsg mpRemoteCommandCenter (mkSelector "ratingCommand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- likeCommand@
likeCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPFeedbackCommand)
likeCommand mpRemoteCommandCenter  =
  sendMsg mpRemoteCommandCenter (mkSelector "likeCommand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- dislikeCommand@
dislikeCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPFeedbackCommand)
dislikeCommand mpRemoteCommandCenter  =
  sendMsg mpRemoteCommandCenter (mkSelector "dislikeCommand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- bookmarkCommand@
bookmarkCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPFeedbackCommand)
bookmarkCommand mpRemoteCommandCenter  =
  sendMsg mpRemoteCommandCenter (mkSelector "bookmarkCommand") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedCommandCenter@
sharedCommandCenterSelector :: Selector
sharedCommandCenterSelector = mkSelector "sharedCommandCenter"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @pauseCommand@
pauseCommandSelector :: Selector
pauseCommandSelector = mkSelector "pauseCommand"

-- | @Selector@ for @playCommand@
playCommandSelector :: Selector
playCommandSelector = mkSelector "playCommand"

-- | @Selector@ for @stopCommand@
stopCommandSelector :: Selector
stopCommandSelector = mkSelector "stopCommand"

-- | @Selector@ for @togglePlayPauseCommand@
togglePlayPauseCommandSelector :: Selector
togglePlayPauseCommandSelector = mkSelector "togglePlayPauseCommand"

-- | @Selector@ for @enableLanguageOptionCommand@
enableLanguageOptionCommandSelector :: Selector
enableLanguageOptionCommandSelector = mkSelector "enableLanguageOptionCommand"

-- | @Selector@ for @disableLanguageOptionCommand@
disableLanguageOptionCommandSelector :: Selector
disableLanguageOptionCommandSelector = mkSelector "disableLanguageOptionCommand"

-- | @Selector@ for @changePlaybackRateCommand@
changePlaybackRateCommandSelector :: Selector
changePlaybackRateCommandSelector = mkSelector "changePlaybackRateCommand"

-- | @Selector@ for @changeRepeatModeCommand@
changeRepeatModeCommandSelector :: Selector
changeRepeatModeCommandSelector = mkSelector "changeRepeatModeCommand"

-- | @Selector@ for @changeShuffleModeCommand@
changeShuffleModeCommandSelector :: Selector
changeShuffleModeCommandSelector = mkSelector "changeShuffleModeCommand"

-- | @Selector@ for @nextTrackCommand@
nextTrackCommandSelector :: Selector
nextTrackCommandSelector = mkSelector "nextTrackCommand"

-- | @Selector@ for @previousTrackCommand@
previousTrackCommandSelector :: Selector
previousTrackCommandSelector = mkSelector "previousTrackCommand"

-- | @Selector@ for @skipForwardCommand@
skipForwardCommandSelector :: Selector
skipForwardCommandSelector = mkSelector "skipForwardCommand"

-- | @Selector@ for @skipBackwardCommand@
skipBackwardCommandSelector :: Selector
skipBackwardCommandSelector = mkSelector "skipBackwardCommand"

-- | @Selector@ for @seekForwardCommand@
seekForwardCommandSelector :: Selector
seekForwardCommandSelector = mkSelector "seekForwardCommand"

-- | @Selector@ for @seekBackwardCommand@
seekBackwardCommandSelector :: Selector
seekBackwardCommandSelector = mkSelector "seekBackwardCommand"

-- | @Selector@ for @changePlaybackPositionCommand@
changePlaybackPositionCommandSelector :: Selector
changePlaybackPositionCommandSelector = mkSelector "changePlaybackPositionCommand"

-- | @Selector@ for @ratingCommand@
ratingCommandSelector :: Selector
ratingCommandSelector = mkSelector "ratingCommand"

-- | @Selector@ for @likeCommand@
likeCommandSelector :: Selector
likeCommandSelector = mkSelector "likeCommand"

-- | @Selector@ for @dislikeCommand@
dislikeCommandSelector :: Selector
dislikeCommandSelector = mkSelector "dislikeCommand"

-- | @Selector@ for @bookmarkCommand@
bookmarkCommandSelector :: Selector
bookmarkCommandSelector = mkSelector "bookmarkCommand"

