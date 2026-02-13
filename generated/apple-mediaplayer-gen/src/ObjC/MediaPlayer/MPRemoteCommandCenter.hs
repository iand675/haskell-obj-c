{-# LANGUAGE DataKinds #-}
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
  , bookmarkCommandSelector
  , changePlaybackPositionCommandSelector
  , changePlaybackRateCommandSelector
  , changeRepeatModeCommandSelector
  , changeShuffleModeCommandSelector
  , disableLanguageOptionCommandSelector
  , dislikeCommandSelector
  , enableLanguageOptionCommandSelector
  , initSelector
  , likeCommandSelector
  , newSelector
  , nextTrackCommandSelector
  , pauseCommandSelector
  , playCommandSelector
  , previousTrackCommandSelector
  , ratingCommandSelector
  , seekBackwardCommandSelector
  , seekForwardCommandSelector
  , sharedCommandCenterSelector
  , skipBackwardCommandSelector
  , skipForwardCommandSelector
  , stopCommandSelector
  , togglePlayPauseCommandSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sharedCommandCenter@
sharedCommandCenter :: IO (Id MPRemoteCommandCenter)
sharedCommandCenter  =
  do
    cls' <- getRequiredClass "MPRemoteCommandCenter"
    sendClassMessage cls' sharedCommandCenterSelector

-- | @+ new@
new :: IO (Id MPRemoteCommandCenter)
new  =
  do
    cls' <- getRequiredClass "MPRemoteCommandCenter"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRemoteCommandCenter)
init_ mpRemoteCommandCenter =
  sendOwnedMessage mpRemoteCommandCenter initSelector

-- | @- pauseCommand@
pauseCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRemoteCommand)
pauseCommand mpRemoteCommandCenter =
  sendMessage mpRemoteCommandCenter pauseCommandSelector

-- | @- playCommand@
playCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRemoteCommand)
playCommand mpRemoteCommandCenter =
  sendMessage mpRemoteCommandCenter playCommandSelector

-- | @- stopCommand@
stopCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRemoteCommand)
stopCommand mpRemoteCommandCenter =
  sendMessage mpRemoteCommandCenter stopCommandSelector

-- | @- togglePlayPauseCommand@
togglePlayPauseCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRemoteCommand)
togglePlayPauseCommand mpRemoteCommandCenter =
  sendMessage mpRemoteCommandCenter togglePlayPauseCommandSelector

-- | @- enableLanguageOptionCommand@
enableLanguageOptionCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRemoteCommand)
enableLanguageOptionCommand mpRemoteCommandCenter =
  sendMessage mpRemoteCommandCenter enableLanguageOptionCommandSelector

-- | @- disableLanguageOptionCommand@
disableLanguageOptionCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRemoteCommand)
disableLanguageOptionCommand mpRemoteCommandCenter =
  sendMessage mpRemoteCommandCenter disableLanguageOptionCommandSelector

-- | @- changePlaybackRateCommand@
changePlaybackRateCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPChangePlaybackRateCommand)
changePlaybackRateCommand mpRemoteCommandCenter =
  sendMessage mpRemoteCommandCenter changePlaybackRateCommandSelector

-- | @- changeRepeatModeCommand@
changeRepeatModeCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPChangeRepeatModeCommand)
changeRepeatModeCommand mpRemoteCommandCenter =
  sendMessage mpRemoteCommandCenter changeRepeatModeCommandSelector

-- | @- changeShuffleModeCommand@
changeShuffleModeCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPChangeShuffleModeCommand)
changeShuffleModeCommand mpRemoteCommandCenter =
  sendMessage mpRemoteCommandCenter changeShuffleModeCommandSelector

-- | @- nextTrackCommand@
nextTrackCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRemoteCommand)
nextTrackCommand mpRemoteCommandCenter =
  sendMessage mpRemoteCommandCenter nextTrackCommandSelector

-- | @- previousTrackCommand@
previousTrackCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRemoteCommand)
previousTrackCommand mpRemoteCommandCenter =
  sendMessage mpRemoteCommandCenter previousTrackCommandSelector

-- | @- skipForwardCommand@
skipForwardCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPSkipIntervalCommand)
skipForwardCommand mpRemoteCommandCenter =
  sendMessage mpRemoteCommandCenter skipForwardCommandSelector

-- | @- skipBackwardCommand@
skipBackwardCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPSkipIntervalCommand)
skipBackwardCommand mpRemoteCommandCenter =
  sendMessage mpRemoteCommandCenter skipBackwardCommandSelector

-- | @- seekForwardCommand@
seekForwardCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRemoteCommand)
seekForwardCommand mpRemoteCommandCenter =
  sendMessage mpRemoteCommandCenter seekForwardCommandSelector

-- | @- seekBackwardCommand@
seekBackwardCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRemoteCommand)
seekBackwardCommand mpRemoteCommandCenter =
  sendMessage mpRemoteCommandCenter seekBackwardCommandSelector

-- | @- changePlaybackPositionCommand@
changePlaybackPositionCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPChangePlaybackPositionCommand)
changePlaybackPositionCommand mpRemoteCommandCenter =
  sendMessage mpRemoteCommandCenter changePlaybackPositionCommandSelector

-- | @- ratingCommand@
ratingCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPRatingCommand)
ratingCommand mpRemoteCommandCenter =
  sendMessage mpRemoteCommandCenter ratingCommandSelector

-- | @- likeCommand@
likeCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPFeedbackCommand)
likeCommand mpRemoteCommandCenter =
  sendMessage mpRemoteCommandCenter likeCommandSelector

-- | @- dislikeCommand@
dislikeCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPFeedbackCommand)
dislikeCommand mpRemoteCommandCenter =
  sendMessage mpRemoteCommandCenter dislikeCommandSelector

-- | @- bookmarkCommand@
bookmarkCommand :: IsMPRemoteCommandCenter mpRemoteCommandCenter => mpRemoteCommandCenter -> IO (Id MPFeedbackCommand)
bookmarkCommand mpRemoteCommandCenter =
  sendMessage mpRemoteCommandCenter bookmarkCommandSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedCommandCenter@
sharedCommandCenterSelector :: Selector '[] (Id MPRemoteCommandCenter)
sharedCommandCenterSelector = mkSelector "sharedCommandCenter"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MPRemoteCommandCenter)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPRemoteCommandCenter)
initSelector = mkSelector "init"

-- | @Selector@ for @pauseCommand@
pauseCommandSelector :: Selector '[] (Id MPRemoteCommand)
pauseCommandSelector = mkSelector "pauseCommand"

-- | @Selector@ for @playCommand@
playCommandSelector :: Selector '[] (Id MPRemoteCommand)
playCommandSelector = mkSelector "playCommand"

-- | @Selector@ for @stopCommand@
stopCommandSelector :: Selector '[] (Id MPRemoteCommand)
stopCommandSelector = mkSelector "stopCommand"

-- | @Selector@ for @togglePlayPauseCommand@
togglePlayPauseCommandSelector :: Selector '[] (Id MPRemoteCommand)
togglePlayPauseCommandSelector = mkSelector "togglePlayPauseCommand"

-- | @Selector@ for @enableLanguageOptionCommand@
enableLanguageOptionCommandSelector :: Selector '[] (Id MPRemoteCommand)
enableLanguageOptionCommandSelector = mkSelector "enableLanguageOptionCommand"

-- | @Selector@ for @disableLanguageOptionCommand@
disableLanguageOptionCommandSelector :: Selector '[] (Id MPRemoteCommand)
disableLanguageOptionCommandSelector = mkSelector "disableLanguageOptionCommand"

-- | @Selector@ for @changePlaybackRateCommand@
changePlaybackRateCommandSelector :: Selector '[] (Id MPChangePlaybackRateCommand)
changePlaybackRateCommandSelector = mkSelector "changePlaybackRateCommand"

-- | @Selector@ for @changeRepeatModeCommand@
changeRepeatModeCommandSelector :: Selector '[] (Id MPChangeRepeatModeCommand)
changeRepeatModeCommandSelector = mkSelector "changeRepeatModeCommand"

-- | @Selector@ for @changeShuffleModeCommand@
changeShuffleModeCommandSelector :: Selector '[] (Id MPChangeShuffleModeCommand)
changeShuffleModeCommandSelector = mkSelector "changeShuffleModeCommand"

-- | @Selector@ for @nextTrackCommand@
nextTrackCommandSelector :: Selector '[] (Id MPRemoteCommand)
nextTrackCommandSelector = mkSelector "nextTrackCommand"

-- | @Selector@ for @previousTrackCommand@
previousTrackCommandSelector :: Selector '[] (Id MPRemoteCommand)
previousTrackCommandSelector = mkSelector "previousTrackCommand"

-- | @Selector@ for @skipForwardCommand@
skipForwardCommandSelector :: Selector '[] (Id MPSkipIntervalCommand)
skipForwardCommandSelector = mkSelector "skipForwardCommand"

-- | @Selector@ for @skipBackwardCommand@
skipBackwardCommandSelector :: Selector '[] (Id MPSkipIntervalCommand)
skipBackwardCommandSelector = mkSelector "skipBackwardCommand"

-- | @Selector@ for @seekForwardCommand@
seekForwardCommandSelector :: Selector '[] (Id MPRemoteCommand)
seekForwardCommandSelector = mkSelector "seekForwardCommand"

-- | @Selector@ for @seekBackwardCommand@
seekBackwardCommandSelector :: Selector '[] (Id MPRemoteCommand)
seekBackwardCommandSelector = mkSelector "seekBackwardCommand"

-- | @Selector@ for @changePlaybackPositionCommand@
changePlaybackPositionCommandSelector :: Selector '[] (Id MPChangePlaybackPositionCommand)
changePlaybackPositionCommandSelector = mkSelector "changePlaybackPositionCommand"

-- | @Selector@ for @ratingCommand@
ratingCommandSelector :: Selector '[] (Id MPRatingCommand)
ratingCommandSelector = mkSelector "ratingCommand"

-- | @Selector@ for @likeCommand@
likeCommandSelector :: Selector '[] (Id MPFeedbackCommand)
likeCommandSelector = mkSelector "likeCommand"

-- | @Selector@ for @dislikeCommand@
dislikeCommandSelector :: Selector '[] (Id MPFeedbackCommand)
dislikeCommandSelector = mkSelector "dislikeCommand"

-- | @Selector@ for @bookmarkCommand@
bookmarkCommandSelector :: Selector '[] (Id MPFeedbackCommand)
bookmarkCommandSelector = mkSelector "bookmarkCommand"

