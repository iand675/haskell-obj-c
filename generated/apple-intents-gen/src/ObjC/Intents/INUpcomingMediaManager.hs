{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INUpcomingMediaManager@.
module ObjC.Intents.INUpcomingMediaManager
  ( INUpcomingMediaManager
  , IsINUpcomingMediaManager(..)
  , setSuggestedMediaIntents
  , setPredictionMode_forType
  , sharedManager
  , setPredictionMode_forTypeSelector
  , setSuggestedMediaIntentsSelector
  , sharedManagerSelector

  -- * Enum types
  , INMediaItemType(INMediaItemType)
  , pattern INMediaItemTypeUnknown
  , pattern INMediaItemTypeSong
  , pattern INMediaItemTypeAlbum
  , pattern INMediaItemTypeArtist
  , pattern INMediaItemTypeGenre
  , pattern INMediaItemTypePlaylist
  , pattern INMediaItemTypePodcastShow
  , pattern INMediaItemTypePodcastEpisode
  , pattern INMediaItemTypePodcastPlaylist
  , pattern INMediaItemTypeMusicStation
  , pattern INMediaItemTypeAudioBook
  , pattern INMediaItemTypeMovie
  , pattern INMediaItemTypeTVShow
  , pattern INMediaItemTypeTVShowEpisode
  , pattern INMediaItemTypeMusicVideo
  , pattern INMediaItemTypePodcastStation
  , pattern INMediaItemTypeRadioStation
  , pattern INMediaItemTypeStation
  , pattern INMediaItemTypeMusic
  , pattern INMediaItemTypeAlgorithmicRadioStation
  , pattern INMediaItemTypeNews
  , INUpcomingMediaPredictionMode(INUpcomingMediaPredictionMode)
  , pattern INUpcomingMediaPredictionModeDefault
  , pattern INUpcomingMediaPredictionModeOnlyPredictSuggestedIntents

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- setSuggestedMediaIntents:@
setSuggestedMediaIntents :: (IsINUpcomingMediaManager inUpcomingMediaManager, IsNSOrderedSet intents) => inUpcomingMediaManager -> intents -> IO ()
setSuggestedMediaIntents inUpcomingMediaManager intents =
  sendMessage inUpcomingMediaManager setSuggestedMediaIntentsSelector (toNSOrderedSet intents)

-- | @- setPredictionMode:forType:@
setPredictionMode_forType :: IsINUpcomingMediaManager inUpcomingMediaManager => inUpcomingMediaManager -> INUpcomingMediaPredictionMode -> INMediaItemType -> IO ()
setPredictionMode_forType inUpcomingMediaManager mode type_ =
  sendMessage inUpcomingMediaManager setPredictionMode_forTypeSelector mode type_

-- | @+ sharedManager@
sharedManager :: IO (Id INUpcomingMediaManager)
sharedManager  =
  do
    cls' <- getRequiredClass "INUpcomingMediaManager"
    sendClassMessage cls' sharedManagerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setSuggestedMediaIntents:@
setSuggestedMediaIntentsSelector :: Selector '[Id NSOrderedSet] ()
setSuggestedMediaIntentsSelector = mkSelector "setSuggestedMediaIntents:"

-- | @Selector@ for @setPredictionMode:forType:@
setPredictionMode_forTypeSelector :: Selector '[INUpcomingMediaPredictionMode, INMediaItemType] ()
setPredictionMode_forTypeSelector = mkSelector "setPredictionMode:forType:"

-- | @Selector@ for @sharedManager@
sharedManagerSelector :: Selector '[] (Id INUpcomingMediaManager)
sharedManagerSelector = mkSelector "sharedManager"

