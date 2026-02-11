{-# LANGUAGE PatternSynonyms #-}
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
  , setSuggestedMediaIntentsSelector
  , setPredictionMode_forTypeSelector
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

import ObjC.Intents.Internal.Classes
import ObjC.Intents.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- setSuggestedMediaIntents:@
setSuggestedMediaIntents :: (IsINUpcomingMediaManager inUpcomingMediaManager, IsNSOrderedSet intents) => inUpcomingMediaManager -> intents -> IO ()
setSuggestedMediaIntents inUpcomingMediaManager  intents =
withObjCPtr intents $ \raw_intents ->
    sendMsg inUpcomingMediaManager (mkSelector "setSuggestedMediaIntents:") retVoid [argPtr (castPtr raw_intents :: Ptr ())]

-- | @- setPredictionMode:forType:@
setPredictionMode_forType :: IsINUpcomingMediaManager inUpcomingMediaManager => inUpcomingMediaManager -> INUpcomingMediaPredictionMode -> INMediaItemType -> IO ()
setPredictionMode_forType inUpcomingMediaManager  mode type_ =
  sendMsg inUpcomingMediaManager (mkSelector "setPredictionMode:forType:") retVoid [argCLong (coerce mode), argCLong (coerce type_)]

-- | @+ sharedManager@
sharedManager :: IO (Id INUpcomingMediaManager)
sharedManager  =
  do
    cls' <- getRequiredClass "INUpcomingMediaManager"
    sendClassMsg cls' (mkSelector "sharedManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setSuggestedMediaIntents:@
setSuggestedMediaIntentsSelector :: Selector
setSuggestedMediaIntentsSelector = mkSelector "setSuggestedMediaIntents:"

-- | @Selector@ for @setPredictionMode:forType:@
setPredictionMode_forTypeSelector :: Selector
setPredictionMode_forTypeSelector = mkSelector "setPredictionMode:forType:"

-- | @Selector@ for @sharedManager@
sharedManagerSelector :: Selector
sharedManagerSelector = mkSelector "sharedManager"

