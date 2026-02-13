{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChannelClusterSeriesInfoStruct@.
module ObjC.Matter.MTRChannelClusterSeriesInfoStruct
  ( MTRChannelClusterSeriesInfoStruct
  , IsMTRChannelClusterSeriesInfoStruct(..)
  , season
  , setSeason
  , episode
  , setEpisode
  , episodeSelector
  , seasonSelector
  , setEpisodeSelector
  , setSeasonSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- season@
season :: IsMTRChannelClusterSeriesInfoStruct mtrChannelClusterSeriesInfoStruct => mtrChannelClusterSeriesInfoStruct -> IO (Id NSString)
season mtrChannelClusterSeriesInfoStruct =
  sendMessage mtrChannelClusterSeriesInfoStruct seasonSelector

-- | @- setSeason:@
setSeason :: (IsMTRChannelClusterSeriesInfoStruct mtrChannelClusterSeriesInfoStruct, IsNSString value) => mtrChannelClusterSeriesInfoStruct -> value -> IO ()
setSeason mtrChannelClusterSeriesInfoStruct value =
  sendMessage mtrChannelClusterSeriesInfoStruct setSeasonSelector (toNSString value)

-- | @- episode@
episode :: IsMTRChannelClusterSeriesInfoStruct mtrChannelClusterSeriesInfoStruct => mtrChannelClusterSeriesInfoStruct -> IO (Id NSString)
episode mtrChannelClusterSeriesInfoStruct =
  sendMessage mtrChannelClusterSeriesInfoStruct episodeSelector

-- | @- setEpisode:@
setEpisode :: (IsMTRChannelClusterSeriesInfoStruct mtrChannelClusterSeriesInfoStruct, IsNSString value) => mtrChannelClusterSeriesInfoStruct -> value -> IO ()
setEpisode mtrChannelClusterSeriesInfoStruct value =
  sendMessage mtrChannelClusterSeriesInfoStruct setEpisodeSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @season@
seasonSelector :: Selector '[] (Id NSString)
seasonSelector = mkSelector "season"

-- | @Selector@ for @setSeason:@
setSeasonSelector :: Selector '[Id NSString] ()
setSeasonSelector = mkSelector "setSeason:"

-- | @Selector@ for @episode@
episodeSelector :: Selector '[] (Id NSString)
episodeSelector = mkSelector "episode"

-- | @Selector@ for @setEpisode:@
setEpisodeSelector :: Selector '[Id NSString] ()
setEpisodeSelector = mkSelector "setEpisode:"

