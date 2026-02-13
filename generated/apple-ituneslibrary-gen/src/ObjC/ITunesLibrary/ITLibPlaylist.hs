{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A playlist is a collection of related media items. (Media items are described in ITLibMediaItem Class Reference.) 			Each playlist has a name, a set of attributes, and a unique identifier that persists across application launches.
--
-- Generated bindings for @ITLibPlaylist@.
module ObjC.ITunesLibrary.ITLibPlaylist
  ( ITLibPlaylist
  , IsITLibPlaylist(..)
  , name
  , primary
  , parentID
  , visible
  , allItemsPlaylist
  , items
  , distinguishedKind
  , kind
  , master
  , allItemsPlaylistSelector
  , distinguishedKindSelector
  , itemsSelector
  , kindSelector
  , masterSelector
  , nameSelector
  , parentIDSelector
  , primarySelector
  , visibleSelector

  -- * Enum types
  , ITLibDistinguishedPlaylistKind(ITLibDistinguishedPlaylistKind)
  , pattern ITLibDistinguishedPlaylistKindNone
  , pattern ITLibDistinguishedPlaylistKindMovies
  , pattern ITLibDistinguishedPlaylistKindTVShows
  , pattern ITLibDistinguishedPlaylistKindMusic
  , pattern ITLibDistinguishedPlaylistKindAudiobooks
  , pattern ITLibDistinguishedPlaylistKindBooks
  , pattern ITLibDistinguishedPlaylistKindRingtones
  , pattern ITLibDistinguishedPlaylistKindPodcasts
  , pattern ITLibDistinguishedPlaylistKindVoiceMemos
  , pattern ITLibDistinguishedPlaylistKindPurchases
  , pattern ITLibDistinguishedPlaylistKindiTunesU
  , pattern ITLibDistinguishedPlaylistKind90sMusic
  , pattern ITLibDistinguishedPlaylistKindMyTopRated
  , pattern ITLibDistinguishedPlaylistKindTop25MostPlayed
  , pattern ITLibDistinguishedPlaylistKindRecentlyPlayed
  , pattern ITLibDistinguishedPlaylistKindRecentlyAdded
  , pattern ITLibDistinguishedPlaylistKindMusicVideos
  , pattern ITLibDistinguishedPlaylistKindClassicalMusic
  , pattern ITLibDistinguishedPlaylistKindLibraryMusicVideos
  , pattern ITLibDistinguishedPlaylistKindHomeVideos
  , pattern ITLibDistinguishedPlaylistKindApplications
  , pattern ITLibDistinguishedPlaylistKindLovedSongs
  , pattern ITLibDistinguishedPlaylistKindMusicShowsAndMovies
  , ITLibPlaylistKind(ITLibPlaylistKind)
  , pattern ITLibPlaylistKindRegular
  , pattern ITLibPlaylistKindSmart
  , pattern ITLibPlaylistKindGenius
  , pattern ITLibPlaylistKindFolder
  , pattern ITLibPlaylistKindGeniusMix

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ITunesLibrary.Internal.Classes
import ObjC.ITunesLibrary.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The name or title of this playlist.
--
-- ObjC selector: @- name@
name :: IsITLibPlaylist itLibPlaylist => itLibPlaylist -> IO (Id NSString)
name itLibPlaylist =
  sendMessage itLibPlaylist nameSelector

-- | Whether this playlist is the primary playlist.
--
-- ObjC selector: @- primary@
primary :: IsITLibPlaylist itLibPlaylist => itLibPlaylist -> IO Bool
primary itLibPlaylist =
  sendMessage itLibPlaylist primarySelector

-- | The unique identifier of this playlist' parent.
--
-- ObjC selector: @- parentID@
parentID :: IsITLibPlaylist itLibPlaylist => itLibPlaylist -> IO (Id NSNumber)
parentID itLibPlaylist =
  sendMessage itLibPlaylist parentIDSelector

-- | Whether this playlist is visible.
--
-- ObjC selector: @- visible@
visible :: IsITLibPlaylist itLibPlaylist => itLibPlaylist -> IO Bool
visible itLibPlaylist =
  sendMessage itLibPlaylist visibleSelector

-- | Whether or not every item in this playlist is exposed via this API.  Generally true but not that useful.
--
-- ObjC selector: @- allItemsPlaylist@
allItemsPlaylist :: IsITLibPlaylist itLibPlaylist => itLibPlaylist -> IO Bool
allItemsPlaylist itLibPlaylist =
  sendMessage itLibPlaylist allItemsPlaylistSelector

-- | The media items contained within this playlist.
--
-- ObjC selector: @- items@
items :: IsITLibPlaylist itLibPlaylist => itLibPlaylist -> IO (Id NSArray)
items itLibPlaylist =
  sendMessage itLibPlaylist itemsSelector

-- | The distinguished kind of this playlist.
--
-- ObjC selector: @- distinguishedKind@
distinguishedKind :: IsITLibPlaylist itLibPlaylist => itLibPlaylist -> IO ITLibDistinguishedPlaylistKind
distinguishedKind itLibPlaylist =
  sendMessage itLibPlaylist distinguishedKindSelector

-- | The kind of this playlist.
--
-- ObjC selector: @- kind@
kind :: IsITLibPlaylist itLibPlaylist => itLibPlaylist -> IO ITLibPlaylistKind
kind itLibPlaylist =
  sendMessage itLibPlaylist kindSelector

-- | Whether this playlist is the primary playlist.
--
-- ObjC selector: @- master@
master :: IsITLibPlaylist itLibPlaylist => itLibPlaylist -> IO Bool
master itLibPlaylist =
  sendMessage itLibPlaylist masterSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @primary@
primarySelector :: Selector '[] Bool
primarySelector = mkSelector "primary"

-- | @Selector@ for @parentID@
parentIDSelector :: Selector '[] (Id NSNumber)
parentIDSelector = mkSelector "parentID"

-- | @Selector@ for @visible@
visibleSelector :: Selector '[] Bool
visibleSelector = mkSelector "visible"

-- | @Selector@ for @allItemsPlaylist@
allItemsPlaylistSelector :: Selector '[] Bool
allItemsPlaylistSelector = mkSelector "allItemsPlaylist"

-- | @Selector@ for @items@
itemsSelector :: Selector '[] (Id NSArray)
itemsSelector = mkSelector "items"

-- | @Selector@ for @distinguishedKind@
distinguishedKindSelector :: Selector '[] ITLibDistinguishedPlaylistKind
distinguishedKindSelector = mkSelector "distinguishedKind"

-- | @Selector@ for @kind@
kindSelector :: Selector '[] ITLibPlaylistKind
kindSelector = mkSelector "kind"

-- | @Selector@ for @master@
masterSelector :: Selector '[] Bool
masterSelector = mkSelector "master"

