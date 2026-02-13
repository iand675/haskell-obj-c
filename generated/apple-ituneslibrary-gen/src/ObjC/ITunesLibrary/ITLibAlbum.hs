{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The ITLibAlbum class represents an album where a given media item (ITLibMediaItem) is contained.
--
-- Generated bindings for @ITLibAlbum@.
module ObjC.ITunesLibrary.ITLibAlbum
  ( ITLibAlbum
  , IsITLibAlbum(..)
  , title
  , sortTitle
  , compilation
  , artist
  , discCount
  , discNumber
  , rating
  , ratingComputed
  , gapless
  , trackCount
  , albumArtist
  , sortAlbumArtist
  , persistentID
  , albumArtistSelector
  , artistSelector
  , compilationSelector
  , discCountSelector
  , discNumberSelector
  , gaplessSelector
  , persistentIDSelector
  , ratingComputedSelector
  , ratingSelector
  , sortAlbumArtistSelector
  , sortTitleSelector
  , titleSelector
  , trackCountSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ITunesLibrary.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The name of this album.
--
-- ObjC selector: @- title@
title :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO (Id NSString)
title itLibAlbum =
  sendMessage itLibAlbum titleSelector

-- | The name of this that should be used for sorting purposes.
--
-- ObjC selector: @- sortTitle@
sortTitle :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO (Id NSString)
sortTitle itLibAlbum =
  sendMessage itLibAlbum sortTitleSelector

-- | Whether this album is a compilation.
--
-- ObjC selector: @- compilation@
compilation :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO Bool
compilation itLibAlbum =
  sendMessage itLibAlbum compilationSelector

-- | Deprecated. Will be removed in future versions.
--
-- ObjC selector: @- artist@
artist :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO RawId
artist itLibAlbum =
  sendMessage itLibAlbum artistSelector

-- | The number of discs in this album.
--
-- ObjC selector: @- discCount@
discCount :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO CULong
discCount itLibAlbum =
  sendMessage itLibAlbum discCountSelector

-- | The index (i.e. 1, 2, 3, etc.) of the disc this album refers to within a compilation.
--
-- ObjC selector: @- discNumber@
discNumber :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO CULong
discNumber itLibAlbum =
  sendMessage itLibAlbum discNumberSelector

-- | The rating of this track's album.
--
-- ObjC selector: @- rating@
rating :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO CLong
rating itLibAlbum =
  sendMessage itLibAlbum ratingSelector

-- | The rating of this track's album.
--
-- ObjC selector: @- ratingComputed@
ratingComputed :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO Bool
ratingComputed itLibAlbum =
  sendMessage itLibAlbum ratingComputedSelector

-- | Whether this track's album is gapless.
--
-- ObjC selector: @- gapless@
gapless :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO Bool
gapless itLibAlbum =
  sendMessage itLibAlbum gaplessSelector

-- | Number of tracks in this album.
--
-- ObjC selector: @- trackCount@
trackCount :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO CULong
trackCount itLibAlbum =
  sendMessage itLibAlbum trackCountSelector

-- | The artist associated with this album.
--
-- ObjC selector: @- albumArtist@
albumArtist :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO (Id NSString)
albumArtist itLibAlbum =
  sendMessage itLibAlbum albumArtistSelector

-- | The artist associated with this album. This field should be used when sorting.
--
-- ObjC selector: @- sortAlbumArtist@
sortAlbumArtist :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO (Id NSString)
sortAlbumArtist itLibAlbum =
  sendMessage itLibAlbum sortAlbumArtistSelector

-- | The unique identifier of this album.
--
-- ObjC selector: @- persistentID@
persistentID :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO (Id NSNumber)
persistentID itLibAlbum =
  sendMessage itLibAlbum persistentIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @sortTitle@
sortTitleSelector :: Selector '[] (Id NSString)
sortTitleSelector = mkSelector "sortTitle"

-- | @Selector@ for @compilation@
compilationSelector :: Selector '[] Bool
compilationSelector = mkSelector "compilation"

-- | @Selector@ for @artist@
artistSelector :: Selector '[] RawId
artistSelector = mkSelector "artist"

-- | @Selector@ for @discCount@
discCountSelector :: Selector '[] CULong
discCountSelector = mkSelector "discCount"

-- | @Selector@ for @discNumber@
discNumberSelector :: Selector '[] CULong
discNumberSelector = mkSelector "discNumber"

-- | @Selector@ for @rating@
ratingSelector :: Selector '[] CLong
ratingSelector = mkSelector "rating"

-- | @Selector@ for @ratingComputed@
ratingComputedSelector :: Selector '[] Bool
ratingComputedSelector = mkSelector "ratingComputed"

-- | @Selector@ for @gapless@
gaplessSelector :: Selector '[] Bool
gaplessSelector = mkSelector "gapless"

-- | @Selector@ for @trackCount@
trackCountSelector :: Selector '[] CULong
trackCountSelector = mkSelector "trackCount"

-- | @Selector@ for @albumArtist@
albumArtistSelector :: Selector '[] (Id NSString)
albumArtistSelector = mkSelector "albumArtist"

-- | @Selector@ for @sortAlbumArtist@
sortAlbumArtistSelector :: Selector '[] (Id NSString)
sortAlbumArtistSelector = mkSelector "sortAlbumArtist"

-- | @Selector@ for @persistentID@
persistentIDSelector :: Selector '[] (Id NSNumber)
persistentIDSelector = mkSelector "persistentID"

