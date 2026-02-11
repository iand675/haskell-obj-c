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
  , titleSelector
  , sortTitleSelector
  , compilationSelector
  , artistSelector
  , discCountSelector
  , discNumberSelector
  , ratingSelector
  , ratingComputedSelector
  , gaplessSelector
  , trackCountSelector
  , albumArtistSelector
  , sortAlbumArtistSelector
  , persistentIDSelector


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

import ObjC.ITunesLibrary.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The name of this album.
--
-- ObjC selector: @- title@
title :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO (Id NSString)
title itLibAlbum  =
    sendMsg itLibAlbum (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The name of this that should be used for sorting purposes.
--
-- ObjC selector: @- sortTitle@
sortTitle :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO (Id NSString)
sortTitle itLibAlbum  =
    sendMsg itLibAlbum (mkSelector "sortTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Whether this album is a compilation.
--
-- ObjC selector: @- compilation@
compilation :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO Bool
compilation itLibAlbum  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg itLibAlbum (mkSelector "compilation") retCULong []

-- | Deprecated. Will be removed in future versions.
--
-- ObjC selector: @- artist@
artist :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO RawId
artist itLibAlbum  =
    fmap (RawId . castPtr) $ sendMsg itLibAlbum (mkSelector "artist") (retPtr retVoid) []

-- | The number of discs in this album.
--
-- ObjC selector: @- discCount@
discCount :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO CULong
discCount itLibAlbum  =
    sendMsg itLibAlbum (mkSelector "discCount") retCULong []

-- | The index (i.e. 1, 2, 3, etc.) of the disc this album refers to within a compilation.
--
-- ObjC selector: @- discNumber@
discNumber :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO CULong
discNumber itLibAlbum  =
    sendMsg itLibAlbum (mkSelector "discNumber") retCULong []

-- | The rating of this track's album.
--
-- ObjC selector: @- rating@
rating :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO CLong
rating itLibAlbum  =
    sendMsg itLibAlbum (mkSelector "rating") retCLong []

-- | The rating of this track's album.
--
-- ObjC selector: @- ratingComputed@
ratingComputed :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO Bool
ratingComputed itLibAlbum  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg itLibAlbum (mkSelector "ratingComputed") retCULong []

-- | Whether this track's album is gapless.
--
-- ObjC selector: @- gapless@
gapless :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO Bool
gapless itLibAlbum  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg itLibAlbum (mkSelector "gapless") retCULong []

-- | Number of tracks in this album.
--
-- ObjC selector: @- trackCount@
trackCount :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO CULong
trackCount itLibAlbum  =
    sendMsg itLibAlbum (mkSelector "trackCount") retCULong []

-- | The artist associated with this album.
--
-- ObjC selector: @- albumArtist@
albumArtist :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO (Id NSString)
albumArtist itLibAlbum  =
    sendMsg itLibAlbum (mkSelector "albumArtist") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The artist associated with this album. This field should be used when sorting.
--
-- ObjC selector: @- sortAlbumArtist@
sortAlbumArtist :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO (Id NSString)
sortAlbumArtist itLibAlbum  =
    sendMsg itLibAlbum (mkSelector "sortAlbumArtist") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The unique identifier of this album.
--
-- ObjC selector: @- persistentID@
persistentID :: IsITLibAlbum itLibAlbum => itLibAlbum -> IO (Id NSNumber)
persistentID itLibAlbum  =
    sendMsg itLibAlbum (mkSelector "persistentID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @sortTitle@
sortTitleSelector :: Selector
sortTitleSelector = mkSelector "sortTitle"

-- | @Selector@ for @compilation@
compilationSelector :: Selector
compilationSelector = mkSelector "compilation"

-- | @Selector@ for @artist@
artistSelector :: Selector
artistSelector = mkSelector "artist"

-- | @Selector@ for @discCount@
discCountSelector :: Selector
discCountSelector = mkSelector "discCount"

-- | @Selector@ for @discNumber@
discNumberSelector :: Selector
discNumberSelector = mkSelector "discNumber"

-- | @Selector@ for @rating@
ratingSelector :: Selector
ratingSelector = mkSelector "rating"

-- | @Selector@ for @ratingComputed@
ratingComputedSelector :: Selector
ratingComputedSelector = mkSelector "ratingComputed"

-- | @Selector@ for @gapless@
gaplessSelector :: Selector
gaplessSelector = mkSelector "gapless"

-- | @Selector@ for @trackCount@
trackCountSelector :: Selector
trackCountSelector = mkSelector "trackCount"

-- | @Selector@ for @albumArtist@
albumArtistSelector :: Selector
albumArtistSelector = mkSelector "albumArtist"

-- | @Selector@ for @sortAlbumArtist@
sortAlbumArtistSelector :: Selector
sortAlbumArtistSelector = mkSelector "sortAlbumArtist"

-- | @Selector@ for @persistentID@
persistentIDSelector :: Selector
persistentIDSelector = mkSelector "persistentID"

