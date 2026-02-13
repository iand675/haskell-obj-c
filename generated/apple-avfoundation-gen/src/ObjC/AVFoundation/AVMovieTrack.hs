{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVMovieTrack@.
module ObjC.AVFoundation.AVMovieTrack
  ( AVMovieTrack
  , IsAVMovieTrack(..)
  , alternateGroupID
  , mediaDataStorage
  , alternateGroupIDSelector
  , mediaDataStorageSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- alternateGroupID@
alternateGroupID :: IsAVMovieTrack avMovieTrack => avMovieTrack -> IO CLong
alternateGroupID avMovieTrack =
  sendMessage avMovieTrack alternateGroupIDSelector

-- | mediaDataStorage
--
-- The storage container for media data added to a track.
--
-- The value of this property is an AVMediaDataStorage object that indicates the location at which media data inserted or appended to the track will be written.
--
-- ObjC selector: @- mediaDataStorage@
mediaDataStorage :: IsAVMovieTrack avMovieTrack => avMovieTrack -> IO (Id AVMediaDataStorage)
mediaDataStorage avMovieTrack =
  sendMessage avMovieTrack mediaDataStorageSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alternateGroupID@
alternateGroupIDSelector :: Selector '[] CLong
alternateGroupIDSelector = mkSelector "alternateGroupID"

-- | @Selector@ for @mediaDataStorage@
mediaDataStorageSelector :: Selector '[] (Id AVMediaDataStorage)
mediaDataStorageSelector = mkSelector "mediaDataStorage"

