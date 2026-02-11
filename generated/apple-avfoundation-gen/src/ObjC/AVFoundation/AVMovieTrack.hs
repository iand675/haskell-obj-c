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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- alternateGroupID@
alternateGroupID :: IsAVMovieTrack avMovieTrack => avMovieTrack -> IO CLong
alternateGroupID avMovieTrack  =
    sendMsg avMovieTrack (mkSelector "alternateGroupID") retCLong []

-- | mediaDataStorage
--
-- The storage container for media data added to a track.
--
-- The value of this property is an AVMediaDataStorage object that indicates the location at which media data inserted or appended to the track will be written.
--
-- ObjC selector: @- mediaDataStorage@
mediaDataStorage :: IsAVMovieTrack avMovieTrack => avMovieTrack -> IO (Id AVMediaDataStorage)
mediaDataStorage avMovieTrack  =
    sendMsg avMovieTrack (mkSelector "mediaDataStorage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @alternateGroupID@
alternateGroupIDSelector :: Selector
alternateGroupIDSelector = mkSelector "alternateGroupID"

-- | @Selector@ for @mediaDataStorage@
mediaDataStorageSelector :: Selector
mediaDataStorageSelector = mkSelector "mediaDataStorage"

