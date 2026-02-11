{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMovieTrack
--
-- AVMovieTrack represents the tracks of audiovisual containers in a file that conforms to the QuickTime movie file format or to one of the related ISO base media file formats (such as MPEG-4).
--
-- Generated bindings for @AVMovieTrackInternal@.
module ObjC.AVFoundation.AVMovieTrackInternal
  ( AVMovieTrackInternal
  , IsAVMovieTrackInternal(..)


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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

