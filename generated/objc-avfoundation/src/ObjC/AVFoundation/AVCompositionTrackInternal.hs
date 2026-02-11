{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCompositionTrack
--
-- AVCompositionTrack offers the low-level representation of tracks of AVCompositions, comprising a media type, a track identifier, and an array of AVCompositionTrackSegments, each comprising a URL, and track identifier, and a time mapping.
--
-- Generated bindings for @AVCompositionTrackInternal@.
module ObjC.AVFoundation.AVCompositionTrackInternal
  ( AVCompositionTrackInternal
  , IsAVCompositionTrackInternal(..)


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

