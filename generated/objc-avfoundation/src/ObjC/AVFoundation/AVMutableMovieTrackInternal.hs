{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMutableMovieTrack
--
-- AVMutableMovieTrack provides the track-level editing interface of an AVMutableMovie. Media can be inserted into a movie track and other editing operations performed via an instance of this class.
--
-- Generated bindings for @AVMutableMovieTrackInternal@.
module ObjC.AVFoundation.AVMutableMovieTrackInternal
  ( AVMutableMovieTrackInternal
  , IsAVMutableMovieTrackInternal(..)


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

