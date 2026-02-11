{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMutableCompositionTrack
--
-- AVMutableCompositionTrack provides a convenient interface for insertions, removals, and scaling of track segments without direct manipulation of their low-level representation.
--
-- Generated bindings for @AVMutableCompositionTrackInternal@.
module ObjC.AVFoundation.AVMutableCompositionTrackInternal
  ( AVMutableCompositionTrackInternal
  , IsAVMutableCompositionTrackInternal(..)


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

