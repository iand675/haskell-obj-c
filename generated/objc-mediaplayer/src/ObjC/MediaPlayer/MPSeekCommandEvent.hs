{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSeekCommandEvent@.
module ObjC.MediaPlayer.MPSeekCommandEvent
  ( MPSeekCommandEvent
  , IsMPSeekCommandEvent(..)
  , type_
  , typeSelector

  -- * Enum types
  , MPSeekCommandEventType(MPSeekCommandEventType)
  , pattern MPSeekCommandEventTypeBeginSeeking
  , pattern MPSeekCommandEventTypeEndSeeking

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

import ObjC.MediaPlayer.Internal.Classes
import ObjC.MediaPlayer.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The type of seek command event, which specifies whether an external player began or ended seeking.
--
-- ObjC selector: @- type@
type_ :: IsMPSeekCommandEvent mpSeekCommandEvent => mpSeekCommandEvent -> IO MPSeekCommandEventType
type_ mpSeekCommandEvent  =
  fmap (coerce :: CULong -> MPSeekCommandEventType) $ sendMsg mpSeekCommandEvent (mkSelector "type") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

