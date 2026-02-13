{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.MediaPlayer.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The type of seek command event, which specifies whether an external player began or ended seeking.
--
-- ObjC selector: @- type@
type_ :: IsMPSeekCommandEvent mpSeekCommandEvent => mpSeekCommandEvent -> IO MPSeekCommandEventType
type_ mpSeekCommandEvent =
  sendMessage mpSeekCommandEvent typeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @type@
typeSelector :: Selector '[] MPSeekCommandEventType
typeSelector = mkSelector "type"

