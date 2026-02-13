{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSkipIntervalCommandEvent@.
module ObjC.MediaPlayer.MPSkipIntervalCommandEvent
  ( MPSkipIntervalCommandEvent
  , IsMPSkipIntervalCommandEvent(..)
  , interval
  , intervalSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The chosen interval for this skip command event.
--
-- ObjC selector: @- interval@
interval :: IsMPSkipIntervalCommandEvent mpSkipIntervalCommandEvent => mpSkipIntervalCommandEvent -> IO CDouble
interval mpSkipIntervalCommandEvent =
  sendMessage mpSkipIntervalCommandEvent intervalSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @interval@
intervalSelector :: Selector '[] CDouble
intervalSelector = mkSelector "interval"

