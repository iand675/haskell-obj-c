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
import ObjC.Foundation.Internal.Classes

-- | The chosen interval for this skip command event.
--
-- ObjC selector: @- interval@
interval :: IsMPSkipIntervalCommandEvent mpSkipIntervalCommandEvent => mpSkipIntervalCommandEvent -> IO CDouble
interval mpSkipIntervalCommandEvent  =
  sendMsg mpSkipIntervalCommandEvent (mkSelector "interval") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @interval@
intervalSelector :: Selector
intervalSelector = mkSelector "interval"

