{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPFeedbackCommandEvent@.
module ObjC.MediaPlayer.MPFeedbackCommandEvent
  ( MPFeedbackCommandEvent
  , IsMPFeedbackCommandEvent(..)
  , negative
  , negativeSelector


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

-- | Whether the command event is a negative operation. For example, the command might ask that the app remove a bookmark for a particular track, rather than add it. In this case, the handler for the bookmark command should check this flag and remove the bookmark if it is set to YES.
--
-- For like/dislike, a "negative like" might be treated differently from a dislike command. The app might want to remove the "like" flag from the current track, but not treat it as a dislike command.
--
-- ObjC selector: @- negative@
negative :: IsMPFeedbackCommandEvent mpFeedbackCommandEvent => mpFeedbackCommandEvent -> IO Bool
negative mpFeedbackCommandEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpFeedbackCommandEvent (mkSelector "negative") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @negative@
negativeSelector :: Selector
negativeSelector = mkSelector "negative"

