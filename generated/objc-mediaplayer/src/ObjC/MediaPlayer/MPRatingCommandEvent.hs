{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPRatingCommandEvent@.
module ObjC.MediaPlayer.MPRatingCommandEvent
  ( MPRatingCommandEvent
  , IsMPRatingCommandEvent(..)
  , rating
  , ratingSelector


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

-- | The chosen rating for this command event. This value will be within the minimumRating and maximumRating values set for the MPRatingCommand object.
--
-- ObjC selector: @- rating@
rating :: IsMPRatingCommandEvent mpRatingCommandEvent => mpRatingCommandEvent -> IO CFloat
rating mpRatingCommandEvent  =
  sendMsg mpRatingCommandEvent (mkSelector "rating") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rating@
ratingSelector :: Selector
ratingSelector = mkSelector "rating"

