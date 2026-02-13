{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The chosen rating for this command event. This value will be within the minimumRating and maximumRating values set for the MPRatingCommand object.
--
-- ObjC selector: @- rating@
rating :: IsMPRatingCommandEvent mpRatingCommandEvent => mpRatingCommandEvent -> IO CFloat
rating mpRatingCommandEvent =
  sendMessage mpRatingCommandEvent ratingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @rating@
ratingSelector :: Selector '[] CFloat
ratingSelector = mkSelector "rating"

