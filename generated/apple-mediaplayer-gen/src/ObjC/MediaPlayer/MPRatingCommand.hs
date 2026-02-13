{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPRatingCommand@.
module ObjC.MediaPlayer.MPRatingCommand
  ( MPRatingCommand
  , IsMPRatingCommand(..)
  , minimumRating
  , setMinimumRating
  , maximumRating
  , setMaximumRating
  , maximumRatingSelector
  , minimumRatingSelector
  , setMaximumRatingSelector
  , setMinimumRatingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Minimum rating for the command.
--
-- ObjC selector: @- minimumRating@
minimumRating :: IsMPRatingCommand mpRatingCommand => mpRatingCommand -> IO CFloat
minimumRating mpRatingCommand =
  sendMessage mpRatingCommand minimumRatingSelector

-- | Minimum rating for the command.
--
-- ObjC selector: @- setMinimumRating:@
setMinimumRating :: IsMPRatingCommand mpRatingCommand => mpRatingCommand -> CFloat -> IO ()
setMinimumRating mpRatingCommand value =
  sendMessage mpRatingCommand setMinimumRatingSelector value

-- | Maximum rating for the command.
--
-- ObjC selector: @- maximumRating@
maximumRating :: IsMPRatingCommand mpRatingCommand => mpRatingCommand -> IO CFloat
maximumRating mpRatingCommand =
  sendMessage mpRatingCommand maximumRatingSelector

-- | Maximum rating for the command.
--
-- ObjC selector: @- setMaximumRating:@
setMaximumRating :: IsMPRatingCommand mpRatingCommand => mpRatingCommand -> CFloat -> IO ()
setMaximumRating mpRatingCommand value =
  sendMessage mpRatingCommand setMaximumRatingSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @minimumRating@
minimumRatingSelector :: Selector '[] CFloat
minimumRatingSelector = mkSelector "minimumRating"

-- | @Selector@ for @setMinimumRating:@
setMinimumRatingSelector :: Selector '[CFloat] ()
setMinimumRatingSelector = mkSelector "setMinimumRating:"

-- | @Selector@ for @maximumRating@
maximumRatingSelector :: Selector '[] CFloat
maximumRatingSelector = mkSelector "maximumRating"

-- | @Selector@ for @setMaximumRating:@
setMaximumRatingSelector :: Selector '[CFloat] ()
setMaximumRatingSelector = mkSelector "setMaximumRating:"

