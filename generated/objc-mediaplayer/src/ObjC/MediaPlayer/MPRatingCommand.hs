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
  , minimumRatingSelector
  , setMinimumRatingSelector
  , maximumRatingSelector
  , setMaximumRatingSelector


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

-- | Minimum rating for the command.
--
-- ObjC selector: @- minimumRating@
minimumRating :: IsMPRatingCommand mpRatingCommand => mpRatingCommand -> IO CFloat
minimumRating mpRatingCommand  =
  sendMsg mpRatingCommand (mkSelector "minimumRating") retCFloat []

-- | Minimum rating for the command.
--
-- ObjC selector: @- setMinimumRating:@
setMinimumRating :: IsMPRatingCommand mpRatingCommand => mpRatingCommand -> CFloat -> IO ()
setMinimumRating mpRatingCommand  value =
  sendMsg mpRatingCommand (mkSelector "setMinimumRating:") retVoid [argCFloat (fromIntegral value)]

-- | Maximum rating for the command.
--
-- ObjC selector: @- maximumRating@
maximumRating :: IsMPRatingCommand mpRatingCommand => mpRatingCommand -> IO CFloat
maximumRating mpRatingCommand  =
  sendMsg mpRatingCommand (mkSelector "maximumRating") retCFloat []

-- | Maximum rating for the command.
--
-- ObjC selector: @- setMaximumRating:@
setMaximumRating :: IsMPRatingCommand mpRatingCommand => mpRatingCommand -> CFloat -> IO ()
setMaximumRating mpRatingCommand  value =
  sendMsg mpRatingCommand (mkSelector "setMaximumRating:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @minimumRating@
minimumRatingSelector :: Selector
minimumRatingSelector = mkSelector "minimumRating"

-- | @Selector@ for @setMinimumRating:@
setMinimumRatingSelector :: Selector
setMinimumRatingSelector = mkSelector "setMinimumRating:"

-- | @Selector@ for @maximumRating@
maximumRatingSelector :: Selector
maximumRatingSelector = mkSelector "maximumRating"

-- | @Selector@ for @setMaximumRating:@
setMaximumRatingSelector :: Selector
setMaximumRatingSelector = mkSelector "setMaximumRating:"

