{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that represents the metadata for a matched reference signature.
--
-- To access properties for custom media items, use subscripting. For more information, see ``SHMediaItem``.
--
-- Generated bindings for @SHMatchedMediaItem@.
module ObjC.ShazamKit.SHMatchedMediaItem
  ( SHMatchedMediaItem
  , IsSHMatchedMediaItem(..)
  , frequencySkew
  , matchOffset
  , predictedCurrentMatchOffset
  , confidence
  , frequencySkewSelector
  , matchOffsetSelector
  , predictedCurrentMatchOffsetSelector
  , confidenceSelector


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

import ObjC.ShazamKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A multiple for the difference in frequency between the matched audio and the query audio.
--
-- A value of @0.0@ indicates that the query and matched audio are at the same frequency. Other values indicate that the query audio is playing at a different frequency. For example, if the original recording plays at @100@ Hz, a value of @0.05@ indicates that the query recording plays at @105@ Hz.
--
-- No match returns if the frequency skew is too large.
--
-- ObjC selector: @- frequencySkew@
frequencySkew :: IsSHMatchedMediaItem shMatchedMediaItem => shMatchedMediaItem -> IO CFloat
frequencySkew shMatchedMediaItem  =
  sendMsg shMatchedMediaItem (mkSelector "frequencySkew") retCFloat []

-- | The timecode in the reference recording that matches the start of the query, in seconds.
--
-- The value can be negative if the query signature contains unrecognizable data before the data that corresponds to the start of the matched reference item.
--
-- ObjC selector: @- matchOffset@
matchOffset :: IsSHMatchedMediaItem shMatchedMediaItem => shMatchedMediaItem -> IO CDouble
matchOffset shMatchedMediaItem  =
  sendMsg shMatchedMediaItem (mkSelector "matchOffset") retCDouble []

-- | The updated timecode in the reference recording that matches the current playback position of the query audio, in seconds.
--
-- ObjC selector: @- predictedCurrentMatchOffset@
predictedCurrentMatchOffset :: IsSHMatchedMediaItem shMatchedMediaItem => shMatchedMediaItem -> IO CDouble
predictedCurrentMatchOffset shMatchedMediaItem  =
  sendMsg shMatchedMediaItem (mkSelector "predictedCurrentMatchOffset") retCDouble []

-- | The level of confidence in the match result.
--
-- The value ranges from 0.0 to 1.0, where 1.0 indicates the highest level of confidence.
--
-- ObjC selector: @- confidence@
confidence :: IsSHMatchedMediaItem shMatchedMediaItem => shMatchedMediaItem -> IO CFloat
confidence shMatchedMediaItem  =
  sendMsg shMatchedMediaItem (mkSelector "confidence") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @frequencySkew@
frequencySkewSelector :: Selector
frequencySkewSelector = mkSelector "frequencySkew"

-- | @Selector@ for @matchOffset@
matchOffsetSelector :: Selector
matchOffsetSelector = mkSelector "matchOffset"

-- | @Selector@ for @predictedCurrentMatchOffset@
predictedCurrentMatchOffsetSelector :: Selector
predictedCurrentMatchOffsetSelector = mkSelector "predictedCurrentMatchOffset"

-- | @Selector@ for @confidence@
confidenceSelector :: Selector
confidenceSelector = mkSelector "confidence"

