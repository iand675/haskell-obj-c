{-# LANGUAGE DataKinds #-}
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
  , confidenceSelector
  , frequencySkewSelector
  , matchOffsetSelector
  , predictedCurrentMatchOffsetSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
frequencySkew shMatchedMediaItem =
  sendMessage shMatchedMediaItem frequencySkewSelector

-- | The timecode in the reference recording that matches the start of the query, in seconds.
--
-- The value can be negative if the query signature contains unrecognizable data before the data that corresponds to the start of the matched reference item.
--
-- ObjC selector: @- matchOffset@
matchOffset :: IsSHMatchedMediaItem shMatchedMediaItem => shMatchedMediaItem -> IO CDouble
matchOffset shMatchedMediaItem =
  sendMessage shMatchedMediaItem matchOffsetSelector

-- | The updated timecode in the reference recording that matches the current playback position of the query audio, in seconds.
--
-- ObjC selector: @- predictedCurrentMatchOffset@
predictedCurrentMatchOffset :: IsSHMatchedMediaItem shMatchedMediaItem => shMatchedMediaItem -> IO CDouble
predictedCurrentMatchOffset shMatchedMediaItem =
  sendMessage shMatchedMediaItem predictedCurrentMatchOffsetSelector

-- | The level of confidence in the match result.
--
-- The value ranges from 0.0 to 1.0, where 1.0 indicates the highest level of confidence.
--
-- ObjC selector: @- confidence@
confidence :: IsSHMatchedMediaItem shMatchedMediaItem => shMatchedMediaItem -> IO CFloat
confidence shMatchedMediaItem =
  sendMessage shMatchedMediaItem confidenceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @frequencySkew@
frequencySkewSelector :: Selector '[] CFloat
frequencySkewSelector = mkSelector "frequencySkew"

-- | @Selector@ for @matchOffset@
matchOffsetSelector :: Selector '[] CDouble
matchOffsetSelector = mkSelector "matchOffset"

-- | @Selector@ for @predictedCurrentMatchOffset@
predictedCurrentMatchOffsetSelector :: Selector '[] CDouble
predictedCurrentMatchOffsetSelector = mkSelector "predictedCurrentMatchOffset"

-- | @Selector@ for @confidence@
confidenceSelector :: Selector '[] CFloat
confidenceSelector = mkSelector "confidence"

