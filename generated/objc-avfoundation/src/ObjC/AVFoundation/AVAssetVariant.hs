{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An AVAssetVariant represents a bit rate variant. Each asset contains a collection of variants that represent a combination of audio, video, text, closed captions, and subtitles for a particular bit rate. Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVAssetVariant@.
module ObjC.AVFoundation.AVAssetVariant
  ( AVAssetVariant
  , IsAVAssetVariant(..)
  , init_
  , new
  , peakBitRate
  , averageBitRate
  , videoAttributes
  , audioAttributes
  , url
  , initSelector
  , newSelector
  , peakBitRateSelector
  , averageBitRateSelector
  , videoAttributesSelector
  , audioAttributesSelector
  , urlSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetVariant avAssetVariant => avAssetVariant -> IO (Id AVAssetVariant)
init_ avAssetVariant  =
  sendMsg avAssetVariant (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetVariant)
new  =
  do
    cls' <- getRequiredClass "AVAssetVariant"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | If it is not declared, the value will be negative.
--
-- ObjC selector: @- peakBitRate@
peakBitRate :: IsAVAssetVariant avAssetVariant => avAssetVariant -> IO CDouble
peakBitRate avAssetVariant  =
  sendMsg avAssetVariant (mkSelector "peakBitRate") retCDouble []

-- | If it is not declared, the value will be negative.
--
-- ObjC selector: @- averageBitRate@
averageBitRate :: IsAVAssetVariant avAssetVariant => avAssetVariant -> IO CDouble
averageBitRate avAssetVariant  =
  sendMsg avAssetVariant (mkSelector "averageBitRate") retCDouble []

-- | Provides variant's video rendition attributes. If no video attributes are declared, it will be nil.
--
-- ObjC selector: @- videoAttributes@
videoAttributes :: IsAVAssetVariant avAssetVariant => avAssetVariant -> IO (Id AVAssetVariantVideoAttributes)
videoAttributes avAssetVariant  =
  sendMsg avAssetVariant (mkSelector "videoAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides variant's audio rendition attributes. If no audio attributes are declared, it will be nil.
--
-- ObjC selector: @- audioAttributes@
audioAttributes :: IsAVAssetVariant avAssetVariant => avAssetVariant -> IO (Id AVAssetVariantAudioAttributes)
audioAttributes avAssetVariant  =
  sendMsg avAssetVariant (mkSelector "audioAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides URL to media playlist corresponding to variant
--
-- ObjC selector: @- URL@
url :: IsAVAssetVariant avAssetVariant => avAssetVariant -> IO (Id NSURL)
url avAssetVariant  =
  sendMsg avAssetVariant (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @peakBitRate@
peakBitRateSelector :: Selector
peakBitRateSelector = mkSelector "peakBitRate"

-- | @Selector@ for @averageBitRate@
averageBitRateSelector :: Selector
averageBitRateSelector = mkSelector "averageBitRate"

-- | @Selector@ for @videoAttributes@
videoAttributesSelector :: Selector
videoAttributesSelector = mkSelector "videoAttributes"

-- | @Selector@ for @audioAttributes@
audioAttributesSelector :: Selector
audioAttributesSelector = mkSelector "audioAttributes"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

