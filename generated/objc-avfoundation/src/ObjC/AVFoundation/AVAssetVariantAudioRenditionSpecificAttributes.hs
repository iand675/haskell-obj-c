{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Audio rendition attributes for an asset variant.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVAssetVariantAudioRenditionSpecificAttributes@.
module ObjC.AVFoundation.AVAssetVariantAudioRenditionSpecificAttributes
  ( AVAssetVariantAudioRenditionSpecificAttributes
  , IsAVAssetVariantAudioRenditionSpecificAttributes(..)
  , channelCount
  , binaural
  , immersive
  , downmix
  , channelCountSelector
  , binauralSelector
  , immersiveSelector
  , downmixSelector


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

-- | If it is not declared, the value will be negative.
--
-- A channel count greater than two indicates that the variant offers a rich multichannel authoring.
--
-- ObjC selector: @- channelCount@
channelCount :: IsAVAssetVariantAudioRenditionSpecificAttributes avAssetVariantAudioRenditionSpecificAttributes => avAssetVariantAudioRenditionSpecificAttributes -> IO CLong
channelCount avAssetVariantAudioRenditionSpecificAttributes  =
  sendMsg avAssetVariantAudioRenditionSpecificAttributes (mkSelector "channelCount") retCLong []

-- | Indicates that the variant is best suited for delivery to headphones.
--
-- A binaural variant may originate from a direct binaural recording or from the processing of a multichannel audio source.
--
-- ObjC selector: @- binaural@
binaural :: IsAVAssetVariantAudioRenditionSpecificAttributes avAssetVariantAudioRenditionSpecificAttributes => avAssetVariantAudioRenditionSpecificAttributes -> IO Bool
binaural avAssetVariantAudioRenditionSpecificAttributes  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetVariantAudioRenditionSpecificAttributes (mkSelector "binaural") retCULong []

-- | Indicates that this variant contains virtualized or otherwise pre-processed audio content that is suitable for a variety of purposes.
--
-- If a variant audio redition is immersive it is eligible for rendering either to headphones or speakers.
--
-- ObjC selector: @- immersive@
immersive :: IsAVAssetVariantAudioRenditionSpecificAttributes avAssetVariantAudioRenditionSpecificAttributes => avAssetVariantAudioRenditionSpecificAttributes -> IO Bool
immersive avAssetVariantAudioRenditionSpecificAttributes  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetVariantAudioRenditionSpecificAttributes (mkSelector "immersive") retCULong []

-- | Indicates that this variant is declared as a downmix derivative of other media of greater channel count.
--
-- If one or more multichannel variants are also provided, the dowmix is assumed to be compatible in its internal timing and other attributes with those variants. Typically this is because it has been derived from the same source. A downmix can be used as a suitable substitute for a multichannel variant under some conditions.
--
-- ObjC selector: @- downmix@
downmix :: IsAVAssetVariantAudioRenditionSpecificAttributes avAssetVariantAudioRenditionSpecificAttributes => avAssetVariantAudioRenditionSpecificAttributes -> IO Bool
downmix avAssetVariantAudioRenditionSpecificAttributes  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetVariantAudioRenditionSpecificAttributes (mkSelector "downmix") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @channelCount@
channelCountSelector :: Selector
channelCountSelector = mkSelector "channelCount"

-- | @Selector@ for @binaural@
binauralSelector :: Selector
binauralSelector = mkSelector "binaural"

-- | @Selector@ for @immersive@
immersiveSelector :: Selector
immersiveSelector = mkSelector "immersive"

-- | @Selector@ for @downmix@
downmixSelector :: Selector
downmixSelector = mkSelector "downmix"

