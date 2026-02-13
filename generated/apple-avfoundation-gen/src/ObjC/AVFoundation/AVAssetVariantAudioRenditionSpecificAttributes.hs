{-# LANGUAGE DataKinds #-}
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
  , binauralSelector
  , channelCountSelector
  , downmixSelector
  , immersiveSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
channelCount avAssetVariantAudioRenditionSpecificAttributes =
  sendMessage avAssetVariantAudioRenditionSpecificAttributes channelCountSelector

-- | Indicates that the variant is best suited for delivery to headphones.
--
-- A binaural variant may originate from a direct binaural recording or from the processing of a multichannel audio source.
--
-- ObjC selector: @- binaural@
binaural :: IsAVAssetVariantAudioRenditionSpecificAttributes avAssetVariantAudioRenditionSpecificAttributes => avAssetVariantAudioRenditionSpecificAttributes -> IO Bool
binaural avAssetVariantAudioRenditionSpecificAttributes =
  sendMessage avAssetVariantAudioRenditionSpecificAttributes binauralSelector

-- | Indicates that this variant contains virtualized or otherwise pre-processed audio content that is suitable for a variety of purposes.
--
-- If a variant audio redition is immersive it is eligible for rendering either to headphones or speakers.
--
-- ObjC selector: @- immersive@
immersive :: IsAVAssetVariantAudioRenditionSpecificAttributes avAssetVariantAudioRenditionSpecificAttributes => avAssetVariantAudioRenditionSpecificAttributes -> IO Bool
immersive avAssetVariantAudioRenditionSpecificAttributes =
  sendMessage avAssetVariantAudioRenditionSpecificAttributes immersiveSelector

-- | Indicates that this variant is declared as a downmix derivative of other media of greater channel count.
--
-- If one or more multichannel variants are also provided, the dowmix is assumed to be compatible in its internal timing and other attributes with those variants. Typically this is because it has been derived from the same source. A downmix can be used as a suitable substitute for a multichannel variant under some conditions.
--
-- ObjC selector: @- downmix@
downmix :: IsAVAssetVariantAudioRenditionSpecificAttributes avAssetVariantAudioRenditionSpecificAttributes => avAssetVariantAudioRenditionSpecificAttributes -> IO Bool
downmix avAssetVariantAudioRenditionSpecificAttributes =
  sendMessage avAssetVariantAudioRenditionSpecificAttributes downmixSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @channelCount@
channelCountSelector :: Selector '[] CLong
channelCountSelector = mkSelector "channelCount"

-- | @Selector@ for @binaural@
binauralSelector :: Selector '[] Bool
binauralSelector = mkSelector "binaural"

-- | @Selector@ for @immersive@
immersiveSelector :: Selector '[] Bool
immersiveSelector = mkSelector "immersive"

-- | @Selector@ for @downmix@
downmixSelector :: Selector '[] Bool
downmixSelector = mkSelector "downmix"

