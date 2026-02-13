{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents the configuration consisting of variant and the variant's media options.
--
-- Generated bindings for @AVAssetDownloadContentConfiguration@.
module ObjC.AVFoundation.AVAssetDownloadContentConfiguration
  ( AVAssetDownloadContentConfiguration
  , IsAVAssetDownloadContentConfiguration(..)
  , variantQualifiers
  , setVariantQualifiers
  , mediaSelections
  , setMediaSelections
  , mediaSelectionsSelector
  , setMediaSelectionsSelector
  , setVariantQualifiersSelector
  , variantQualifiersSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | An array of variant qualifiers.
--
-- The qualifiers are expected to be added in the preferential order and will be evaluated in that order until the qualifier matches one or more AVAssetVariants. Only those variants which can be played on the current device configuration will be initially chosen for evaluation. If there is more than one match, automatic variant selection will be used to choose among the matched. If a variant qualifier is constructed to explicitly choose a variant, no evaluation is performed and the variant provided will be downloaded as is, even if it is not playable on current device configuration. If a variant qualifier has not been provided, or if the variant qualifier when evaluated does not match any of the variants which can be played according to the current device configuration, automatic variant selection will be used.
--
-- ObjC selector: @- variantQualifiers@
variantQualifiers :: IsAVAssetDownloadContentConfiguration avAssetDownloadContentConfiguration => avAssetDownloadContentConfiguration -> IO (Id NSArray)
variantQualifiers avAssetDownloadContentConfiguration =
  sendMessage avAssetDownloadContentConfiguration variantQualifiersSelector

-- | An array of variant qualifiers.
--
-- The qualifiers are expected to be added in the preferential order and will be evaluated in that order until the qualifier matches one or more AVAssetVariants. Only those variants which can be played on the current device configuration will be initially chosen for evaluation. If there is more than one match, automatic variant selection will be used to choose among the matched. If a variant qualifier is constructed to explicitly choose a variant, no evaluation is performed and the variant provided will be downloaded as is, even if it is not playable on current device configuration. If a variant qualifier has not been provided, or if the variant qualifier when evaluated does not match any of the variants which can be played according to the current device configuration, automatic variant selection will be used.
--
-- ObjC selector: @- setVariantQualifiers:@
setVariantQualifiers :: (IsAVAssetDownloadContentConfiguration avAssetDownloadContentConfiguration, IsNSArray value) => avAssetDownloadContentConfiguration -> value -> IO ()
setVariantQualifiers avAssetDownloadContentConfiguration value =
  sendMessage avAssetDownloadContentConfiguration setVariantQualifiersSelector (toNSArray value)

-- | An array of media selections obtained from the AVAsset.
--
-- If a media selection is not provided, automatic media selection associated with the asset will be used.
--
-- ObjC selector: @- mediaSelections@
mediaSelections :: IsAVAssetDownloadContentConfiguration avAssetDownloadContentConfiguration => avAssetDownloadContentConfiguration -> IO (Id NSArray)
mediaSelections avAssetDownloadContentConfiguration =
  sendMessage avAssetDownloadContentConfiguration mediaSelectionsSelector

-- | An array of media selections obtained from the AVAsset.
--
-- If a media selection is not provided, automatic media selection associated with the asset will be used.
--
-- ObjC selector: @- setMediaSelections:@
setMediaSelections :: (IsAVAssetDownloadContentConfiguration avAssetDownloadContentConfiguration, IsNSArray value) => avAssetDownloadContentConfiguration -> value -> IO ()
setMediaSelections avAssetDownloadContentConfiguration value =
  sendMessage avAssetDownloadContentConfiguration setMediaSelectionsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @variantQualifiers@
variantQualifiersSelector :: Selector '[] (Id NSArray)
variantQualifiersSelector = mkSelector "variantQualifiers"

-- | @Selector@ for @setVariantQualifiers:@
setVariantQualifiersSelector :: Selector '[Id NSArray] ()
setVariantQualifiersSelector = mkSelector "setVariantQualifiers:"

-- | @Selector@ for @mediaSelections@
mediaSelectionsSelector :: Selector '[] (Id NSArray)
mediaSelectionsSelector = mkSelector "mediaSelections"

-- | @Selector@ for @setMediaSelections:@
setMediaSelectionsSelector :: Selector '[Id NSArray] ()
setMediaSelectionsSelector = mkSelector "setMediaSelections:"

