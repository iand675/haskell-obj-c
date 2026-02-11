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
  , variantQualifiersSelector
  , setVariantQualifiersSelector
  , mediaSelectionsSelector
  , setMediaSelectionsSelector


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

-- | An array of variant qualifiers.
--
-- The qualifiers are expected to be added in the preferential order and will be evaluated in that order until the qualifier matches one or more AVAssetVariants. Only those variants which can be played on the current device configuration will be initially chosen for evaluation. If there is more than one match, automatic variant selection will be used to choose among the matched. If a variant qualifier is constructed to explicitly choose a variant, no evaluation is performed and the variant provided will be downloaded as is, even if it is not playable on current device configuration. If a variant qualifier has not been provided, or if the variant qualifier when evaluated does not match any of the variants which can be played according to the current device configuration, automatic variant selection will be used.
--
-- ObjC selector: @- variantQualifiers@
variantQualifiers :: IsAVAssetDownloadContentConfiguration avAssetDownloadContentConfiguration => avAssetDownloadContentConfiguration -> IO (Id NSArray)
variantQualifiers avAssetDownloadContentConfiguration  =
  sendMsg avAssetDownloadContentConfiguration (mkSelector "variantQualifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An array of variant qualifiers.
--
-- The qualifiers are expected to be added in the preferential order and will be evaluated in that order until the qualifier matches one or more AVAssetVariants. Only those variants which can be played on the current device configuration will be initially chosen for evaluation. If there is more than one match, automatic variant selection will be used to choose among the matched. If a variant qualifier is constructed to explicitly choose a variant, no evaluation is performed and the variant provided will be downloaded as is, even if it is not playable on current device configuration. If a variant qualifier has not been provided, or if the variant qualifier when evaluated does not match any of the variants which can be played according to the current device configuration, automatic variant selection will be used.
--
-- ObjC selector: @- setVariantQualifiers:@
setVariantQualifiers :: (IsAVAssetDownloadContentConfiguration avAssetDownloadContentConfiguration, IsNSArray value) => avAssetDownloadContentConfiguration -> value -> IO ()
setVariantQualifiers avAssetDownloadContentConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg avAssetDownloadContentConfiguration (mkSelector "setVariantQualifiers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | An array of media selections obtained from the AVAsset.
--
-- If a media selection is not provided, automatic media selection associated with the asset will be used.
--
-- ObjC selector: @- mediaSelections@
mediaSelections :: IsAVAssetDownloadContentConfiguration avAssetDownloadContentConfiguration => avAssetDownloadContentConfiguration -> IO (Id NSArray)
mediaSelections avAssetDownloadContentConfiguration  =
  sendMsg avAssetDownloadContentConfiguration (mkSelector "mediaSelections") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An array of media selections obtained from the AVAsset.
--
-- If a media selection is not provided, automatic media selection associated with the asset will be used.
--
-- ObjC selector: @- setMediaSelections:@
setMediaSelections :: (IsAVAssetDownloadContentConfiguration avAssetDownloadContentConfiguration, IsNSArray value) => avAssetDownloadContentConfiguration -> value -> IO ()
setMediaSelections avAssetDownloadContentConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg avAssetDownloadContentConfiguration (mkSelector "setMediaSelections:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @variantQualifiers@
variantQualifiersSelector :: Selector
variantQualifiersSelector = mkSelector "variantQualifiers"

-- | @Selector@ for @setVariantQualifiers:@
setVariantQualifiersSelector :: Selector
setVariantQualifiersSelector = mkSelector "setVariantQualifiers:"

-- | @Selector@ for @mediaSelections@
mediaSelectionsSelector :: Selector
mediaSelectionsSelector = mkSelector "mediaSelections"

-- | @Selector@ for @setMediaSelections:@
setMediaSelectionsSelector :: Selector
setMediaSelectionsSelector = mkSelector "setMediaSelections:"

