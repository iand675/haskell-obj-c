{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Audio attributes for an asset variant.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVAssetVariantAudioAttributes@.
module ObjC.AVFoundation.AVAssetVariantAudioAttributes
  ( AVAssetVariantAudioAttributes
  , IsAVAssetVariantAudioAttributes(..)
  , init_
  , new
  , renditionSpecificAttributesForMediaOption
  , formatIDs
  , formatIDsSelector
  , initSelector
  , newSelector
  , renditionSpecificAttributesForMediaOptionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetVariantAudioAttributes avAssetVariantAudioAttributes => avAssetVariantAudioAttributes -> IO (Id AVAssetVariantAudioAttributes)
init_ avAssetVariantAudioAttributes =
  sendOwnedMessage avAssetVariantAudioAttributes initSelector

-- | @+ new@
new :: IO (Id AVAssetVariantAudioAttributes)
new  =
  do
    cls' <- getRequiredClass "AVAssetVariantAudioAttributes"
    sendOwnedClassMessage cls' newSelector

-- | Provides attributes for a specific audio media selection option. If no rendition specific attributes are declared, it will be nil.
--
-- - Parameter mediaSelectionOption: The option to return rendition specific information for.
--
-- ObjC selector: @- renditionSpecificAttributesForMediaOption:@
renditionSpecificAttributesForMediaOption :: (IsAVAssetVariantAudioAttributes avAssetVariantAudioAttributes, IsAVMediaSelectionOption mediaSelectionOption) => avAssetVariantAudioAttributes -> mediaSelectionOption -> IO (Id AVAssetVariantAudioRenditionSpecificAttributes)
renditionSpecificAttributesForMediaOption avAssetVariantAudioAttributes mediaSelectionOption =
  sendMessage avAssetVariantAudioAttributes renditionSpecificAttributesForMediaOptionSelector (toAVMediaSelectionOption mediaSelectionOption)

-- | Provides an array of audio formats present in the variant's renditions if any are declared. Each value in the array is a NSNumber representation of AudioFormatID.
--
-- ObjC selector: @- formatIDs@
formatIDs :: IsAVAssetVariantAudioAttributes avAssetVariantAudioAttributes => avAssetVariantAudioAttributes -> IO (Id NSArray)
formatIDs avAssetVariantAudioAttributes =
  sendMessage avAssetVariantAudioAttributes formatIDsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetVariantAudioAttributes)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetVariantAudioAttributes)
newSelector = mkSelector "new"

-- | @Selector@ for @renditionSpecificAttributesForMediaOption:@
renditionSpecificAttributesForMediaOptionSelector :: Selector '[Id AVMediaSelectionOption] (Id AVAssetVariantAudioRenditionSpecificAttributes)
renditionSpecificAttributesForMediaOptionSelector = mkSelector "renditionSpecificAttributesForMediaOption:"

-- | @Selector@ for @formatIDs@
formatIDsSelector :: Selector '[] (Id NSArray)
formatIDsSelector = mkSelector "formatIDs"

