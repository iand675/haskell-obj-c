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
  , initSelector
  , newSelector
  , renditionSpecificAttributesForMediaOptionSelector
  , formatIDsSelector


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
init_ :: IsAVAssetVariantAudioAttributes avAssetVariantAudioAttributes => avAssetVariantAudioAttributes -> IO (Id AVAssetVariantAudioAttributes)
init_ avAssetVariantAudioAttributes  =
  sendMsg avAssetVariantAudioAttributes (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetVariantAudioAttributes)
new  =
  do
    cls' <- getRequiredClass "AVAssetVariantAudioAttributes"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Provides attributes for a specific audio media selection option. If no rendition specific attributes are declared, it will be nil.
--
-- - Parameter mediaSelectionOption: The option to return rendition specific information for.
--
-- ObjC selector: @- renditionSpecificAttributesForMediaOption:@
renditionSpecificAttributesForMediaOption :: (IsAVAssetVariantAudioAttributes avAssetVariantAudioAttributes, IsAVMediaSelectionOption mediaSelectionOption) => avAssetVariantAudioAttributes -> mediaSelectionOption -> IO (Id AVAssetVariantAudioRenditionSpecificAttributes)
renditionSpecificAttributesForMediaOption avAssetVariantAudioAttributes  mediaSelectionOption =
withObjCPtr mediaSelectionOption $ \raw_mediaSelectionOption ->
    sendMsg avAssetVariantAudioAttributes (mkSelector "renditionSpecificAttributesForMediaOption:") (retPtr retVoid) [argPtr (castPtr raw_mediaSelectionOption :: Ptr ())] >>= retainedObject . castPtr

-- | Provides an array of audio formats present in the variant's renditions if any are declared. Each value in the array is a NSNumber representation of AudioFormatID.
--
-- ObjC selector: @- formatIDs@
formatIDs :: IsAVAssetVariantAudioAttributes avAssetVariantAudioAttributes => avAssetVariantAudioAttributes -> IO (Id NSArray)
formatIDs avAssetVariantAudioAttributes  =
  sendMsg avAssetVariantAudioAttributes (mkSelector "formatIDs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @renditionSpecificAttributesForMediaOption:@
renditionSpecificAttributesForMediaOptionSelector :: Selector
renditionSpecificAttributesForMediaOptionSelector = mkSelector "renditionSpecificAttributesForMediaOption:"

-- | @Selector@ for @formatIDs@
formatIDsSelector :: Selector
formatIDsSelector = mkSelector "formatIDs"

