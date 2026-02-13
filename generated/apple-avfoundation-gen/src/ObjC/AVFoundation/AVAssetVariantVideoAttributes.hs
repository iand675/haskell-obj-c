{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Video attributes for an asset variant.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVAssetVariantVideoAttributes@.
module ObjC.AVFoundation.AVAssetVariantVideoAttributes
  ( AVAssetVariantVideoAttributes
  , IsAVAssetVariantVideoAttributes(..)
  , init_
  , new
  , videoRange
  , codecTypes
  , nominalFrameRate
  , videoLayoutAttributes
  , codecTypesSelector
  , initSelector
  , newSelector
  , nominalFrameRateSelector
  , videoLayoutAttributesSelector
  , videoRangeSelector


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
init_ :: IsAVAssetVariantVideoAttributes avAssetVariantVideoAttributes => avAssetVariantVideoAttributes -> IO (Id AVAssetVariantVideoAttributes)
init_ avAssetVariantVideoAttributes =
  sendOwnedMessage avAssetVariantVideoAttributes initSelector

-- | @+ new@
new :: IO (Id AVAssetVariantVideoAttributes)
new  =
  do
    cls' <- getRequiredClass "AVAssetVariantVideoAttributes"
    sendOwnedClassMessage cls' newSelector

-- | Provides the video range of the variant. If it is not declared, it will be AVVideoRangeSDR.
--
-- ObjC selector: @- videoRange@
videoRange :: IsAVAssetVariantVideoAttributes avAssetVariantVideoAttributes => avAssetVariantVideoAttributes -> IO (Id NSString)
videoRange avAssetVariantVideoAttributes =
  sendMessage avAssetVariantVideoAttributes videoRangeSelector

-- | Provides an array of video sample codec types present in the variant's renditions if any are declared. Each value in the array is a NSNumber representation of CMVideoCodecType.
--
-- ObjC selector: @- codecTypes@
codecTypes :: IsAVAssetVariantVideoAttributes avAssetVariantVideoAttributes => avAssetVariantVideoAttributes -> IO (Id NSArray)
codecTypes avAssetVariantVideoAttributes =
  sendMessage avAssetVariantVideoAttributes codecTypesSelector

-- | If it is not declared, the value will be negative.
--
-- ObjC selector: @- nominalFrameRate@
nominalFrameRate :: IsAVAssetVariantVideoAttributes avAssetVariantVideoAttributes => avAssetVariantVideoAttributes -> IO CDouble
nominalFrameRate avAssetVariantVideoAttributes =
  sendMessage avAssetVariantVideoAttributes nominalFrameRateSelector

-- | Describes the video layout attributes.
--
-- videoLayoutAttributes' count may be greater than one if this variant contains a collection of differing video layout media attributes over time.
--
-- ObjC selector: @- videoLayoutAttributes@
videoLayoutAttributes :: IsAVAssetVariantVideoAttributes avAssetVariantVideoAttributes => avAssetVariantVideoAttributes -> IO (Id NSArray)
videoLayoutAttributes avAssetVariantVideoAttributes =
  sendMessage avAssetVariantVideoAttributes videoLayoutAttributesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetVariantVideoAttributes)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetVariantVideoAttributes)
newSelector = mkSelector "new"

-- | @Selector@ for @videoRange@
videoRangeSelector :: Selector '[] (Id NSString)
videoRangeSelector = mkSelector "videoRange"

-- | @Selector@ for @codecTypes@
codecTypesSelector :: Selector '[] (Id NSArray)
codecTypesSelector = mkSelector "codecTypes"

-- | @Selector@ for @nominalFrameRate@
nominalFrameRateSelector :: Selector '[] CDouble
nominalFrameRateSelector = mkSelector "nominalFrameRate"

-- | @Selector@ for @videoLayoutAttributes@
videoLayoutAttributesSelector :: Selector '[] (Id NSArray)
videoLayoutAttributesSelector = mkSelector "videoLayoutAttributes"

