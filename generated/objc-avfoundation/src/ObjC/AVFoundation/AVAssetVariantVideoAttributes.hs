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
  , initSelector
  , newSelector
  , videoRangeSelector
  , codecTypesSelector
  , nominalFrameRateSelector
  , videoLayoutAttributesSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetVariantVideoAttributes avAssetVariantVideoAttributes => avAssetVariantVideoAttributes -> IO (Id AVAssetVariantVideoAttributes)
init_ avAssetVariantVideoAttributes  =
  sendMsg avAssetVariantVideoAttributes (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetVariantVideoAttributes)
new  =
  do
    cls' <- getRequiredClass "AVAssetVariantVideoAttributes"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Provides the video range of the variant. If it is not declared, it will be AVVideoRangeSDR.
--
-- ObjC selector: @- videoRange@
videoRange :: IsAVAssetVariantVideoAttributes avAssetVariantVideoAttributes => avAssetVariantVideoAttributes -> IO (Id NSString)
videoRange avAssetVariantVideoAttributes  =
  sendMsg avAssetVariantVideoAttributes (mkSelector "videoRange") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides an array of video sample codec types present in the variant's renditions if any are declared. Each value in the array is a NSNumber representation of CMVideoCodecType.
--
-- ObjC selector: @- codecTypes@
codecTypes :: IsAVAssetVariantVideoAttributes avAssetVariantVideoAttributes => avAssetVariantVideoAttributes -> IO (Id NSArray)
codecTypes avAssetVariantVideoAttributes  =
  sendMsg avAssetVariantVideoAttributes (mkSelector "codecTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If it is not declared, the value will be negative.
--
-- ObjC selector: @- nominalFrameRate@
nominalFrameRate :: IsAVAssetVariantVideoAttributes avAssetVariantVideoAttributes => avAssetVariantVideoAttributes -> IO CDouble
nominalFrameRate avAssetVariantVideoAttributes  =
  sendMsg avAssetVariantVideoAttributes (mkSelector "nominalFrameRate") retCDouble []

-- | Describes the video layout attributes.
--
-- videoLayoutAttributes' count may be greater than one if this variant contains a collection of differing video layout media attributes over time.
--
-- ObjC selector: @- videoLayoutAttributes@
videoLayoutAttributes :: IsAVAssetVariantVideoAttributes avAssetVariantVideoAttributes => avAssetVariantVideoAttributes -> IO (Id NSArray)
videoLayoutAttributes avAssetVariantVideoAttributes  =
  sendMsg avAssetVariantVideoAttributes (mkSelector "videoLayoutAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @videoRange@
videoRangeSelector :: Selector
videoRangeSelector = mkSelector "videoRange"

-- | @Selector@ for @codecTypes@
codecTypesSelector :: Selector
codecTypesSelector = mkSelector "codecTypes"

-- | @Selector@ for @nominalFrameRate@
nominalFrameRateSelector :: Selector
nominalFrameRateSelector = mkSelector "nominalFrameRate"

-- | @Selector@ for @videoLayoutAttributes@
videoLayoutAttributesSelector :: Selector
videoLayoutAttributesSelector = mkSelector "videoLayoutAttributes"

