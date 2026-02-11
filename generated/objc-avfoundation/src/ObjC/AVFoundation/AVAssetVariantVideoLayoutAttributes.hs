{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVAssetVariantVideoLayoutAttributes@.
module ObjC.AVFoundation.AVAssetVariantVideoLayoutAttributes
  ( AVAssetVariantVideoLayoutAttributes
  , IsAVAssetVariantVideoLayoutAttributes(..)
  , init_
  , new
  , stereoViewComponents
  , projectionType
  , initSelector
  , newSelector
  , stereoViewComponentsSelector
  , projectionTypeSelector


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
init_ :: IsAVAssetVariantVideoLayoutAttributes avAssetVariantVideoLayoutAttributes => avAssetVariantVideoLayoutAttributes -> IO (Id AVAssetVariantVideoLayoutAttributes)
init_ avAssetVariantVideoLayoutAttributes  =
  sendMsg avAssetVariantVideoLayoutAttributes (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetVariantVideoLayoutAttributes)
new  =
  do
    cls' <- getRequiredClass "AVAssetVariantVideoLayoutAttributes"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Describes the stereo components. If not declared, the value will be @kCMStereoViewComponent_None@. In case of monoscopic content, the value will be @kCMStereoViewComponent_None@ and incase of stereoscopic content, the value will be @(kCMStereoViewComponent_LeftEye | kCMStereoViewComponent_RightEye)@.
--
-- ObjC selector: @- stereoViewComponents@
stereoViewComponents :: IsAVAssetVariantVideoLayoutAttributes avAssetVariantVideoLayoutAttributes => avAssetVariantVideoLayoutAttributes -> IO CInt
stereoViewComponents avAssetVariantVideoLayoutAttributes  =
  sendMsg avAssetVariantVideoLayoutAttributes (mkSelector "stereoViewComponents") retCInt []

-- | Describes the video projection.
--
-- ObjC selector: @- projectionType@
projectionType :: IsAVAssetVariantVideoLayoutAttributes avAssetVariantVideoLayoutAttributes => avAssetVariantVideoLayoutAttributes -> IO CInt
projectionType avAssetVariantVideoLayoutAttributes  =
  sendMsg avAssetVariantVideoLayoutAttributes (mkSelector "projectionType") retCInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @stereoViewComponents@
stereoViewComponentsSelector :: Selector
stereoViewComponentsSelector = mkSelector "stereoViewComponents"

-- | @Selector@ for @projectionType@
projectionTypeSelector :: Selector
projectionTypeSelector = mkSelector "projectionType"

