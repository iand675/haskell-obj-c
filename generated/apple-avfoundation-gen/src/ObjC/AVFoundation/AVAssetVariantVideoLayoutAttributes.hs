{-# LANGUAGE DataKinds #-}
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
  , projectionTypeSelector
  , stereoViewComponentsSelector


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
init_ :: IsAVAssetVariantVideoLayoutAttributes avAssetVariantVideoLayoutAttributes => avAssetVariantVideoLayoutAttributes -> IO (Id AVAssetVariantVideoLayoutAttributes)
init_ avAssetVariantVideoLayoutAttributes =
  sendOwnedMessage avAssetVariantVideoLayoutAttributes initSelector

-- | @+ new@
new :: IO (Id AVAssetVariantVideoLayoutAttributes)
new  =
  do
    cls' <- getRequiredClass "AVAssetVariantVideoLayoutAttributes"
    sendOwnedClassMessage cls' newSelector

-- | Describes the stereo components. If not declared, the value will be @kCMStereoViewComponent_None@. In case of monoscopic content, the value will be @kCMStereoViewComponent_None@ and incase of stereoscopic content, the value will be @(kCMStereoViewComponent_LeftEye | kCMStereoViewComponent_RightEye)@.
--
-- ObjC selector: @- stereoViewComponents@
stereoViewComponents :: IsAVAssetVariantVideoLayoutAttributes avAssetVariantVideoLayoutAttributes => avAssetVariantVideoLayoutAttributes -> IO CInt
stereoViewComponents avAssetVariantVideoLayoutAttributes =
  sendMessage avAssetVariantVideoLayoutAttributes stereoViewComponentsSelector

-- | Describes the video projection.
--
-- ObjC selector: @- projectionType@
projectionType :: IsAVAssetVariantVideoLayoutAttributes avAssetVariantVideoLayoutAttributes => avAssetVariantVideoLayoutAttributes -> IO CInt
projectionType avAssetVariantVideoLayoutAttributes =
  sendMessage avAssetVariantVideoLayoutAttributes projectionTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetVariantVideoLayoutAttributes)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetVariantVideoLayoutAttributes)
newSelector = mkSelector "new"

-- | @Selector@ for @stereoViewComponents@
stereoViewComponentsSelector :: Selector '[] CInt
stereoViewComponentsSelector = mkSelector "stereoViewComponents"

-- | @Selector@ for @projectionType@
projectionTypeSelector :: Selector '[] CInt
projectionTypeSelector = mkSelector "projectionType"

