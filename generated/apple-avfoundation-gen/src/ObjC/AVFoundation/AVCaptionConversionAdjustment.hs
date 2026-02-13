{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptionConversionAdjustment
--
-- Describes an adjustment that can be performed in order to correct a problem encountered during the validation of a caption conversion.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVCaptionConversionAdjustment@.
module ObjC.AVFoundation.AVCaptionConversionAdjustment
  ( AVCaptionConversionAdjustment
  , IsAVCaptionConversionAdjustment(..)
  , init_
  , new
  , adjustmentType
  , adjustmentTypeSelector
  , initSelector
  , newSelector


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
init_ :: IsAVCaptionConversionAdjustment avCaptionConversionAdjustment => avCaptionConversionAdjustment -> IO (Id AVCaptionConversionAdjustment)
init_ avCaptionConversionAdjustment =
  sendOwnedMessage avCaptionConversionAdjustment initSelector

-- | @+ new@
new :: IO (Id AVCaptionConversionAdjustment)
new  =
  do
    cls' <- getRequiredClass "AVCaptionConversionAdjustment"
    sendOwnedClassMessage cls' newSelector

-- | adjustmentType
--
-- Indicates the type of adjustment described by the receiver.
--
-- ObjC selector: @- adjustmentType@
adjustmentType :: IsAVCaptionConversionAdjustment avCaptionConversionAdjustment => avCaptionConversionAdjustment -> IO (Id NSString)
adjustmentType avCaptionConversionAdjustment =
  sendMessage avCaptionConversionAdjustment adjustmentTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptionConversionAdjustment)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptionConversionAdjustment)
newSelector = mkSelector "new"

-- | @Selector@ for @adjustmentType@
adjustmentTypeSelector :: Selector '[] (Id NSString)
adjustmentTypeSelector = mkSelector "adjustmentType"

