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
  , initSelector
  , newSelector
  , adjustmentTypeSelector


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
init_ :: IsAVCaptionConversionAdjustment avCaptionConversionAdjustment => avCaptionConversionAdjustment -> IO (Id AVCaptionConversionAdjustment)
init_ avCaptionConversionAdjustment  =
  sendMsg avCaptionConversionAdjustment (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptionConversionAdjustment)
new  =
  do
    cls' <- getRequiredClass "AVCaptionConversionAdjustment"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | adjustmentType
--
-- Indicates the type of adjustment described by the receiver.
--
-- ObjC selector: @- adjustmentType@
adjustmentType :: IsAVCaptionConversionAdjustment avCaptionConversionAdjustment => avCaptionConversionAdjustment -> IO (Id NSString)
adjustmentType avCaptionConversionAdjustment  =
  sendMsg avCaptionConversionAdjustment (mkSelector "adjustmentType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @adjustmentType@
adjustmentTypeSelector :: Selector
adjustmentTypeSelector = mkSelector "adjustmentType"

