{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptionConversionTimeRangeAdjustment
--
-- Describes an adjustment to the timeRange of one or more captions.
--
-- Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVCaptionConversionTimeRangeAdjustment@.
module ObjC.AVFoundation.AVCaptionConversionTimeRangeAdjustment
  ( AVCaptionConversionTimeRangeAdjustment
  , IsAVCaptionConversionTimeRangeAdjustment(..)
  , init_
  , new
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
init_ :: IsAVCaptionConversionTimeRangeAdjustment avCaptionConversionTimeRangeAdjustment => avCaptionConversionTimeRangeAdjustment -> IO (Id AVCaptionConversionTimeRangeAdjustment)
init_ avCaptionConversionTimeRangeAdjustment =
  sendOwnedMessage avCaptionConversionTimeRangeAdjustment initSelector

-- | @+ new@
new :: IO (Id AVCaptionConversionTimeRangeAdjustment)
new  =
  do
    cls' <- getRequiredClass "AVCaptionConversionTimeRangeAdjustment"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptionConversionTimeRangeAdjustment)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptionConversionTimeRangeAdjustment)
newSelector = mkSelector "new"

