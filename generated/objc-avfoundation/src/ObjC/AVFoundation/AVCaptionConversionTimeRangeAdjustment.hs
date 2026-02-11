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
init_ :: IsAVCaptionConversionTimeRangeAdjustment avCaptionConversionTimeRangeAdjustment => avCaptionConversionTimeRangeAdjustment -> IO (Id AVCaptionConversionTimeRangeAdjustment)
init_ avCaptionConversionTimeRangeAdjustment  =
  sendMsg avCaptionConversionTimeRangeAdjustment (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptionConversionTimeRangeAdjustment)
new  =
  do
    cls' <- getRequiredClass "AVCaptionConversionTimeRangeAdjustment"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

