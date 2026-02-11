{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @QCCompositionPickerPanel@.
module ObjC.Quartz.QCCompositionPickerPanel
  ( QCCompositionPickerPanel
  , IsQCCompositionPickerPanel(..)
  , sharedCompositionPickerPanel
  , compositionPickerView
  , sharedCompositionPickerPanelSelector
  , compositionPickerViewSelector


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

import ObjC.Quartz.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sharedCompositionPickerPanel@
sharedCompositionPickerPanel :: IO (Id QCCompositionPickerPanel)
sharedCompositionPickerPanel  =
  do
    cls' <- getRequiredClass "QCCompositionPickerPanel"
    sendClassMsg cls' (mkSelector "sharedCompositionPickerPanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- compositionPickerView@
compositionPickerView :: IsQCCompositionPickerPanel qcCompositionPickerPanel => qcCompositionPickerPanel -> IO (Id QCCompositionPickerView)
compositionPickerView qcCompositionPickerPanel  =
  sendMsg qcCompositionPickerPanel (mkSelector "compositionPickerView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedCompositionPickerPanel@
sharedCompositionPickerPanelSelector :: Selector
sharedCompositionPickerPanelSelector = mkSelector "sharedCompositionPickerPanel"

-- | @Selector@ for @compositionPickerView@
compositionPickerViewSelector :: Selector
compositionPickerViewSelector = mkSelector "compositionPickerView"

