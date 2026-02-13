{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @QCCompositionPickerPanel@.
module ObjC.Quartz.QCCompositionPickerPanel
  ( QCCompositionPickerPanel
  , IsQCCompositionPickerPanel(..)
  , sharedCompositionPickerPanel
  , compositionPickerView
  , compositionPickerViewSelector
  , sharedCompositionPickerPanelSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' sharedCompositionPickerPanelSelector

-- | @- compositionPickerView@
compositionPickerView :: IsQCCompositionPickerPanel qcCompositionPickerPanel => qcCompositionPickerPanel -> IO (Id QCCompositionPickerView)
compositionPickerView qcCompositionPickerPanel =
  sendMessage qcCompositionPickerPanel compositionPickerViewSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedCompositionPickerPanel@
sharedCompositionPickerPanelSelector :: Selector '[] (Id QCCompositionPickerPanel)
sharedCompositionPickerPanelSelector = mkSelector "sharedCompositionPickerPanel"

-- | @Selector@ for @compositionPickerView@
compositionPickerViewSelector :: Selector '[] (Id QCCompositionPickerView)
compositionPickerViewSelector = mkSelector "compositionPickerView"

