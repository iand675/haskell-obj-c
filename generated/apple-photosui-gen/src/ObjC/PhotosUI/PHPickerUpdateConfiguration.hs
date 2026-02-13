{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An update configuration for @PHPickerViewController.@
--
-- Generated bindings for @PHPickerUpdateConfiguration@.
module ObjC.PhotosUI.PHPickerUpdateConfiguration
  ( PHPickerUpdateConfiguration
  , IsPHPickerUpdateConfiguration(..)
  , selectionLimit
  , setSelectionLimit
  , edgesWithoutContentMargins
  , setEdgesWithoutContentMargins
  , edgesWithoutContentMarginsSelector
  , selectionLimitSelector
  , setEdgesWithoutContentMarginsSelector
  , setSelectionLimitSelector

  -- * Enum types
  , NSDirectionalRectEdge(NSDirectionalRectEdge)
  , pattern NSDirectionalRectEdgeNone
  , pattern NSDirectionalRectEdgeTop
  , pattern NSDirectionalRectEdgeLeading
  , pattern NSDirectionalRectEdgeBottom
  , pattern NSDirectionalRectEdgeTrailing
  , pattern NSDirectionalRectEdgeAll

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PhotosUI.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The maximum number of assets that can be selected.
--
-- ObjC selector: @- selectionLimit@
selectionLimit :: IsPHPickerUpdateConfiguration phPickerUpdateConfiguration => phPickerUpdateConfiguration -> IO CLong
selectionLimit phPickerUpdateConfiguration =
  sendMessage phPickerUpdateConfiguration selectionLimitSelector

-- | The maximum number of assets that can be selected.
--
-- ObjC selector: @- setSelectionLimit:@
setSelectionLimit :: IsPHPickerUpdateConfiguration phPickerUpdateConfiguration => phPickerUpdateConfiguration -> CLong -> IO ()
setSelectionLimit phPickerUpdateConfiguration value =
  sendMessage phPickerUpdateConfiguration setSelectionLimitSelector value

-- | Edges of the picker that have no margin between the content and the edge (e.g. without bars in between).
--
-- ObjC selector: @- edgesWithoutContentMargins@
edgesWithoutContentMargins :: IsPHPickerUpdateConfiguration phPickerUpdateConfiguration => phPickerUpdateConfiguration -> IO NSDirectionalRectEdge
edgesWithoutContentMargins phPickerUpdateConfiguration =
  sendMessage phPickerUpdateConfiguration edgesWithoutContentMarginsSelector

-- | Edges of the picker that have no margin between the content and the edge (e.g. without bars in between).
--
-- ObjC selector: @- setEdgesWithoutContentMargins:@
setEdgesWithoutContentMargins :: IsPHPickerUpdateConfiguration phPickerUpdateConfiguration => phPickerUpdateConfiguration -> NSDirectionalRectEdge -> IO ()
setEdgesWithoutContentMargins phPickerUpdateConfiguration value =
  sendMessage phPickerUpdateConfiguration setEdgesWithoutContentMarginsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectionLimit@
selectionLimitSelector :: Selector '[] CLong
selectionLimitSelector = mkSelector "selectionLimit"

-- | @Selector@ for @setSelectionLimit:@
setSelectionLimitSelector :: Selector '[CLong] ()
setSelectionLimitSelector = mkSelector "setSelectionLimit:"

-- | @Selector@ for @edgesWithoutContentMargins@
edgesWithoutContentMarginsSelector :: Selector '[] NSDirectionalRectEdge
edgesWithoutContentMarginsSelector = mkSelector "edgesWithoutContentMargins"

-- | @Selector@ for @setEdgesWithoutContentMargins:@
setEdgesWithoutContentMarginsSelector :: Selector '[NSDirectionalRectEdge] ()
setEdgesWithoutContentMarginsSelector = mkSelector "setEdgesWithoutContentMargins:"

