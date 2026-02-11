{-# LANGUAGE PatternSynonyms #-}
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
  , selectionLimitSelector
  , setSelectionLimitSelector
  , edgesWithoutContentMarginsSelector
  , setEdgesWithoutContentMarginsSelector

  -- * Enum types
  , NSDirectionalRectEdge(NSDirectionalRectEdge)
  , pattern NSDirectionalRectEdgeNone
  , pattern NSDirectionalRectEdgeTop
  , pattern NSDirectionalRectEdgeLeading
  , pattern NSDirectionalRectEdgeBottom
  , pattern NSDirectionalRectEdgeTrailing
  , pattern NSDirectionalRectEdgeAll

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

import ObjC.PhotosUI.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The maximum number of assets that can be selected.
--
-- ObjC selector: @- selectionLimit@
selectionLimit :: IsPHPickerUpdateConfiguration phPickerUpdateConfiguration => phPickerUpdateConfiguration -> IO CLong
selectionLimit phPickerUpdateConfiguration  =
  sendMsg phPickerUpdateConfiguration (mkSelector "selectionLimit") retCLong []

-- | The maximum number of assets that can be selected.
--
-- ObjC selector: @- setSelectionLimit:@
setSelectionLimit :: IsPHPickerUpdateConfiguration phPickerUpdateConfiguration => phPickerUpdateConfiguration -> CLong -> IO ()
setSelectionLimit phPickerUpdateConfiguration  value =
  sendMsg phPickerUpdateConfiguration (mkSelector "setSelectionLimit:") retVoid [argCLong (fromIntegral value)]

-- | Edges of the picker that have no margin between the content and the edge (e.g. without bars in between).
--
-- ObjC selector: @- edgesWithoutContentMargins@
edgesWithoutContentMargins :: IsPHPickerUpdateConfiguration phPickerUpdateConfiguration => phPickerUpdateConfiguration -> IO NSDirectionalRectEdge
edgesWithoutContentMargins phPickerUpdateConfiguration  =
  fmap (coerce :: CULong -> NSDirectionalRectEdge) $ sendMsg phPickerUpdateConfiguration (mkSelector "edgesWithoutContentMargins") retCULong []

-- | Edges of the picker that have no margin between the content and the edge (e.g. without bars in between).
--
-- ObjC selector: @- setEdgesWithoutContentMargins:@
setEdgesWithoutContentMargins :: IsPHPickerUpdateConfiguration phPickerUpdateConfiguration => phPickerUpdateConfiguration -> NSDirectionalRectEdge -> IO ()
setEdgesWithoutContentMargins phPickerUpdateConfiguration  value =
  sendMsg phPickerUpdateConfiguration (mkSelector "setEdgesWithoutContentMargins:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @selectionLimit@
selectionLimitSelector :: Selector
selectionLimitSelector = mkSelector "selectionLimit"

-- | @Selector@ for @setSelectionLimit:@
setSelectionLimitSelector :: Selector
setSelectionLimitSelector = mkSelector "setSelectionLimit:"

-- | @Selector@ for @edgesWithoutContentMargins@
edgesWithoutContentMarginsSelector :: Selector
edgesWithoutContentMarginsSelector = mkSelector "edgesWithoutContentMargins"

-- | @Selector@ for @setEdgesWithoutContentMargins:@
setEdgesWithoutContentMarginsSelector :: Selector
setEdgesWithoutContentMarginsSelector = mkSelector "setEdgesWithoutContentMargins:"

