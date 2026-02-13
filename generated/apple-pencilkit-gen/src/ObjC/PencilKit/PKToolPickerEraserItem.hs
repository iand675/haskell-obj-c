{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A user interface for an eraser tool item in PKToolPicker.
--
-- Generated bindings for @PKToolPickerEraserItem@.
module ObjC.PencilKit.PKToolPickerEraserItem
  ( PKToolPickerEraserItem
  , IsPKToolPickerEraserItem(..)
  , initWithEraserType
  , initWithEraserType_width
  , eraserTool
  , eraserToolSelector
  , initWithEraserTypeSelector
  , initWithEraserType_widthSelector

  -- * Enum types
  , PKEraserType(PKEraserType)
  , pattern PKEraserTypeVector
  , pattern PKEraserTypeBitmap
  , pattern PKEraserTypeFixedWidthBitmap

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PencilKit.Internal.Classes
import ObjC.PencilKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Create a new eraser tool item.
--
-- ObjC selector: @- initWithEraserType:@
initWithEraserType :: IsPKToolPickerEraserItem pkToolPickerEraserItem => pkToolPickerEraserItem -> PKEraserType -> IO (Id PKToolPickerEraserItem)
initWithEraserType pkToolPickerEraserItem eraserType =
  sendOwnedMessage pkToolPickerEraserItem initWithEraserTypeSelector eraserType

-- | Create a new eraser tool item with a width.
--
-- @eraserType@ — The type of eraser.
--
-- @width@ — The width of the eraser.
--
-- ObjC selector: @- initWithEraserType:width:@
initWithEraserType_width :: IsPKToolPickerEraserItem pkToolPickerEraserItem => pkToolPickerEraserItem -> PKEraserType -> CDouble -> IO (Id PKToolPickerEraserItem)
initWithEraserType_width pkToolPickerEraserItem eraserType width =
  sendOwnedMessage pkToolPickerEraserItem initWithEraserType_widthSelector eraserType width

-- | An eraser tool for erasing parts of a drawing.
--
-- ObjC selector: @- eraserTool@
eraserTool :: IsPKToolPickerEraserItem pkToolPickerEraserItem => pkToolPickerEraserItem -> IO (Id PKEraserTool)
eraserTool pkToolPickerEraserItem =
  sendMessage pkToolPickerEraserItem eraserToolSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithEraserType:@
initWithEraserTypeSelector :: Selector '[PKEraserType] (Id PKToolPickerEraserItem)
initWithEraserTypeSelector = mkSelector "initWithEraserType:"

-- | @Selector@ for @initWithEraserType:width:@
initWithEraserType_widthSelector :: Selector '[PKEraserType, CDouble] (Id PKToolPickerEraserItem)
initWithEraserType_widthSelector = mkSelector "initWithEraserType:width:"

-- | @Selector@ for @eraserTool@
eraserToolSelector :: Selector '[] (Id PKEraserTool)
eraserToolSelector = mkSelector "eraserTool"

