{-# LANGUAGE PatternSynonyms #-}
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
  , initWithEraserTypeSelector
  , initWithEraserType_widthSelector
  , eraserToolSelector

  -- * Enum types
  , PKEraserType(PKEraserType)
  , pattern PKEraserTypeVector
  , pattern PKEraserTypeBitmap
  , pattern PKEraserTypeFixedWidthBitmap

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

import ObjC.PencilKit.Internal.Classes
import ObjC.PencilKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Create a new eraser tool item.
--
-- ObjC selector: @- initWithEraserType:@
initWithEraserType :: IsPKToolPickerEraserItem pkToolPickerEraserItem => pkToolPickerEraserItem -> PKEraserType -> IO (Id PKToolPickerEraserItem)
initWithEraserType pkToolPickerEraserItem  eraserType =
    sendMsg pkToolPickerEraserItem (mkSelector "initWithEraserType:") (retPtr retVoid) [argCLong (coerce eraserType)] >>= ownedObject . castPtr

-- | Create a new eraser tool item with a width.
--
-- @eraserType@ — The type of eraser.
--
-- @width@ — The width of the eraser.
--
-- ObjC selector: @- initWithEraserType:width:@
initWithEraserType_width :: IsPKToolPickerEraserItem pkToolPickerEraserItem => pkToolPickerEraserItem -> PKEraserType -> CDouble -> IO (Id PKToolPickerEraserItem)
initWithEraserType_width pkToolPickerEraserItem  eraserType width =
    sendMsg pkToolPickerEraserItem (mkSelector "initWithEraserType:width:") (retPtr retVoid) [argCLong (coerce eraserType), argCDouble width] >>= ownedObject . castPtr

-- | An eraser tool for erasing parts of a drawing.
--
-- ObjC selector: @- eraserTool@
eraserTool :: IsPKToolPickerEraserItem pkToolPickerEraserItem => pkToolPickerEraserItem -> IO (Id PKEraserTool)
eraserTool pkToolPickerEraserItem  =
    sendMsg pkToolPickerEraserItem (mkSelector "eraserTool") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithEraserType:@
initWithEraserTypeSelector :: Selector
initWithEraserTypeSelector = mkSelector "initWithEraserType:"

-- | @Selector@ for @initWithEraserType:width:@
initWithEraserType_widthSelector :: Selector
initWithEraserType_widthSelector = mkSelector "initWithEraserType:width:"

-- | @Selector@ for @eraserTool@
eraserToolSelector :: Selector
eraserToolSelector = mkSelector "eraserTool"

