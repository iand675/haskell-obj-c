{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An eraser tool for erasing parts of a drawing.
--
-- Generated bindings for @PKEraserTool@.
module ObjC.PencilKit.PKEraserTool
  ( PKEraserTool
  , IsPKEraserTool(..)
  , initWithEraserType
  , initWithEraserType_width
  , defaultWidthForEraserType
  , minimumWidthForEraserType
  , maximumWidthForEraserType
  , eraserType
  , width
  , defaultWidthForEraserTypeSelector
  , eraserTypeSelector
  , initWithEraserTypeSelector
  , initWithEraserType_widthSelector
  , maximumWidthForEraserTypeSelector
  , minimumWidthForEraserTypeSelector
  , widthSelector

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

-- | @- initWithEraserType:@
initWithEraserType :: IsPKEraserTool pkEraserTool => pkEraserTool -> PKEraserType -> IO (Id PKEraserTool)
initWithEraserType pkEraserTool eraserType =
  sendOwnedMessage pkEraserTool initWithEraserTypeSelector eraserType

-- | Create a new eraser tool with a width.
--
-- @eraserType@ — The type of eraser.
--
-- @width@ — The width of the eraser.
--
-- ObjC selector: @- initWithEraserType:width:@
initWithEraserType_width :: IsPKEraserTool pkEraserTool => pkEraserTool -> PKEraserType -> CDouble -> IO (Id PKEraserTool)
initWithEraserType_width pkEraserTool eraserType width =
  sendOwnedMessage pkEraserTool initWithEraserType_widthSelector eraserType width

-- | The default width for an eraser type.
--
-- ObjC selector: @+ defaultWidthForEraserType:@
defaultWidthForEraserType :: PKEraserType -> IO CDouble
defaultWidthForEraserType eraserType =
  do
    cls' <- getRequiredClass "PKEraserTool"
    sendClassMessage cls' defaultWidthForEraserTypeSelector eraserType

-- | The minimum width for an eraser type.
--
-- ObjC selector: @+ minimumWidthForEraserType:@
minimumWidthForEraserType :: PKEraserType -> IO CDouble
minimumWidthForEraserType eraserType =
  do
    cls' <- getRequiredClass "PKEraserTool"
    sendClassMessage cls' minimumWidthForEraserTypeSelector eraserType

-- | The maximum width for an eraser type.
--
-- ObjC selector: @+ maximumWidthForEraserType:@
maximumWidthForEraserType :: PKEraserType -> IO CDouble
maximumWidthForEraserType eraserType =
  do
    cls' <- getRequiredClass "PKEraserTool"
    sendClassMessage cls' maximumWidthForEraserTypeSelector eraserType

-- | The eraser type.
--
-- ObjC selector: @- eraserType@
eraserType :: IsPKEraserTool pkEraserTool => pkEraserTool -> IO PKEraserType
eraserType pkEraserTool =
  sendMessage pkEraserTool eraserTypeSelector

-- | The width of the eraser.
--
-- ObjC selector: @- width@
width :: IsPKEraserTool pkEraserTool => pkEraserTool -> IO CDouble
width pkEraserTool =
  sendMessage pkEraserTool widthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithEraserType:@
initWithEraserTypeSelector :: Selector '[PKEraserType] (Id PKEraserTool)
initWithEraserTypeSelector = mkSelector "initWithEraserType:"

-- | @Selector@ for @initWithEraserType:width:@
initWithEraserType_widthSelector :: Selector '[PKEraserType, CDouble] (Id PKEraserTool)
initWithEraserType_widthSelector = mkSelector "initWithEraserType:width:"

-- | @Selector@ for @defaultWidthForEraserType:@
defaultWidthForEraserTypeSelector :: Selector '[PKEraserType] CDouble
defaultWidthForEraserTypeSelector = mkSelector "defaultWidthForEraserType:"

-- | @Selector@ for @minimumWidthForEraserType:@
minimumWidthForEraserTypeSelector :: Selector '[PKEraserType] CDouble
minimumWidthForEraserTypeSelector = mkSelector "minimumWidthForEraserType:"

-- | @Selector@ for @maximumWidthForEraserType:@
maximumWidthForEraserTypeSelector :: Selector '[PKEraserType] CDouble
maximumWidthForEraserTypeSelector = mkSelector "maximumWidthForEraserType:"

-- | @Selector@ for @eraserType@
eraserTypeSelector :: Selector '[] PKEraserType
eraserTypeSelector = mkSelector "eraserType"

-- | @Selector@ for @width@
widthSelector :: Selector '[] CDouble
widthSelector = mkSelector "width"

