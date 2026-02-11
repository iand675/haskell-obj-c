{-# LANGUAGE PatternSynonyms #-}
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
  , initWithEraserTypeSelector
  , initWithEraserType_widthSelector
  , defaultWidthForEraserTypeSelector
  , minimumWidthForEraserTypeSelector
  , maximumWidthForEraserTypeSelector
  , eraserTypeSelector
  , widthSelector

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

-- | @- initWithEraserType:@
initWithEraserType :: IsPKEraserTool pkEraserTool => pkEraserTool -> PKEraserType -> IO (Id PKEraserTool)
initWithEraserType pkEraserTool  eraserType =
  sendMsg pkEraserTool (mkSelector "initWithEraserType:") (retPtr retVoid) [argCLong (coerce eraserType)] >>= ownedObject . castPtr

-- | Create a new eraser tool with a width.
--
-- @eraserType@ — The type of eraser.
--
-- @width@ — The width of the eraser.
--
-- ObjC selector: @- initWithEraserType:width:@
initWithEraserType_width :: IsPKEraserTool pkEraserTool => pkEraserTool -> PKEraserType -> CDouble -> IO (Id PKEraserTool)
initWithEraserType_width pkEraserTool  eraserType width =
  sendMsg pkEraserTool (mkSelector "initWithEraserType:width:") (retPtr retVoid) [argCLong (coerce eraserType), argCDouble (fromIntegral width)] >>= ownedObject . castPtr

-- | The default width for an eraser type.
--
-- ObjC selector: @+ defaultWidthForEraserType:@
defaultWidthForEraserType :: PKEraserType -> IO CDouble
defaultWidthForEraserType eraserType =
  do
    cls' <- getRequiredClass "PKEraserTool"
    sendClassMsg cls' (mkSelector "defaultWidthForEraserType:") retCDouble [argCLong (coerce eraserType)]

-- | The minimum width for an eraser type.
--
-- ObjC selector: @+ minimumWidthForEraserType:@
minimumWidthForEraserType :: PKEraserType -> IO CDouble
minimumWidthForEraserType eraserType =
  do
    cls' <- getRequiredClass "PKEraserTool"
    sendClassMsg cls' (mkSelector "minimumWidthForEraserType:") retCDouble [argCLong (coerce eraserType)]

-- | The maximum width for an eraser type.
--
-- ObjC selector: @+ maximumWidthForEraserType:@
maximumWidthForEraserType :: PKEraserType -> IO CDouble
maximumWidthForEraserType eraserType =
  do
    cls' <- getRequiredClass "PKEraserTool"
    sendClassMsg cls' (mkSelector "maximumWidthForEraserType:") retCDouble [argCLong (coerce eraserType)]

-- | The eraser type.
--
-- ObjC selector: @- eraserType@
eraserType :: IsPKEraserTool pkEraserTool => pkEraserTool -> IO PKEraserType
eraserType pkEraserTool  =
  fmap (coerce :: CLong -> PKEraserType) $ sendMsg pkEraserTool (mkSelector "eraserType") retCLong []

-- | The width of the eraser.
--
-- ObjC selector: @- width@
width :: IsPKEraserTool pkEraserTool => pkEraserTool -> IO CDouble
width pkEraserTool  =
  sendMsg pkEraserTool (mkSelector "width") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithEraserType:@
initWithEraserTypeSelector :: Selector
initWithEraserTypeSelector = mkSelector "initWithEraserType:"

-- | @Selector@ for @initWithEraserType:width:@
initWithEraserType_widthSelector :: Selector
initWithEraserType_widthSelector = mkSelector "initWithEraserType:width:"

-- | @Selector@ for @defaultWidthForEraserType:@
defaultWidthForEraserTypeSelector :: Selector
defaultWidthForEraserTypeSelector = mkSelector "defaultWidthForEraserType:"

-- | @Selector@ for @minimumWidthForEraserType:@
minimumWidthForEraserTypeSelector :: Selector
minimumWidthForEraserTypeSelector = mkSelector "minimumWidthForEraserType:"

-- | @Selector@ for @maximumWidthForEraserType:@
maximumWidthForEraserTypeSelector :: Selector
maximumWidthForEraserTypeSelector = mkSelector "maximumWidthForEraserType:"

-- | @Selector@ for @eraserType@
eraserTypeSelector :: Selector
eraserTypeSelector = mkSelector "eraserType"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

