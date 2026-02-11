{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A tool for drawing on a PKCanvasView.
--
-- Generated bindings for @PKInkingTool@.
module ObjC.PencilKit.PKInkingTool
  ( PKInkingTool
  , IsPKInkingTool(..)
  , initWithInkType_color_width
  , initWithInkType_color_width_azimuth
  , initWithInkType_color
  , initWithInk_width
  , defaultWidthForInkType
  , minimumWidthForInkType
  , maximumWidthForInkType
  , invertColor
  , inkType
  , color
  , width
  , azimuth
  , ink
  , requiredContentVersion
  , initWithInkType_color_widthSelector
  , initWithInkType_color_width_azimuthSelector
  , initWithInkType_colorSelector
  , initWithInk_widthSelector
  , defaultWidthForInkTypeSelector
  , minimumWidthForInkTypeSelector
  , maximumWidthForInkTypeSelector
  , invertColorSelector
  , inkTypeSelector
  , colorSelector
  , widthSelector
  , azimuthSelector
  , inkSelector
  , requiredContentVersionSelector

  -- * Enum types
  , PKContentVersion(PKContentVersion)
  , pattern PKContentVersion1
  , pattern PKContentVersion2
  , pattern PKContentVersion3
  , pattern PKContentVersion4
  , pattern PKContentVersionLatest

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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithInkType:color:width:@
initWithInkType_color_width :: (IsPKInkingTool pkInkingTool, IsNSString type_, IsNSColor color) => pkInkingTool -> type_ -> color -> CDouble -> IO (Id PKInkingTool)
initWithInkType_color_width pkInkingTool  type_ color width =
  withObjCPtr type_ $ \raw_type_ ->
    withObjCPtr color $ \raw_color ->
        sendMsg pkInkingTool (mkSelector "initWithInkType:color:width:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ()), argPtr (castPtr raw_color :: Ptr ()), argCDouble width] >>= ownedObject . castPtr

-- | @- initWithInkType:color:width:azimuth:@
initWithInkType_color_width_azimuth :: (IsPKInkingTool pkInkingTool, IsNSString type_, IsNSColor color) => pkInkingTool -> type_ -> color -> CDouble -> CDouble -> IO (Id PKInkingTool)
initWithInkType_color_width_azimuth pkInkingTool  type_ color width angle =
  withObjCPtr type_ $ \raw_type_ ->
    withObjCPtr color $ \raw_color ->
        sendMsg pkInkingTool (mkSelector "initWithInkType:color:width:azimuth:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ()), argPtr (castPtr raw_color :: Ptr ()), argCDouble width, argCDouble angle] >>= ownedObject . castPtr

-- | @- initWithInkType:color:@
initWithInkType_color :: (IsPKInkingTool pkInkingTool, IsNSString type_, IsNSColor color) => pkInkingTool -> type_ -> color -> IO (Id PKInkingTool)
initWithInkType_color pkInkingTool  type_ color =
  withObjCPtr type_ $ \raw_type_ ->
    withObjCPtr color $ \raw_color ->
        sendMsg pkInkingTool (mkSelector "initWithInkType:color:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ()), argPtr (castPtr raw_color :: Ptr ())] >>= ownedObject . castPtr

-- | Create a new inking tool for the provided ink.
--
-- @ink@ — The ink to use.
--
-- @width@ — The width of stroke to create.
--
-- ObjC selector: @- initWithInk:width:@
initWithInk_width :: (IsPKInkingTool pkInkingTool, IsPKInk ink) => pkInkingTool -> ink -> CDouble -> IO (Id PKInkingTool)
initWithInk_width pkInkingTool  ink width =
  withObjCPtr ink $ \raw_ink ->
      sendMsg pkInkingTool (mkSelector "initWithInk:width:") (retPtr retVoid) [argPtr (castPtr raw_ink :: Ptr ()), argCDouble width] >>= ownedObject . castPtr

-- | The default width for an ink of a type.
--
-- ObjC selector: @+ defaultWidthForInkType:@
defaultWidthForInkType :: IsNSString inkType => inkType -> IO CDouble
defaultWidthForInkType inkType =
  do
    cls' <- getRequiredClass "PKInkingTool"
    withObjCPtr inkType $ \raw_inkType ->
      sendClassMsg cls' (mkSelector "defaultWidthForInkType:") retCDouble [argPtr (castPtr raw_inkType :: Ptr ())]

-- | The minimum width for an ink of a type.
--
-- ObjC selector: @+ minimumWidthForInkType:@
minimumWidthForInkType :: IsNSString inkType => inkType -> IO CDouble
minimumWidthForInkType inkType =
  do
    cls' <- getRequiredClass "PKInkingTool"
    withObjCPtr inkType $ \raw_inkType ->
      sendClassMsg cls' (mkSelector "minimumWidthForInkType:") retCDouble [argPtr (castPtr raw_inkType :: Ptr ())]

-- | The maximum width for an ink of a type.
--
-- ObjC selector: @+ maximumWidthForInkType:@
maximumWidthForInkType :: IsNSString inkType => inkType -> IO CDouble
maximumWidthForInkType inkType =
  do
    cls' <- getRequiredClass "PKInkingTool"
    withObjCPtr inkType $ \raw_inkType ->
      sendClassMsg cls' (mkSelector "maximumWidthForInkType:") retCDouble [argPtr (castPtr raw_inkType :: Ptr ())]

-- | Converts a color from light to dark appearance or vice versa.
--
-- @color@ — The color to be inverted light<->dark.
--
-- Returns: The inverted color.
--
-- This has the same effect as @convertColor@ with opposite user interface styles.
--
-- ObjC selector: @+ invertColor:@
invertColor :: Ptr () -> IO (Ptr ())
invertColor color =
  do
    cls' <- getRequiredClass "PKInkingTool"
    fmap castPtr $ sendClassMsg cls' (mkSelector "invertColor:") (retPtr retVoid) [argPtr color]

-- | The type of ink, eg. pen, pencil...
--
-- ObjC selector: @- inkType@
inkType :: IsPKInkingTool pkInkingTool => pkInkingTool -> IO (Id NSString)
inkType pkInkingTool  =
    sendMsg pkInkingTool (mkSelector "inkType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- color@
color :: IsPKInkingTool pkInkingTool => pkInkingTool -> IO (Id NSColor)
color pkInkingTool  =
    sendMsg pkInkingTool (mkSelector "color") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The base width of the ink.
--
-- ObjC selector: @- width@
width :: IsPKInkingTool pkInkingTool => pkInkingTool -> IO CDouble
width pkInkingTool  =
    sendMsg pkInkingTool (mkSelector "width") retCDouble []

-- | The base angle of the ink.
--
-- ObjC selector: @- azimuth@
azimuth :: IsPKInkingTool pkInkingTool => pkInkingTool -> IO CDouble
azimuth pkInkingTool  =
    sendMsg pkInkingTool (mkSelector "azimuth") retCDouble []

-- | The ink that this tool will create strokes with.
--
-- ObjC selector: @- ink@
ink :: IsPKInkingTool pkInkingTool => pkInkingTool -> IO (Id PKInk)
ink pkInkingTool  =
    sendMsg pkInkingTool (mkSelector "ink") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The PencilKit version required to use this inking tool.
--
-- ObjC selector: @- requiredContentVersion@
requiredContentVersion :: IsPKInkingTool pkInkingTool => pkInkingTool -> IO PKContentVersion
requiredContentVersion pkInkingTool  =
    fmap (coerce :: CLong -> PKContentVersion) $ sendMsg pkInkingTool (mkSelector "requiredContentVersion") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithInkType:color:width:@
initWithInkType_color_widthSelector :: Selector
initWithInkType_color_widthSelector = mkSelector "initWithInkType:color:width:"

-- | @Selector@ for @initWithInkType:color:width:azimuth:@
initWithInkType_color_width_azimuthSelector :: Selector
initWithInkType_color_width_azimuthSelector = mkSelector "initWithInkType:color:width:azimuth:"

-- | @Selector@ for @initWithInkType:color:@
initWithInkType_colorSelector :: Selector
initWithInkType_colorSelector = mkSelector "initWithInkType:color:"

-- | @Selector@ for @initWithInk:width:@
initWithInk_widthSelector :: Selector
initWithInk_widthSelector = mkSelector "initWithInk:width:"

-- | @Selector@ for @defaultWidthForInkType:@
defaultWidthForInkTypeSelector :: Selector
defaultWidthForInkTypeSelector = mkSelector "defaultWidthForInkType:"

-- | @Selector@ for @minimumWidthForInkType:@
minimumWidthForInkTypeSelector :: Selector
minimumWidthForInkTypeSelector = mkSelector "minimumWidthForInkType:"

-- | @Selector@ for @maximumWidthForInkType:@
maximumWidthForInkTypeSelector :: Selector
maximumWidthForInkTypeSelector = mkSelector "maximumWidthForInkType:"

-- | @Selector@ for @invertColor:@
invertColorSelector :: Selector
invertColorSelector = mkSelector "invertColor:"

-- | @Selector@ for @inkType@
inkTypeSelector :: Selector
inkTypeSelector = mkSelector "inkType"

-- | @Selector@ for @color@
colorSelector :: Selector
colorSelector = mkSelector "color"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @azimuth@
azimuthSelector :: Selector
azimuthSelector = mkSelector "azimuth"

-- | @Selector@ for @ink@
inkSelector :: Selector
inkSelector = mkSelector "ink"

-- | @Selector@ for @requiredContentVersion@
requiredContentVersionSelector :: Selector
requiredContentVersionSelector = mkSelector "requiredContentVersion"

