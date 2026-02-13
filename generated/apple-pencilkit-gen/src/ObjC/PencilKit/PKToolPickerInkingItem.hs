{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A user interface for an inking tool item in PKToolPicker.
--
-- Generated bindings for @PKToolPickerInkingItem@.
module ObjC.PencilKit.PKToolPickerInkingItem
  ( PKToolPickerInkingItem
  , IsPKToolPickerInkingItem(..)
  , initWithInkType
  , initWithInkType_color
  , initWithInkType_width
  , initWithInkType_color_width
  , initWithInkType_color_width_identifier
  , initWithInkType_color_width_azimuth_identifier
  , inkingTool
  , allowsColorSelection
  , setAllowsColorSelection
  , allowsColorSelectionSelector
  , initWithInkTypeSelector
  , initWithInkType_colorSelector
  , initWithInkType_color_widthSelector
  , initWithInkType_color_width_azimuth_identifierSelector
  , initWithInkType_color_width_identifierSelector
  , initWithInkType_widthSelector
  , inkingToolSelector
  , setAllowsColorSelectionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PencilKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a new tool picker item with a @PKInkType@.
--
-- @inkType@ — The ink type for the tool.
--
-- ObjC selector: @- initWithInkType:@
initWithInkType :: (IsPKToolPickerInkingItem pkToolPickerInkingItem, IsNSString inkType) => pkToolPickerInkingItem -> inkType -> IO (Id PKToolPickerInkingItem)
initWithInkType pkToolPickerInkingItem inkType =
  sendOwnedMessage pkToolPickerInkingItem initWithInkTypeSelector (toNSString inkType)

-- | @- initWithInkType:color:@
initWithInkType_color :: (IsPKToolPickerInkingItem pkToolPickerInkingItem, IsNSString inkType, IsNSColor color) => pkToolPickerInkingItem -> inkType -> color -> IO (Id PKToolPickerInkingItem)
initWithInkType_color pkToolPickerInkingItem inkType color =
  sendOwnedMessage pkToolPickerInkingItem initWithInkType_colorSelector (toNSString inkType) (toNSColor color)

-- | Create a new tool picker item with a @PKInkType@.
--
-- @inkType@ — The ink type for the tool.
--
-- @width@ — The width for the tool.
--
-- ObjC selector: @- initWithInkType:width:@
initWithInkType_width :: (IsPKToolPickerInkingItem pkToolPickerInkingItem, IsNSString inkType) => pkToolPickerInkingItem -> inkType -> CDouble -> IO (Id PKToolPickerInkingItem)
initWithInkType_width pkToolPickerInkingItem inkType width =
  sendOwnedMessage pkToolPickerInkingItem initWithInkType_widthSelector (toNSString inkType) width

-- | @- initWithInkType:color:width:@
initWithInkType_color_width :: (IsPKToolPickerInkingItem pkToolPickerInkingItem, IsNSString inkType, IsNSColor color) => pkToolPickerInkingItem -> inkType -> color -> CDouble -> IO (Id PKToolPickerInkingItem)
initWithInkType_color_width pkToolPickerInkingItem inkType color width =
  sendOwnedMessage pkToolPickerInkingItem initWithInkType_color_widthSelector (toNSString inkType) (toNSColor color) width

-- | @- initWithInkType:color:width:identifier:@
initWithInkType_color_width_identifier :: (IsPKToolPickerInkingItem pkToolPickerInkingItem, IsNSString inkType, IsNSColor color, IsNSString identifier) => pkToolPickerInkingItem -> inkType -> color -> CDouble -> identifier -> IO (Id PKToolPickerInkingItem)
initWithInkType_color_width_identifier pkToolPickerInkingItem inkType color width identifier =
  sendOwnedMessage pkToolPickerInkingItem initWithInkType_color_width_identifierSelector (toNSString inkType) (toNSColor color) width (toNSString identifier)

-- | @- initWithInkType:color:width:azimuth:identifier:@
initWithInkType_color_width_azimuth_identifier :: (IsPKToolPickerInkingItem pkToolPickerInkingItem, IsNSString inkType, IsNSColor color, IsNSString identifier) => pkToolPickerInkingItem -> inkType -> color -> CDouble -> CDouble -> identifier -> IO (Id PKToolPickerInkingItem)
initWithInkType_color_width_azimuth_identifier pkToolPickerInkingItem inkType color width azimuth identifier =
  sendOwnedMessage pkToolPickerInkingItem initWithInkType_color_width_azimuth_identifierSelector (toNSString inkType) (toNSColor color) width azimuth (toNSString identifier)

-- | A tool for drawing on a @PKCanvasView@.
--
-- ObjC selector: @- inkingTool@
inkingTool :: IsPKToolPickerInkingItem pkToolPickerInkingItem => pkToolPickerInkingItem -> IO (Id PKInkingTool)
inkingTool pkToolPickerInkingItem =
  sendMessage pkToolPickerInkingItem inkingToolSelector

-- | Present color selection UI to the user. Default value is YES.
--
-- ObjC selector: @- allowsColorSelection@
allowsColorSelection :: IsPKToolPickerInkingItem pkToolPickerInkingItem => pkToolPickerInkingItem -> IO Bool
allowsColorSelection pkToolPickerInkingItem =
  sendMessage pkToolPickerInkingItem allowsColorSelectionSelector

-- | Present color selection UI to the user. Default value is YES.
--
-- ObjC selector: @- setAllowsColorSelection:@
setAllowsColorSelection :: IsPKToolPickerInkingItem pkToolPickerInkingItem => pkToolPickerInkingItem -> Bool -> IO ()
setAllowsColorSelection pkToolPickerInkingItem value =
  sendMessage pkToolPickerInkingItem setAllowsColorSelectionSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithInkType:@
initWithInkTypeSelector :: Selector '[Id NSString] (Id PKToolPickerInkingItem)
initWithInkTypeSelector = mkSelector "initWithInkType:"

-- | @Selector@ for @initWithInkType:color:@
initWithInkType_colorSelector :: Selector '[Id NSString, Id NSColor] (Id PKToolPickerInkingItem)
initWithInkType_colorSelector = mkSelector "initWithInkType:color:"

-- | @Selector@ for @initWithInkType:width:@
initWithInkType_widthSelector :: Selector '[Id NSString, CDouble] (Id PKToolPickerInkingItem)
initWithInkType_widthSelector = mkSelector "initWithInkType:width:"

-- | @Selector@ for @initWithInkType:color:width:@
initWithInkType_color_widthSelector :: Selector '[Id NSString, Id NSColor, CDouble] (Id PKToolPickerInkingItem)
initWithInkType_color_widthSelector = mkSelector "initWithInkType:color:width:"

-- | @Selector@ for @initWithInkType:color:width:identifier:@
initWithInkType_color_width_identifierSelector :: Selector '[Id NSString, Id NSColor, CDouble, Id NSString] (Id PKToolPickerInkingItem)
initWithInkType_color_width_identifierSelector = mkSelector "initWithInkType:color:width:identifier:"

-- | @Selector@ for @initWithInkType:color:width:azimuth:identifier:@
initWithInkType_color_width_azimuth_identifierSelector :: Selector '[Id NSString, Id NSColor, CDouble, CDouble, Id NSString] (Id PKToolPickerInkingItem)
initWithInkType_color_width_azimuth_identifierSelector = mkSelector "initWithInkType:color:width:azimuth:identifier:"

-- | @Selector@ for @inkingTool@
inkingToolSelector :: Selector '[] (Id PKInkingTool)
inkingToolSelector = mkSelector "inkingTool"

-- | @Selector@ for @allowsColorSelection@
allowsColorSelectionSelector :: Selector '[] Bool
allowsColorSelectionSelector = mkSelector "allowsColorSelection"

-- | @Selector@ for @setAllowsColorSelection:@
setAllowsColorSelectionSelector :: Selector '[Bool] ()
setAllowsColorSelectionSelector = mkSelector "setAllowsColorSelection:"

