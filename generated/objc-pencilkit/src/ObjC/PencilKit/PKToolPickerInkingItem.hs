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
  , allowsColorSelection
  , setAllowsColorSelection
  , initWithInkTypeSelector
  , initWithInkType_colorSelector
  , initWithInkType_widthSelector
  , initWithInkType_color_widthSelector
  , initWithInkType_color_width_identifierSelector
  , initWithInkType_color_width_azimuth_identifierSelector
  , allowsColorSelectionSelector
  , setAllowsColorSelectionSelector


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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a new tool picker item with a @PKInkType@.
--
-- @inkType@ — The ink type for the tool.
--
-- ObjC selector: @- initWithInkType:@
initWithInkType :: (IsPKToolPickerInkingItem pkToolPickerInkingItem, IsNSString inkType) => pkToolPickerInkingItem -> inkType -> IO (Id PKToolPickerInkingItem)
initWithInkType pkToolPickerInkingItem  inkType =
withObjCPtr inkType $ \raw_inkType ->
    sendMsg pkToolPickerInkingItem (mkSelector "initWithInkType:") (retPtr retVoid) [argPtr (castPtr raw_inkType :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithInkType:color:@
initWithInkType_color :: (IsPKToolPickerInkingItem pkToolPickerInkingItem, IsNSString inkType, IsNSColor color) => pkToolPickerInkingItem -> inkType -> color -> IO (Id PKToolPickerInkingItem)
initWithInkType_color pkToolPickerInkingItem  inkType color =
withObjCPtr inkType $ \raw_inkType ->
  withObjCPtr color $ \raw_color ->
      sendMsg pkToolPickerInkingItem (mkSelector "initWithInkType:color:") (retPtr retVoid) [argPtr (castPtr raw_inkType :: Ptr ()), argPtr (castPtr raw_color :: Ptr ())] >>= ownedObject . castPtr

-- | Create a new tool picker item with a @PKInkType@.
--
-- @inkType@ — The ink type for the tool.
--
-- @width@ — The width for the tool.
--
-- ObjC selector: @- initWithInkType:width:@
initWithInkType_width :: (IsPKToolPickerInkingItem pkToolPickerInkingItem, IsNSString inkType) => pkToolPickerInkingItem -> inkType -> CDouble -> IO (Id PKToolPickerInkingItem)
initWithInkType_width pkToolPickerInkingItem  inkType width =
withObjCPtr inkType $ \raw_inkType ->
    sendMsg pkToolPickerInkingItem (mkSelector "initWithInkType:width:") (retPtr retVoid) [argPtr (castPtr raw_inkType :: Ptr ()), argCDouble (fromIntegral width)] >>= ownedObject . castPtr

-- | @- initWithInkType:color:width:@
initWithInkType_color_width :: (IsPKToolPickerInkingItem pkToolPickerInkingItem, IsNSString inkType, IsNSColor color) => pkToolPickerInkingItem -> inkType -> color -> CDouble -> IO (Id PKToolPickerInkingItem)
initWithInkType_color_width pkToolPickerInkingItem  inkType color width =
withObjCPtr inkType $ \raw_inkType ->
  withObjCPtr color $ \raw_color ->
      sendMsg pkToolPickerInkingItem (mkSelector "initWithInkType:color:width:") (retPtr retVoid) [argPtr (castPtr raw_inkType :: Ptr ()), argPtr (castPtr raw_color :: Ptr ()), argCDouble (fromIntegral width)] >>= ownedObject . castPtr

-- | @- initWithInkType:color:width:identifier:@
initWithInkType_color_width_identifier :: (IsPKToolPickerInkingItem pkToolPickerInkingItem, IsNSString inkType, IsNSColor color, IsNSString identifier) => pkToolPickerInkingItem -> inkType -> color -> CDouble -> identifier -> IO (Id PKToolPickerInkingItem)
initWithInkType_color_width_identifier pkToolPickerInkingItem  inkType color width identifier =
withObjCPtr inkType $ \raw_inkType ->
  withObjCPtr color $ \raw_color ->
    withObjCPtr identifier $ \raw_identifier ->
        sendMsg pkToolPickerInkingItem (mkSelector "initWithInkType:color:width:identifier:") (retPtr retVoid) [argPtr (castPtr raw_inkType :: Ptr ()), argPtr (castPtr raw_color :: Ptr ()), argCDouble (fromIntegral width), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithInkType:color:width:azimuth:identifier:@
initWithInkType_color_width_azimuth_identifier :: (IsPKToolPickerInkingItem pkToolPickerInkingItem, IsNSString inkType, IsNSColor color, IsNSString identifier) => pkToolPickerInkingItem -> inkType -> color -> CDouble -> CDouble -> identifier -> IO (Id PKToolPickerInkingItem)
initWithInkType_color_width_azimuth_identifier pkToolPickerInkingItem  inkType color width azimuth identifier =
withObjCPtr inkType $ \raw_inkType ->
  withObjCPtr color $ \raw_color ->
    withObjCPtr identifier $ \raw_identifier ->
        sendMsg pkToolPickerInkingItem (mkSelector "initWithInkType:color:width:azimuth:identifier:") (retPtr retVoid) [argPtr (castPtr raw_inkType :: Ptr ()), argPtr (castPtr raw_color :: Ptr ()), argCDouble (fromIntegral width), argCDouble (fromIntegral azimuth), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | Present color selection UI to the user. Default value is YES.
--
-- ObjC selector: @- allowsColorSelection@
allowsColorSelection :: IsPKToolPickerInkingItem pkToolPickerInkingItem => pkToolPickerInkingItem -> IO Bool
allowsColorSelection pkToolPickerInkingItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg pkToolPickerInkingItem (mkSelector "allowsColorSelection") retCULong []

-- | Present color selection UI to the user. Default value is YES.
--
-- ObjC selector: @- setAllowsColorSelection:@
setAllowsColorSelection :: IsPKToolPickerInkingItem pkToolPickerInkingItem => pkToolPickerInkingItem -> Bool -> IO ()
setAllowsColorSelection pkToolPickerInkingItem  value =
  sendMsg pkToolPickerInkingItem (mkSelector "setAllowsColorSelection:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithInkType:@
initWithInkTypeSelector :: Selector
initWithInkTypeSelector = mkSelector "initWithInkType:"

-- | @Selector@ for @initWithInkType:color:@
initWithInkType_colorSelector :: Selector
initWithInkType_colorSelector = mkSelector "initWithInkType:color:"

-- | @Selector@ for @initWithInkType:width:@
initWithInkType_widthSelector :: Selector
initWithInkType_widthSelector = mkSelector "initWithInkType:width:"

-- | @Selector@ for @initWithInkType:color:width:@
initWithInkType_color_widthSelector :: Selector
initWithInkType_color_widthSelector = mkSelector "initWithInkType:color:width:"

-- | @Selector@ for @initWithInkType:color:width:identifier:@
initWithInkType_color_width_identifierSelector :: Selector
initWithInkType_color_width_identifierSelector = mkSelector "initWithInkType:color:width:identifier:"

-- | @Selector@ for @initWithInkType:color:width:azimuth:identifier:@
initWithInkType_color_width_azimuth_identifierSelector :: Selector
initWithInkType_color_width_azimuth_identifierSelector = mkSelector "initWithInkType:color:width:azimuth:identifier:"

-- | @Selector@ for @allowsColorSelection@
allowsColorSelectionSelector :: Selector
allowsColorSelectionSelector = mkSelector "allowsColorSelection"

-- | @Selector@ for @setAllowsColorSelection:@
setAllowsColorSelectionSelector :: Selector
setAllowsColorSelectionSelector = mkSelector "setAllowsColorSelection:"

