{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSColorPicker@.
module ObjC.AppKit.NSColorPicker
  ( NSColorPicker
  , IsNSColorPicker(..)
  , initWithPickerMask_colorPanel
  , insertNewButtonImage_in
  , viewSizeChanged
  , attachColorList
  , detachColorList
  , setMode
  , colorPanel
  , provideNewButtonImage
  , buttonToolTip
  , minContentSize
  , initWithPickerMask_colorPanelSelector
  , insertNewButtonImage_inSelector
  , viewSizeChangedSelector
  , attachColorListSelector
  , detachColorListSelector
  , setModeSelector
  , colorPanelSelector
  , provideNewButtonImageSelector
  , buttonToolTipSelector
  , minContentSizeSelector

  -- * Enum types
  , NSColorPanelMode(NSColorPanelMode)
  , pattern NSColorPanelModeNone
  , pattern NSColorPanelModeGray
  , pattern NSColorPanelModeRGB
  , pattern NSColorPanelModeCMYK
  , pattern NSColorPanelModeHSB
  , pattern NSColorPanelModeCustomPalette
  , pattern NSColorPanelModeColorList
  , pattern NSColorPanelModeWheel
  , pattern NSColorPanelModeCrayon

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithPickerMask:colorPanel:@
initWithPickerMask_colorPanel :: (IsNSColorPicker nsColorPicker, IsNSColorPanel owningColorPanel) => nsColorPicker -> CULong -> owningColorPanel -> IO (Id NSColorPicker)
initWithPickerMask_colorPanel nsColorPicker  mask owningColorPanel =
withObjCPtr owningColorPanel $ \raw_owningColorPanel ->
    sendMsg nsColorPicker (mkSelector "initWithPickerMask:colorPanel:") (retPtr retVoid) [argCULong (fromIntegral mask), argPtr (castPtr raw_owningColorPanel :: Ptr ())] >>= ownedObject . castPtr

-- | @- insertNewButtonImage:in:@
insertNewButtonImage_in :: (IsNSColorPicker nsColorPicker, IsNSImage newButtonImage, IsNSButtonCell buttonCell) => nsColorPicker -> newButtonImage -> buttonCell -> IO ()
insertNewButtonImage_in nsColorPicker  newButtonImage buttonCell =
withObjCPtr newButtonImage $ \raw_newButtonImage ->
  withObjCPtr buttonCell $ \raw_buttonCell ->
      sendMsg nsColorPicker (mkSelector "insertNewButtonImage:in:") retVoid [argPtr (castPtr raw_newButtonImage :: Ptr ()), argPtr (castPtr raw_buttonCell :: Ptr ())]

-- | @- viewSizeChanged:@
viewSizeChanged :: IsNSColorPicker nsColorPicker => nsColorPicker -> RawId -> IO ()
viewSizeChanged nsColorPicker  sender =
  sendMsg nsColorPicker (mkSelector "viewSizeChanged:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- attachColorList:@
attachColorList :: (IsNSColorPicker nsColorPicker, IsNSColorList colorList) => nsColorPicker -> colorList -> IO ()
attachColorList nsColorPicker  colorList =
withObjCPtr colorList $ \raw_colorList ->
    sendMsg nsColorPicker (mkSelector "attachColorList:") retVoid [argPtr (castPtr raw_colorList :: Ptr ())]

-- | @- detachColorList:@
detachColorList :: (IsNSColorPicker nsColorPicker, IsNSColorList colorList) => nsColorPicker -> colorList -> IO ()
detachColorList nsColorPicker  colorList =
withObjCPtr colorList $ \raw_colorList ->
    sendMsg nsColorPicker (mkSelector "detachColorList:") retVoid [argPtr (castPtr raw_colorList :: Ptr ())]

-- | @- setMode:@
setMode :: IsNSColorPicker nsColorPicker => nsColorPicker -> NSColorPanelMode -> IO ()
setMode nsColorPicker  mode =
  sendMsg nsColorPicker (mkSelector "setMode:") retVoid [argCLong (coerce mode)]

-- | @- colorPanel@
colorPanel :: IsNSColorPicker nsColorPicker => nsColorPicker -> IO (Id NSColorPanel)
colorPanel nsColorPicker  =
  sendMsg nsColorPicker (mkSelector "colorPanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- provideNewButtonImage@
provideNewButtonImage :: IsNSColorPicker nsColorPicker => nsColorPicker -> IO (Id NSImage)
provideNewButtonImage nsColorPicker  =
  sendMsg nsColorPicker (mkSelector "provideNewButtonImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- buttonToolTip@
buttonToolTip :: IsNSColorPicker nsColorPicker => nsColorPicker -> IO (Id NSString)
buttonToolTip nsColorPicker  =
  sendMsg nsColorPicker (mkSelector "buttonToolTip") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- minContentSize@
minContentSize :: IsNSColorPicker nsColorPicker => nsColorPicker -> IO NSSize
minContentSize nsColorPicker  =
  sendMsgStret nsColorPicker (mkSelector "minContentSize") retNSSize []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPickerMask:colorPanel:@
initWithPickerMask_colorPanelSelector :: Selector
initWithPickerMask_colorPanelSelector = mkSelector "initWithPickerMask:colorPanel:"

-- | @Selector@ for @insertNewButtonImage:in:@
insertNewButtonImage_inSelector :: Selector
insertNewButtonImage_inSelector = mkSelector "insertNewButtonImage:in:"

-- | @Selector@ for @viewSizeChanged:@
viewSizeChangedSelector :: Selector
viewSizeChangedSelector = mkSelector "viewSizeChanged:"

-- | @Selector@ for @attachColorList:@
attachColorListSelector :: Selector
attachColorListSelector = mkSelector "attachColorList:"

-- | @Selector@ for @detachColorList:@
detachColorListSelector :: Selector
detachColorListSelector = mkSelector "detachColorList:"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @colorPanel@
colorPanelSelector :: Selector
colorPanelSelector = mkSelector "colorPanel"

-- | @Selector@ for @provideNewButtonImage@
provideNewButtonImageSelector :: Selector
provideNewButtonImageSelector = mkSelector "provideNewButtonImage"

-- | @Selector@ for @buttonToolTip@
buttonToolTipSelector :: Selector
buttonToolTipSelector = mkSelector "buttonToolTip"

-- | @Selector@ for @minContentSize@
minContentSizeSelector :: Selector
minContentSizeSelector = mkSelector "minContentSize"

