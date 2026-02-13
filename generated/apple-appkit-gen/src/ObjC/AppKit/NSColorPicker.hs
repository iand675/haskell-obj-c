{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , attachColorListSelector
  , buttonToolTipSelector
  , colorPanelSelector
  , detachColorListSelector
  , initWithPickerMask_colorPanelSelector
  , insertNewButtonImage_inSelector
  , minContentSizeSelector
  , provideNewButtonImageSelector
  , setModeSelector
  , viewSizeChangedSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithPickerMask:colorPanel:@
initWithPickerMask_colorPanel :: (IsNSColorPicker nsColorPicker, IsNSColorPanel owningColorPanel) => nsColorPicker -> CULong -> owningColorPanel -> IO (Id NSColorPicker)
initWithPickerMask_colorPanel nsColorPicker mask owningColorPanel =
  sendOwnedMessage nsColorPicker initWithPickerMask_colorPanelSelector mask (toNSColorPanel owningColorPanel)

-- | @- insertNewButtonImage:in:@
insertNewButtonImage_in :: (IsNSColorPicker nsColorPicker, IsNSImage newButtonImage, IsNSButtonCell buttonCell) => nsColorPicker -> newButtonImage -> buttonCell -> IO ()
insertNewButtonImage_in nsColorPicker newButtonImage buttonCell =
  sendMessage nsColorPicker insertNewButtonImage_inSelector (toNSImage newButtonImage) (toNSButtonCell buttonCell)

-- | @- viewSizeChanged:@
viewSizeChanged :: IsNSColorPicker nsColorPicker => nsColorPicker -> RawId -> IO ()
viewSizeChanged nsColorPicker sender =
  sendMessage nsColorPicker viewSizeChangedSelector sender

-- | @- attachColorList:@
attachColorList :: (IsNSColorPicker nsColorPicker, IsNSColorList colorList) => nsColorPicker -> colorList -> IO ()
attachColorList nsColorPicker colorList =
  sendMessage nsColorPicker attachColorListSelector (toNSColorList colorList)

-- | @- detachColorList:@
detachColorList :: (IsNSColorPicker nsColorPicker, IsNSColorList colorList) => nsColorPicker -> colorList -> IO ()
detachColorList nsColorPicker colorList =
  sendMessage nsColorPicker detachColorListSelector (toNSColorList colorList)

-- | @- setMode:@
setMode :: IsNSColorPicker nsColorPicker => nsColorPicker -> NSColorPanelMode -> IO ()
setMode nsColorPicker mode =
  sendMessage nsColorPicker setModeSelector mode

-- | @- colorPanel@
colorPanel :: IsNSColorPicker nsColorPicker => nsColorPicker -> IO (Id NSColorPanel)
colorPanel nsColorPicker =
  sendMessage nsColorPicker colorPanelSelector

-- | @- provideNewButtonImage@
provideNewButtonImage :: IsNSColorPicker nsColorPicker => nsColorPicker -> IO (Id NSImage)
provideNewButtonImage nsColorPicker =
  sendMessage nsColorPicker provideNewButtonImageSelector

-- | @- buttonToolTip@
buttonToolTip :: IsNSColorPicker nsColorPicker => nsColorPicker -> IO (Id NSString)
buttonToolTip nsColorPicker =
  sendMessage nsColorPicker buttonToolTipSelector

-- | @- minContentSize@
minContentSize :: IsNSColorPicker nsColorPicker => nsColorPicker -> IO NSSize
minContentSize nsColorPicker =
  sendMessage nsColorPicker minContentSizeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPickerMask:colorPanel:@
initWithPickerMask_colorPanelSelector :: Selector '[CULong, Id NSColorPanel] (Id NSColorPicker)
initWithPickerMask_colorPanelSelector = mkSelector "initWithPickerMask:colorPanel:"

-- | @Selector@ for @insertNewButtonImage:in:@
insertNewButtonImage_inSelector :: Selector '[Id NSImage, Id NSButtonCell] ()
insertNewButtonImage_inSelector = mkSelector "insertNewButtonImage:in:"

-- | @Selector@ for @viewSizeChanged:@
viewSizeChangedSelector :: Selector '[RawId] ()
viewSizeChangedSelector = mkSelector "viewSizeChanged:"

-- | @Selector@ for @attachColorList:@
attachColorListSelector :: Selector '[Id NSColorList] ()
attachColorListSelector = mkSelector "attachColorList:"

-- | @Selector@ for @detachColorList:@
detachColorListSelector :: Selector '[Id NSColorList] ()
detachColorListSelector = mkSelector "detachColorList:"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector '[NSColorPanelMode] ()
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @colorPanel@
colorPanelSelector :: Selector '[] (Id NSColorPanel)
colorPanelSelector = mkSelector "colorPanel"

-- | @Selector@ for @provideNewButtonImage@
provideNewButtonImageSelector :: Selector '[] (Id NSImage)
provideNewButtonImageSelector = mkSelector "provideNewButtonImage"

-- | @Selector@ for @buttonToolTip@
buttonToolTipSelector :: Selector '[] (Id NSString)
buttonToolTipSelector = mkSelector "buttonToolTip"

-- | @Selector@ for @minContentSize@
minContentSizeSelector :: Selector '[] NSSize
minContentSizeSelector = mkSelector "minContentSize"

