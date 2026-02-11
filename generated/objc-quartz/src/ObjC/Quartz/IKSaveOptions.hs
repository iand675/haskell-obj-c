{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IKSaveOptions
--
-- The IKSaveOptions class initializes, adds, and manages user interface options for saving image data.
--
-- Generated bindings for @IKSaveOptions@.
module ObjC.Quartz.IKSaveOptions
  ( IKSaveOptions
  , IsIKSaveOptions(..)
  , initWithImageProperties_imageUTType
  , addSaveOptionsAccessoryViewToSavePanel
  , addSaveOptionsToView
  , delegate
  , setDelegate
  , imageProperties
  , imageUTType
  , userSelection
  , rememberLastSetting
  , setRememberLastSetting
  , initWithImageProperties_imageUTTypeSelector
  , addSaveOptionsAccessoryViewToSavePanelSelector
  , addSaveOptionsToViewSelector
  , delegateSelector
  , setDelegateSelector
  , imagePropertiesSelector
  , imageUTTypeSelector
  , userSelectionSelector
  , rememberLastSettingSelector
  , setRememberLastSettingSelector


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

import ObjC.Quartz.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithImageProperties:imageUTType:
--
-- Initializes IKSaveOptions with metadata and UTType.
--
-- ObjC selector: @- initWithImageProperties:imageUTType:@
initWithImageProperties_imageUTType :: (IsIKSaveOptions ikSaveOptions, IsNSDictionary imageProperties, IsNSString imageUTType) => ikSaveOptions -> imageProperties -> imageUTType -> IO (Id IKSaveOptions)
initWithImageProperties_imageUTType ikSaveOptions  imageProperties imageUTType =
withObjCPtr imageProperties $ \raw_imageProperties ->
  withObjCPtr imageUTType $ \raw_imageUTType ->
      sendMsg ikSaveOptions (mkSelector "initWithImageProperties:imageUTType:") (retPtr retVoid) [argPtr (castPtr raw_imageProperties :: Ptr ()), argPtr (castPtr raw_imageUTType :: Ptr ())] >>= ownedObject . castPtr

-- | addSaveOptionsAccessoryViewToSavePanel:
--
-- Adds IKSaveOptions UI to a NSSavePanel.
--
-- ObjC selector: @- addSaveOptionsAccessoryViewToSavePanel:@
addSaveOptionsAccessoryViewToSavePanel :: (IsIKSaveOptions ikSaveOptions, IsNSSavePanel savePanel) => ikSaveOptions -> savePanel -> IO ()
addSaveOptionsAccessoryViewToSavePanel ikSaveOptions  savePanel =
withObjCPtr savePanel $ \raw_savePanel ->
    sendMsg ikSaveOptions (mkSelector "addSaveOptionsAccessoryViewToSavePanel:") retVoid [argPtr (castPtr raw_savePanel :: Ptr ())]

-- | addSaveOptionsToView:
--
-- Adds IKSaveOptions UI to a NSView.
--
-- ObjC selector: @- addSaveOptionsToView:@
addSaveOptionsToView :: (IsIKSaveOptions ikSaveOptions, IsNSView view) => ikSaveOptions -> view -> IO ()
addSaveOptionsToView ikSaveOptions  view =
withObjCPtr view $ \raw_view ->
    sendMsg ikSaveOptions (mkSelector "addSaveOptionsToView:") retVoid [argPtr (castPtr raw_view :: Ptr ())]

-- | delegate
--
-- Delegate of the IKSaveOptions.
--
-- ObjC selector: @- delegate@
delegate :: IsIKSaveOptions ikSaveOptions => ikSaveOptions -> IO RawId
delegate ikSaveOptions  =
  fmap (RawId . castPtr) $ sendMsg ikSaveOptions (mkSelector "delegate") (retPtr retVoid) []

-- | delegate
--
-- Delegate of the IKSaveOptions.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsIKSaveOptions ikSaveOptions => ikSaveOptions -> RawId -> IO ()
setDelegate ikSaveOptions  value =
  sendMsg ikSaveOptions (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | imageProperties
--
-- current imageProperties (respecting user UI selection).
--
-- ObjC selector: @- imageProperties@
imageProperties :: IsIKSaveOptions ikSaveOptions => ikSaveOptions -> IO (Id NSDictionary)
imageProperties ikSaveOptions  =
  sendMsg ikSaveOptions (mkSelector "imageProperties") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | imageUTType
--
-- current imageUTType (respecting user UI selection).
--
-- ObjC selector: @- imageUTType@
imageUTType :: IsIKSaveOptions ikSaveOptions => ikSaveOptions -> IO (Id NSString)
imageUTType ikSaveOptions  =
  sendMsg ikSaveOptions (mkSelector "imageUTType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | userSelection
--
-- information about the UI settings.
--
-- ObjC selector: @- userSelection@
userSelection :: IsIKSaveOptions ikSaveOptions => ikSaveOptions -> IO (Id NSDictionary)
userSelection ikSaveOptions  =
  sendMsg ikSaveOptions (mkSelector "userSelection") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | rememberLastSetting
--
-- If set, the last used UI choices are preserved for the next time IKSaveOptions is used. [default is YES]
--
-- ObjC selector: @- rememberLastSetting@
rememberLastSetting :: IsIKSaveOptions ikSaveOptions => ikSaveOptions -> IO Bool
rememberLastSetting ikSaveOptions  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ikSaveOptions (mkSelector "rememberLastSetting") retCULong []

-- | rememberLastSetting
--
-- If set, the last used UI choices are preserved for the next time IKSaveOptions is used. [default is YES]
--
-- ObjC selector: @- setRememberLastSetting:@
setRememberLastSetting :: IsIKSaveOptions ikSaveOptions => ikSaveOptions -> Bool -> IO ()
setRememberLastSetting ikSaveOptions  value =
  sendMsg ikSaveOptions (mkSelector "setRememberLastSetting:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithImageProperties:imageUTType:@
initWithImageProperties_imageUTTypeSelector :: Selector
initWithImageProperties_imageUTTypeSelector = mkSelector "initWithImageProperties:imageUTType:"

-- | @Selector@ for @addSaveOptionsAccessoryViewToSavePanel:@
addSaveOptionsAccessoryViewToSavePanelSelector :: Selector
addSaveOptionsAccessoryViewToSavePanelSelector = mkSelector "addSaveOptionsAccessoryViewToSavePanel:"

-- | @Selector@ for @addSaveOptionsToView:@
addSaveOptionsToViewSelector :: Selector
addSaveOptionsToViewSelector = mkSelector "addSaveOptionsToView:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @imageProperties@
imagePropertiesSelector :: Selector
imagePropertiesSelector = mkSelector "imageProperties"

-- | @Selector@ for @imageUTType@
imageUTTypeSelector :: Selector
imageUTTypeSelector = mkSelector "imageUTType"

-- | @Selector@ for @userSelection@
userSelectionSelector :: Selector
userSelectionSelector = mkSelector "userSelection"

-- | @Selector@ for @rememberLastSetting@
rememberLastSettingSelector :: Selector
rememberLastSettingSelector = mkSelector "rememberLastSetting"

-- | @Selector@ for @setRememberLastSetting:@
setRememberLastSettingSelector :: Selector
setRememberLastSettingSelector = mkSelector "setRememberLastSetting:"

