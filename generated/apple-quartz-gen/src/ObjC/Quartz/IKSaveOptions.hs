{-# LANGUAGE DataKinds #-}
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
  , addSaveOptionsAccessoryViewToSavePanelSelector
  , addSaveOptionsToViewSelector
  , delegateSelector
  , imagePropertiesSelector
  , imageUTTypeSelector
  , initWithImageProperties_imageUTTypeSelector
  , rememberLastSettingSelector
  , setDelegateSelector
  , setRememberLastSettingSelector
  , userSelectionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithImageProperties_imageUTType ikSaveOptions imageProperties imageUTType =
  sendOwnedMessage ikSaveOptions initWithImageProperties_imageUTTypeSelector (toNSDictionary imageProperties) (toNSString imageUTType)

-- | addSaveOptionsAccessoryViewToSavePanel:
--
-- Adds IKSaveOptions UI to a NSSavePanel.
--
-- ObjC selector: @- addSaveOptionsAccessoryViewToSavePanel:@
addSaveOptionsAccessoryViewToSavePanel :: (IsIKSaveOptions ikSaveOptions, IsNSSavePanel savePanel) => ikSaveOptions -> savePanel -> IO ()
addSaveOptionsAccessoryViewToSavePanel ikSaveOptions savePanel =
  sendMessage ikSaveOptions addSaveOptionsAccessoryViewToSavePanelSelector (toNSSavePanel savePanel)

-- | addSaveOptionsToView:
--
-- Adds IKSaveOptions UI to a NSView.
--
-- ObjC selector: @- addSaveOptionsToView:@
addSaveOptionsToView :: (IsIKSaveOptions ikSaveOptions, IsNSView view) => ikSaveOptions -> view -> IO ()
addSaveOptionsToView ikSaveOptions view =
  sendMessage ikSaveOptions addSaveOptionsToViewSelector (toNSView view)

-- | delegate
--
-- Delegate of the IKSaveOptions.
--
-- ObjC selector: @- delegate@
delegate :: IsIKSaveOptions ikSaveOptions => ikSaveOptions -> IO RawId
delegate ikSaveOptions =
  sendMessage ikSaveOptions delegateSelector

-- | delegate
--
-- Delegate of the IKSaveOptions.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsIKSaveOptions ikSaveOptions => ikSaveOptions -> RawId -> IO ()
setDelegate ikSaveOptions value =
  sendMessage ikSaveOptions setDelegateSelector value

-- | imageProperties
--
-- current imageProperties (respecting user UI selection).
--
-- ObjC selector: @- imageProperties@
imageProperties :: IsIKSaveOptions ikSaveOptions => ikSaveOptions -> IO (Id NSDictionary)
imageProperties ikSaveOptions =
  sendMessage ikSaveOptions imagePropertiesSelector

-- | imageUTType
--
-- current imageUTType (respecting user UI selection).
--
-- ObjC selector: @- imageUTType@
imageUTType :: IsIKSaveOptions ikSaveOptions => ikSaveOptions -> IO (Id NSString)
imageUTType ikSaveOptions =
  sendMessage ikSaveOptions imageUTTypeSelector

-- | userSelection
--
-- information about the UI settings.
--
-- ObjC selector: @- userSelection@
userSelection :: IsIKSaveOptions ikSaveOptions => ikSaveOptions -> IO (Id NSDictionary)
userSelection ikSaveOptions =
  sendMessage ikSaveOptions userSelectionSelector

-- | rememberLastSetting
--
-- If set, the last used UI choices are preserved for the next time IKSaveOptions is used. [default is YES]
--
-- ObjC selector: @- rememberLastSetting@
rememberLastSetting :: IsIKSaveOptions ikSaveOptions => ikSaveOptions -> IO Bool
rememberLastSetting ikSaveOptions =
  sendMessage ikSaveOptions rememberLastSettingSelector

-- | rememberLastSetting
--
-- If set, the last used UI choices are preserved for the next time IKSaveOptions is used. [default is YES]
--
-- ObjC selector: @- setRememberLastSetting:@
setRememberLastSetting :: IsIKSaveOptions ikSaveOptions => ikSaveOptions -> Bool -> IO ()
setRememberLastSetting ikSaveOptions value =
  sendMessage ikSaveOptions setRememberLastSettingSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithImageProperties:imageUTType:@
initWithImageProperties_imageUTTypeSelector :: Selector '[Id NSDictionary, Id NSString] (Id IKSaveOptions)
initWithImageProperties_imageUTTypeSelector = mkSelector "initWithImageProperties:imageUTType:"

-- | @Selector@ for @addSaveOptionsAccessoryViewToSavePanel:@
addSaveOptionsAccessoryViewToSavePanelSelector :: Selector '[Id NSSavePanel] ()
addSaveOptionsAccessoryViewToSavePanelSelector = mkSelector "addSaveOptionsAccessoryViewToSavePanel:"

-- | @Selector@ for @addSaveOptionsToView:@
addSaveOptionsToViewSelector :: Selector '[Id NSView] ()
addSaveOptionsToViewSelector = mkSelector "addSaveOptionsToView:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @imageProperties@
imagePropertiesSelector :: Selector '[] (Id NSDictionary)
imagePropertiesSelector = mkSelector "imageProperties"

-- | @Selector@ for @imageUTType@
imageUTTypeSelector :: Selector '[] (Id NSString)
imageUTTypeSelector = mkSelector "imageUTType"

-- | @Selector@ for @userSelection@
userSelectionSelector :: Selector '[] (Id NSDictionary)
userSelectionSelector = mkSelector "userSelection"

-- | @Selector@ for @rememberLastSetting@
rememberLastSettingSelector :: Selector '[] Bool
rememberLastSettingSelector = mkSelector "rememberLastSetting"

-- | @Selector@ for @setRememberLastSetting:@
setRememberLastSettingSelector :: Selector '[Bool] ()
setRememberLastSettingSelector = mkSelector "setRememberLastSetting:"

