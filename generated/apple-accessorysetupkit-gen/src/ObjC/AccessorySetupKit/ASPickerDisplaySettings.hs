{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A type that contains settings to customize the display of the accessory picker
--
-- Generated bindings for @ASPickerDisplaySettings@.
module ObjC.AccessorySetupKit.ASPickerDisplaySettings
  ( ASPickerDisplaySettings
  , IsASPickerDisplaySettings(..)
  , defaultSettings
  , discoveryTimeout
  , setDiscoveryTimeout
  , options
  , setOptions
  , defaultSettingsSelector
  , discoveryTimeoutSelector
  , optionsSelector
  , setDiscoveryTimeoutSelector
  , setOptionsSelector

  -- * Enum types
  , ASPickerDisplaySettingsOptions(ASPickerDisplaySettingsOptions)
  , pattern ASPickerDisplaySettingsOptionFilterDiscoveryResults

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AccessorySetupKit.Internal.Classes
import ObjC.AccessorySetupKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | An empty settings object.
--
-- ObjC selector: @+ defaultSettings@
defaultSettings :: IO (Id ASPickerDisplaySettings)
defaultSettings  =
  do
    cls' <- getRequiredClass "ASPickerDisplaySettings"
    sendClassMessage cls' defaultSettingsSelector

-- | Custom timeout for picker. Default is 30 seconds.
--
-- ObjC selector: @- discoveryTimeout@
discoveryTimeout :: IsASPickerDisplaySettings asPickerDisplaySettings => asPickerDisplaySettings -> IO CDouble
discoveryTimeout asPickerDisplaySettings =
  sendMessage asPickerDisplaySettings discoveryTimeoutSelector

-- | Custom timeout for picker. Default is 30 seconds.
--
-- ObjC selector: @- setDiscoveryTimeout:@
setDiscoveryTimeout :: IsASPickerDisplaySettings asPickerDisplaySettings => asPickerDisplaySettings -> CDouble -> IO ()
setDiscoveryTimeout asPickerDisplaySettings value =
  sendMessage asPickerDisplaySettings setDiscoveryTimeoutSelector value

-- | Custom options for the picker.
--
-- ObjC selector: @- options@
options :: IsASPickerDisplaySettings asPickerDisplaySettings => asPickerDisplaySettings -> IO ASPickerDisplaySettingsOptions
options asPickerDisplaySettings =
  sendMessage asPickerDisplaySettings optionsSelector

-- | Custom options for the picker.
--
-- ObjC selector: @- setOptions:@
setOptions :: IsASPickerDisplaySettings asPickerDisplaySettings => asPickerDisplaySettings -> ASPickerDisplaySettingsOptions -> IO ()
setOptions asPickerDisplaySettings value =
  sendMessage asPickerDisplaySettings setOptionsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultSettings@
defaultSettingsSelector :: Selector '[] (Id ASPickerDisplaySettings)
defaultSettingsSelector = mkSelector "defaultSettings"

-- | @Selector@ for @discoveryTimeout@
discoveryTimeoutSelector :: Selector '[] CDouble
discoveryTimeoutSelector = mkSelector "discoveryTimeout"

-- | @Selector@ for @setDiscoveryTimeout:@
setDiscoveryTimeoutSelector :: Selector '[CDouble] ()
setDiscoveryTimeoutSelector = mkSelector "setDiscoveryTimeout:"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] ASPickerDisplaySettingsOptions
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector '[ASPickerDisplaySettingsOptions] ()
setOptionsSelector = mkSelector "setOptions:"

