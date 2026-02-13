{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The platform configuration for a generic Intel or ARM virtual machine.
--
-- When a virtual machine is saved to disk then loaded again, the @machineIdentifier@    must be restored to the original value.
--
-- If multiple virtual machines are created from the same configuration, each should have a unique @machineIdentifier@.
--
-- Generated bindings for @VZGenericPlatformConfiguration@.
module ObjC.Virtualization.VZGenericPlatformConfiguration
  ( VZGenericPlatformConfiguration
  , IsVZGenericPlatformConfiguration(..)
  , init_
  , machineIdentifier
  , setMachineIdentifier
  , nestedVirtualizationSupported
  , nestedVirtualizationEnabled
  , setNestedVirtualizationEnabled
  , initSelector
  , machineIdentifierSelector
  , nestedVirtualizationEnabledSelector
  , nestedVirtualizationSupportedSelector
  , setMachineIdentifierSelector
  , setNestedVirtualizationEnabledSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVZGenericPlatformConfiguration vzGenericPlatformConfiguration => vzGenericPlatformConfiguration -> IO (Id VZGenericPlatformConfiguration)
init_ vzGenericPlatformConfiguration =
  sendOwnedMessage vzGenericPlatformConfiguration initSelector

-- | The unique machine identifier.
--
-- Running two virtual machines concurrently with the same identifier results in undefined behavior    in the guest operating system. When restoring a virtual machine from saved state, this    @machineIdentifier@ must match the @machineIdentifier@ of the saved virtual machine.
--
-- ObjC selector: @- machineIdentifier@
machineIdentifier :: IsVZGenericPlatformConfiguration vzGenericPlatformConfiguration => vzGenericPlatformConfiguration -> IO (Id VZGenericMachineIdentifier)
machineIdentifier vzGenericPlatformConfiguration =
  sendMessage vzGenericPlatformConfiguration machineIdentifierSelector

-- | The unique machine identifier.
--
-- Running two virtual machines concurrently with the same identifier results in undefined behavior    in the guest operating system. When restoring a virtual machine from saved state, this    @machineIdentifier@ must match the @machineIdentifier@ of the saved virtual machine.
--
-- ObjC selector: @- setMachineIdentifier:@
setMachineIdentifier :: (IsVZGenericPlatformConfiguration vzGenericPlatformConfiguration, IsVZGenericMachineIdentifier value) => vzGenericPlatformConfiguration -> value -> IO ()
setMachineIdentifier vzGenericPlatformConfiguration value =
  sendMessage vzGenericPlatformConfiguration setMachineIdentifierSelector (toVZGenericMachineIdentifier value)

-- | Indicate whether or not nested virtualization is available.
--
-- Nested virtualization is only available on some hardware and software configurations. It may also be disabled by policy.
--
-- Use this property to check if support is available for the platform. If nested virtualization is supported,    use @nestedVirtualizationEnabled@ to enable the feature.
--
-- See: nestedVirtualizationEnabled.
--
-- ObjC selector: @+ nestedVirtualizationSupported@
nestedVirtualizationSupported :: IO Bool
nestedVirtualizationSupported  =
  do
    cls' <- getRequiredClass "VZGenericPlatformConfiguration"
    sendClassMessage cls' nestedVirtualizationSupportedSelector

-- | Enable nested virtualization for the platform.
--
-- If nested virtualization is available, enable it for the current platform configuration.
--
-- You can use @nestedVirtualizationSupported@ to discover the nested virtualization availability before enabling it.
--
-- The default value is NO, nested virtualization is disabled.
--
-- See: nestedVirtualizationSupported.
--
-- ObjC selector: @- nestedVirtualizationEnabled@
nestedVirtualizationEnabled :: IsVZGenericPlatformConfiguration vzGenericPlatformConfiguration => vzGenericPlatformConfiguration -> IO Bool
nestedVirtualizationEnabled vzGenericPlatformConfiguration =
  sendMessage vzGenericPlatformConfiguration nestedVirtualizationEnabledSelector

-- | Enable nested virtualization for the platform.
--
-- If nested virtualization is available, enable it for the current platform configuration.
--
-- You can use @nestedVirtualizationSupported@ to discover the nested virtualization availability before enabling it.
--
-- The default value is NO, nested virtualization is disabled.
--
-- See: nestedVirtualizationSupported.
--
-- ObjC selector: @- setNestedVirtualizationEnabled:@
setNestedVirtualizationEnabled :: IsVZGenericPlatformConfiguration vzGenericPlatformConfiguration => vzGenericPlatformConfiguration -> Bool -> IO ()
setNestedVirtualizationEnabled vzGenericPlatformConfiguration value =
  sendMessage vzGenericPlatformConfiguration setNestedVirtualizationEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZGenericPlatformConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @machineIdentifier@
machineIdentifierSelector :: Selector '[] (Id VZGenericMachineIdentifier)
machineIdentifierSelector = mkSelector "machineIdentifier"

-- | @Selector@ for @setMachineIdentifier:@
setMachineIdentifierSelector :: Selector '[Id VZGenericMachineIdentifier] ()
setMachineIdentifierSelector = mkSelector "setMachineIdentifier:"

-- | @Selector@ for @nestedVirtualizationSupported@
nestedVirtualizationSupportedSelector :: Selector '[] Bool
nestedVirtualizationSupportedSelector = mkSelector "nestedVirtualizationSupported"

-- | @Selector@ for @nestedVirtualizationEnabled@
nestedVirtualizationEnabledSelector :: Selector '[] Bool
nestedVirtualizationEnabledSelector = mkSelector "nestedVirtualizationEnabled"

-- | @Selector@ for @setNestedVirtualizationEnabled:@
setNestedVirtualizationEnabledSelector :: Selector '[Bool] ()
setNestedVirtualizationEnabledSelector = mkSelector "setNestedVirtualizationEnabled:"

