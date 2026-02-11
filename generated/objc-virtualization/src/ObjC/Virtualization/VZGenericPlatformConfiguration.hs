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
  , nestedVirtualizationSupported
  , nestedVirtualizationEnabled
  , setNestedVirtualizationEnabled
  , initSelector
  , nestedVirtualizationSupportedSelector
  , nestedVirtualizationEnabledSelector
  , setNestedVirtualizationEnabledSelector


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

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVZGenericPlatformConfiguration vzGenericPlatformConfiguration => vzGenericPlatformConfiguration -> IO (Id VZGenericPlatformConfiguration)
init_ vzGenericPlatformConfiguration  =
  sendMsg vzGenericPlatformConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "nestedVirtualizationSupported") retCULong []

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
nestedVirtualizationEnabled vzGenericPlatformConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzGenericPlatformConfiguration (mkSelector "nestedVirtualizationEnabled") retCULong []

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
setNestedVirtualizationEnabled vzGenericPlatformConfiguration  value =
  sendMsg vzGenericPlatformConfiguration (mkSelector "setNestedVirtualizationEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @nestedVirtualizationSupported@
nestedVirtualizationSupportedSelector :: Selector
nestedVirtualizationSupportedSelector = mkSelector "nestedVirtualizationSupported"

-- | @Selector@ for @nestedVirtualizationEnabled@
nestedVirtualizationEnabledSelector :: Selector
nestedVirtualizationEnabledSelector = mkSelector "nestedVirtualizationEnabled"

-- | @Selector@ for @setNestedVirtualizationEnabled:@
setNestedVirtualizationEnabledSelector :: Selector
setNestedVirtualizationEnabledSelector = mkSelector "setNestedVirtualizationEnabled:"

