{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Boot loader configuration for booting guest operating systems expecting an EFI ROM.
--
-- You must use a VZGenericPlatformConfiguration in conjunction with the EFI boot loader.    It is invalid to use it with any other platform configuration.
--
-- See: VZGenericPlatformConfiguration
--
-- See: VZVirtualMachineConfiguration.platform.
--
-- Generated bindings for @VZEFIBootLoader@.
module ObjC.Virtualization.VZEFIBootLoader
  ( VZEFIBootLoader
  , IsVZEFIBootLoader(..)
  , init_
  , variableStore
  , setVariableStore
  , initSelector
  , setVariableStoreSelector
  , variableStoreSelector


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
init_ :: IsVZEFIBootLoader vzefiBootLoader => vzefiBootLoader -> IO (Id VZEFIBootLoader)
init_ vzefiBootLoader =
  sendOwnedMessage vzefiBootLoader initSelector

-- | The EFI variable store.
--
-- ObjC selector: @- variableStore@
variableStore :: IsVZEFIBootLoader vzefiBootLoader => vzefiBootLoader -> IO (Id VZEFIVariableStore)
variableStore vzefiBootLoader =
  sendMessage vzefiBootLoader variableStoreSelector

-- | The EFI variable store.
--
-- ObjC selector: @- setVariableStore:@
setVariableStore :: (IsVZEFIBootLoader vzefiBootLoader, IsVZEFIVariableStore value) => vzefiBootLoader -> value -> IO ()
setVariableStore vzefiBootLoader value =
  sendMessage vzefiBootLoader setVariableStoreSelector (toVZEFIVariableStore value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZEFIBootLoader)
initSelector = mkSelector "init"

-- | @Selector@ for @variableStore@
variableStoreSelector :: Selector '[] (Id VZEFIVariableStore)
variableStoreSelector = mkSelector "variableStore"

-- | @Selector@ for @setVariableStore:@
setVariableStoreSelector :: Selector '[Id VZEFIVariableStore] ()
setVariableStoreSelector = mkSelector "setVariableStore:"

