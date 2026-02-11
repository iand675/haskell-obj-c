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
  , variableStoreSelector
  , setVariableStoreSelector


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
init_ :: IsVZEFIBootLoader vzefiBootLoader => vzefiBootLoader -> IO (Id VZEFIBootLoader)
init_ vzefiBootLoader  =
  sendMsg vzefiBootLoader (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The EFI variable store.
--
-- ObjC selector: @- variableStore@
variableStore :: IsVZEFIBootLoader vzefiBootLoader => vzefiBootLoader -> IO (Id VZEFIVariableStore)
variableStore vzefiBootLoader  =
  sendMsg vzefiBootLoader (mkSelector "variableStore") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The EFI variable store.
--
-- ObjC selector: @- setVariableStore:@
setVariableStore :: (IsVZEFIBootLoader vzefiBootLoader, IsVZEFIVariableStore value) => vzefiBootLoader -> value -> IO ()
setVariableStore vzefiBootLoader  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzefiBootLoader (mkSelector "setVariableStore:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @variableStore@
variableStoreSelector :: Selector
variableStoreSelector = mkSelector "variableStore"

-- | @Selector@ for @setVariableStore:@
setVariableStoreSelector :: Selector
setVariableStoreSelector = mkSelector "setVariableStore:"

