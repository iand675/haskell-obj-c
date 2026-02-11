{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @OSSystemExtensionManager@.
module ObjC.SystemExtensions.OSSystemExtensionManager
  ( OSSystemExtensionManager
  , IsOSSystemExtensionManager(..)
  , init_
  , new
  , submitRequest
  , initSelector
  , newSelector
  , submitRequestSelector


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

import ObjC.SystemExtensions.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsOSSystemExtensionManager osSystemExtensionManager => osSystemExtensionManager -> IO (Id OSSystemExtensionManager)
init_ osSystemExtensionManager  =
  sendMsg osSystemExtensionManager (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- new@
new :: IsOSSystemExtensionManager osSystemExtensionManager => osSystemExtensionManager -> IO (Id OSSystemExtensionManager)
new osSystemExtensionManager  =
  sendMsg osSystemExtensionManager (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Submits a System Extension request to the manager.
--
-- @request@ — The request to process.
--
-- ObjC selector: @- submitRequest:@
submitRequest :: (IsOSSystemExtensionManager osSystemExtensionManager, IsOSSystemExtensionRequest request) => osSystemExtensionManager -> request -> IO ()
submitRequest osSystemExtensionManager  request =
withObjCPtr request $ \raw_request ->
    sendMsg osSystemExtensionManager (mkSelector "submitRequest:") retVoid [argPtr (castPtr raw_request :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @submitRequest:@
submitRequestSelector :: Selector
submitRequestSelector = mkSelector "submitRequest:"

