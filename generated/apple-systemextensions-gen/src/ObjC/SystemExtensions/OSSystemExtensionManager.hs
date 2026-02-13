{-# LANGUAGE DataKinds #-}
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
  , sharedManager
  , initSelector
  , newSelector
  , sharedManagerSelector
  , submitRequestSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SystemExtensions.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsOSSystemExtensionManager osSystemExtensionManager => osSystemExtensionManager -> IO (Id OSSystemExtensionManager)
init_ osSystemExtensionManager =
  sendOwnedMessage osSystemExtensionManager initSelector

-- | @- new@
new :: IsOSSystemExtensionManager osSystemExtensionManager => osSystemExtensionManager -> IO (Id OSSystemExtensionManager)
new osSystemExtensionManager =
  sendOwnedMessage osSystemExtensionManager newSelector

-- | Submits a System Extension request to the manager.
--
-- @request@ — The request to process.
--
-- ObjC selector: @- submitRequest:@
submitRequest :: (IsOSSystemExtensionManager osSystemExtensionManager, IsOSSystemExtensionRequest request) => osSystemExtensionManager -> request -> IO ()
submitRequest osSystemExtensionManager request =
  sendMessage osSystemExtensionManager submitRequestSelector (toOSSystemExtensionRequest request)

-- | @+ sharedManager@
sharedManager :: IO (Id OSSystemExtensionManager)
sharedManager  =
  do
    cls' <- getRequiredClass "OSSystemExtensionManager"
    sendClassMessage cls' sharedManagerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id OSSystemExtensionManager)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id OSSystemExtensionManager)
newSelector = mkSelector "new"

-- | @Selector@ for @submitRequest:@
submitRequestSelector :: Selector '[Id OSSystemExtensionRequest] ()
submitRequestSelector = mkSelector "submitRequest:"

-- | @Selector@ for @sharedManager@
sharedManagerSelector :: Selector '[] (Id OSSystemExtensionManager)
sharedManagerSelector = mkSelector "sharedManager"

