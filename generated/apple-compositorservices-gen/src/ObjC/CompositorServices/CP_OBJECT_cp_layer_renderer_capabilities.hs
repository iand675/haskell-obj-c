{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A type that stores the supported configurations for a layer.
--
-- Generated bindings for @CP_OBJECT_cp_layer_renderer_capabilities@.
module ObjC.CompositorServices.CP_OBJECT_cp_layer_renderer_capabilities
  ( CP_OBJECT_cp_layer_renderer_capabilities
  , IsCP_OBJECT_cp_layer_renderer_capabilities(..)
  , init_
  , new
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CompositorServices.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCP_OBJECT_cp_layer_renderer_capabilities cP_OBJECT_cp_layer_renderer_capabilities => cP_OBJECT_cp_layer_renderer_capabilities -> IO (Id CP_OBJECT_cp_layer_renderer_capabilities)
init_ cP_OBJECT_cp_layer_renderer_capabilities =
  sendOwnedMessage cP_OBJECT_cp_layer_renderer_capabilities initSelector

-- | @+ new@
new :: IO (Id CP_OBJECT_cp_layer_renderer_capabilities)
new  =
  do
    cls' <- getRequiredClass "CP_OBJECT_cp_layer_renderer_capabilities"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CP_OBJECT_cp_layer_renderer_capabilities)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CP_OBJECT_cp_layer_renderer_capabilities)
newSelector = mkSelector "new"

