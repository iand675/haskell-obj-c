{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An opaque type that stores the settings to apply to a Compositor layer renderer.
--
-- You don’t create this type directly. If your ``CompositorLayer`` uses a custom ``CompositorLayerConfiguration``, the compositor layer creates an instance of this type and passes it to the provider’s ``CompositorLayerConfiguration/makeConfiguration(capabilities:configuration:)`` function. Use that instance to modify the default configuration settings for your layer.
--
-- Generated bindings for @CP_OBJECT_cp_layer_renderer_configuration@.
module ObjC.CompositorServices.CP_OBJECT_cp_layer_renderer_configuration
  ( CP_OBJECT_cp_layer_renderer_configuration
  , IsCP_OBJECT_cp_layer_renderer_configuration(..)
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
init_ :: IsCP_OBJECT_cp_layer_renderer_configuration cP_OBJECT_cp_layer_renderer_configuration => cP_OBJECT_cp_layer_renderer_configuration -> IO (Id CP_OBJECT_cp_layer_renderer_configuration)
init_ cP_OBJECT_cp_layer_renderer_configuration =
  sendOwnedMessage cP_OBJECT_cp_layer_renderer_configuration initSelector

-- | @+ new@
new :: IO (Id CP_OBJECT_cp_layer_renderer_configuration)
new  =
  do
    cls' <- getRequiredClass "CP_OBJECT_cp_layer_renderer_configuration"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CP_OBJECT_cp_layer_renderer_configuration)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CP_OBJECT_cp_layer_renderer_configuration)
newSelector = mkSelector "new"

