{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An opaque type that describes the organization of the layer's textures and the relationships between those textures and the views you use for drawing.
--
-- You might use the layer's properties to configure other parts of your app. For example, use them to configure your app's render pipeline.
--
-- You can obtain layer properties directly from your layer. If you don't yet have the layer type, you can create an equivalent set of properties using the ``cp_layer_renderer_properties_create_using_configuration`` function.
--
-- Generated bindings for @CP_OBJECT_cp_layer_renderer_properties@.
module ObjC.CompositorServices.CP_OBJECT_cp_layer_renderer_properties
  ( CP_OBJECT_cp_layer_renderer_properties
  , IsCP_OBJECT_cp_layer_renderer_properties(..)
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
init_ :: IsCP_OBJECT_cp_layer_renderer_properties cP_OBJECT_cp_layer_renderer_properties => cP_OBJECT_cp_layer_renderer_properties -> IO (Id CP_OBJECT_cp_layer_renderer_properties)
init_ cP_OBJECT_cp_layer_renderer_properties =
  sendOwnedMessage cP_OBJECT_cp_layer_renderer_properties initSelector

-- | @+ new@
new :: IO (Id CP_OBJECT_cp_layer_renderer_properties)
new  =
  do
    cls' <- getRequiredClass "CP_OBJECT_cp_layer_renderer_properties"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CP_OBJECT_cp_layer_renderer_properties)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CP_OBJECT_cp_layer_renderer_properties)
newSelector = mkSelector "new"

