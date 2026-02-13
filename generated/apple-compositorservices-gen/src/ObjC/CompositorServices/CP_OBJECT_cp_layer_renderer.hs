{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An opaque type that provides the Metal types and timing information you need to draw your content.
--
-- Generated bindings for @CP_OBJECT_cp_layer_renderer@.
module ObjC.CompositorServices.CP_OBJECT_cp_layer_renderer
  ( CP_OBJECT_cp_layer_renderer
  , IsCP_OBJECT_cp_layer_renderer(..)
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
init_ :: IsCP_OBJECT_cp_layer_renderer cP_OBJECT_cp_layer_renderer => cP_OBJECT_cp_layer_renderer -> IO (Id CP_OBJECT_cp_layer_renderer)
init_ cP_OBJECT_cp_layer_renderer =
  sendOwnedMessage cP_OBJECT_cp_layer_renderer initSelector

-- | @+ new@
new :: IO (Id CP_OBJECT_cp_layer_renderer)
new  =
  do
    cls' <- getRequiredClass "CP_OBJECT_cp_layer_renderer"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CP_OBJECT_cp_layer_renderer)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CP_OBJECT_cp_layer_renderer)
newSelector = mkSelector "new"

