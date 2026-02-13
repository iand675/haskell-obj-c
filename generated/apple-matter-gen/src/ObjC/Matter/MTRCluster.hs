{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTRCluster    This is the base class for clusters.
--
-- Generated bindings for @MTRCluster@.
module ObjC.Matter.MTRCluster
  ( MTRCluster
  , IsMTRCluster(..)
  , init_
  , new
  , endpointID
  , endpointIDSelector
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRCluster mtrCluster => mtrCluster -> IO (Id MTRCluster)
init_ mtrCluster =
  sendOwnedMessage mtrCluster initSelector

-- | @+ new@
new :: IO (Id MTRCluster)
new  =
  do
    cls' <- getRequiredClass "MTRCluster"
    sendOwnedClassMessage cls' newSelector

-- | The endpoint this cluster lives on.
--
-- ObjC selector: @- endpointID@
endpointID :: IsMTRCluster mtrCluster => mtrCluster -> IO (Id NSNumber)
endpointID mtrCluster =
  sendMessage mtrCluster endpointIDSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRCluster)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRCluster)
newSelector = mkSelector "new"

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector '[] (Id NSNumber)
endpointIDSelector = mkSelector "endpointID"

