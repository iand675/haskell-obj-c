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
  , initSelector
  , newSelector
  , endpointIDSelector


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

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRCluster mtrCluster => mtrCluster -> IO (Id MTRCluster)
init_ mtrCluster  =
    sendMsg mtrCluster (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRCluster)
new  =
  do
    cls' <- getRequiredClass "MTRCluster"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The endpoint this cluster lives on.
--
-- ObjC selector: @- endpointID@
endpointID :: IsMTRCluster mtrCluster => mtrCluster -> IO (Id NSNumber)
endpointID mtrCluster  =
    sendMsg mtrCluster (mkSelector "endpointID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector
endpointIDSelector = mkSelector "endpointID"

