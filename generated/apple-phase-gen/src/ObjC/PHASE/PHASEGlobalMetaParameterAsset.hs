{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEGlobalMetaParameterAsset
--
-- An object that represents a registered global metaparameter asset in the asset registry.
--
-- Generated bindings for @PHASEGlobalMetaParameterAsset@.
module ObjC.PHASE.PHASEGlobalMetaParameterAsset
  ( PHASEGlobalMetaParameterAsset
  , IsPHASEGlobalMetaParameterAsset(..)
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

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEGlobalMetaParameterAsset phaseGlobalMetaParameterAsset => phaseGlobalMetaParameterAsset -> IO (Id PHASEGlobalMetaParameterAsset)
init_ phaseGlobalMetaParameterAsset =
  sendOwnedMessage phaseGlobalMetaParameterAsset initSelector

-- | @+ new@
new :: IO (Id PHASEGlobalMetaParameterAsset)
new  =
  do
    cls' <- getRequiredClass "PHASEGlobalMetaParameterAsset"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEGlobalMetaParameterAsset)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEGlobalMetaParameterAsset)
newSelector = mkSelector "new"

