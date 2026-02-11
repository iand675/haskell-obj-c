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

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEGlobalMetaParameterAsset phaseGlobalMetaParameterAsset => phaseGlobalMetaParameterAsset -> IO (Id PHASEGlobalMetaParameterAsset)
init_ phaseGlobalMetaParameterAsset  =
  sendMsg phaseGlobalMetaParameterAsset (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEGlobalMetaParameterAsset)
new  =
  do
    cls' <- getRequiredClass "PHASEGlobalMetaParameterAsset"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

