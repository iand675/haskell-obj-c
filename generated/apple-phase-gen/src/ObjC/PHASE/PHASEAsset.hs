{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ***********************************************************************************************
--
-- PHASEAsset
--
-- An object that represents a registered asset in the asset registry.
--
-- Generated bindings for @PHASEAsset@.
module ObjC.PHASE.PHASEAsset
  ( PHASEAsset
  , IsPHASEAsset(..)
  , init_
  , new
  , identifier
  , identifierSelector
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
init_ :: IsPHASEAsset phaseAsset => phaseAsset -> IO (Id PHASEAsset)
init_ phaseAsset =
  sendOwnedMessage phaseAsset initSelector

-- | @+ new@
new :: IO (Id PHASEAsset)
new  =
  do
    cls' <- getRequiredClass "PHASEAsset"
    sendOwnedClassMessage cls' newSelector

-- | identifier
--
-- The identifier that uniquely represents this asset.
--
-- ObjC selector: @- identifier@
identifier :: IsPHASEAsset phaseAsset => phaseAsset -> IO (Id NSString)
identifier phaseAsset =
  sendMessage phaseAsset identifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEAsset)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEAsset)
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

