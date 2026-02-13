{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASESoundEventNodeAsset
--
-- An object that represents a registered sound event asset in the asset registry.
--
-- Generated bindings for @PHASESoundEventNodeAsset@.
module ObjC.PHASE.PHASESoundEventNodeAsset
  ( PHASESoundEventNodeAsset
  , IsPHASESoundEventNodeAsset(..)
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
init_ :: IsPHASESoundEventNodeAsset phaseSoundEventNodeAsset => phaseSoundEventNodeAsset -> IO (Id PHASESoundEventNodeAsset)
init_ phaseSoundEventNodeAsset =
  sendOwnedMessage phaseSoundEventNodeAsset initSelector

-- | @+ new@
new :: IO (Id PHASESoundEventNodeAsset)
new  =
  do
    cls' <- getRequiredClass "PHASESoundEventNodeAsset"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASESoundEventNodeAsset)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASESoundEventNodeAsset)
newSelector = mkSelector "new"

