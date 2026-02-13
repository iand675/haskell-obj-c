{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEDirectivityModelParameters
--
-- Directivity model parameters.
--
-- Generated bindings for @PHASEDirectivityModelParameters@.
module ObjC.PHASE.PHASEDirectivityModelParameters
  ( PHASEDirectivityModelParameters
  , IsPHASEDirectivityModelParameters(..)
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
init_ :: IsPHASEDirectivityModelParameters phaseDirectivityModelParameters => phaseDirectivityModelParameters -> IO (Id PHASEDirectivityModelParameters)
init_ phaseDirectivityModelParameters =
  sendOwnedMessage phaseDirectivityModelParameters initSelector

-- | @+ new@
new :: IO (Id PHASEDirectivityModelParameters)
new  =
  do
    cls' <- getRequiredClass "PHASEDirectivityModelParameters"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEDirectivityModelParameters)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEDirectivityModelParameters)
newSelector = mkSelector "new"

