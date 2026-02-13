{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEDefinition
--
-- The base class for a definition.
--
-- Contains an identifer that uniquely represents this definition.
--
-- Generated bindings for @PHASEDefinition@.
module ObjC.PHASE.PHASEDefinition
  ( PHASEDefinition
  , IsPHASEDefinition(..)
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
init_ :: IsPHASEDefinition phaseDefinition => phaseDefinition -> IO (Id PHASEDefinition)
init_ phaseDefinition =
  sendOwnedMessage phaseDefinition initSelector

-- | @+ new@
new :: IO (Id PHASEDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASEDefinition"
    sendOwnedClassMessage cls' newSelector

-- | identifier
--
-- The identifier that uniquely represents this definition.
--
-- ObjC selector: @- identifier@
identifier :: IsPHASEDefinition phaseDefinition => phaseDefinition -> IO (Id NSString)
identifier phaseDefinition =
  sendMessage phaseDefinition identifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEDefinition)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEDefinition)
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

