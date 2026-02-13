{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEMetaParameterDefinition
--
-- A base object for metaparameter definitions
--
-- Generated bindings for @PHASEMetaParameterDefinition@.
module ObjC.PHASE.PHASEMetaParameterDefinition
  ( PHASEMetaParameterDefinition
  , IsPHASEMetaParameterDefinition(..)
  , init_
  , new
  , value
  , initSelector
  , newSelector
  , valueSelector


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
init_ :: IsPHASEMetaParameterDefinition phaseMetaParameterDefinition => phaseMetaParameterDefinition -> IO (Id PHASEMetaParameterDefinition)
init_ phaseMetaParameterDefinition =
  sendOwnedMessage phaseMetaParameterDefinition initSelector

-- | @+ new@
new :: IO (Id PHASEMetaParameterDefinition)
new  =
  do
    cls' <- getRequiredClass "PHASEMetaParameterDefinition"
    sendOwnedClassMessage cls' newSelector

-- | value
--
-- The value of the metaparameter.
--
-- ObjC selector: @- value@
value :: IsPHASEMetaParameterDefinition phaseMetaParameterDefinition => phaseMetaParameterDefinition -> IO RawId
value phaseMetaParameterDefinition =
  sendMessage phaseMetaParameterDefinition valueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEMetaParameterDefinition)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEMetaParameterDefinition)
newSelector = mkSelector "new"

-- | @Selector@ for @value@
valueSelector :: Selector '[] RawId
valueSelector = mkSelector "value"

