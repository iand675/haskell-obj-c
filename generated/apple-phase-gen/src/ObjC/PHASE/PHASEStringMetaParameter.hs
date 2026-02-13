{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEStringMetaParameter
--
-- An object that represents an active string metaparameter in the system
--
-- Generated bindings for @PHASEStringMetaParameter@.
module ObjC.PHASE.PHASEStringMetaParameter
  ( PHASEStringMetaParameter
  , IsPHASEStringMetaParameter(..)
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
init_ :: IsPHASEStringMetaParameter phaseStringMetaParameter => phaseStringMetaParameter -> IO (Id PHASEStringMetaParameter)
init_ phaseStringMetaParameter =
  sendOwnedMessage phaseStringMetaParameter initSelector

-- | @+ new@
new :: IO (Id PHASEStringMetaParameter)
new  =
  do
    cls' <- getRequiredClass "PHASEStringMetaParameter"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEStringMetaParameter)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEStringMetaParameter)
newSelector = mkSelector "new"

