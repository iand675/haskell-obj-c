{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class representing the type of a value or a variable in the Program.
--
-- Generated bindings for @MLModelStructureProgramValueType@.
module ObjC.CoreML.MLModelStructureProgramValueType
  ( MLModelStructureProgramValueType
  , IsMLModelStructureProgramValueType(..)
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

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMLModelStructureProgramValueType mlModelStructureProgramValueType => mlModelStructureProgramValueType -> IO (Id MLModelStructureProgramValueType)
init_ mlModelStructureProgramValueType =
  sendOwnedMessage mlModelStructureProgramValueType initSelector

-- | @+ new@
new :: IO (Id MLModelStructureProgramValueType)
new  =
  do
    cls' <- getRequiredClass "MLModelStructureProgramValueType"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLModelStructureProgramValueType)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLModelStructureProgramValueType)
newSelector = mkSelector "new"

