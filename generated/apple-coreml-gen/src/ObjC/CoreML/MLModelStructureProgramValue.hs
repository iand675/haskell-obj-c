{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class representing a constant value in the Program.
--
-- Generated bindings for @MLModelStructureProgramValue@.
module ObjC.CoreML.MLModelStructureProgramValue
  ( MLModelStructureProgramValue
  , IsMLModelStructureProgramValue(..)
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
init_ :: IsMLModelStructureProgramValue mlModelStructureProgramValue => mlModelStructureProgramValue -> IO (Id MLModelStructureProgramValue)
init_ mlModelStructureProgramValue =
  sendOwnedMessage mlModelStructureProgramValue initSelector

-- | @+ new@
new :: IO (Id MLModelStructureProgramValue)
new  =
  do
    cls' <- getRequiredClass "MLModelStructureProgramValue"
    sendOwnedClassMessage cls' newSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLModelStructureProgramValue)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLModelStructureProgramValue)
newSelector = mkSelector "new"

