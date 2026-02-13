{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class representing the structure of an ML Program model.
--
-- Generated bindings for @MLModelStructureProgram@.
module ObjC.CoreML.MLModelStructureProgram
  ( MLModelStructureProgram
  , IsMLModelStructureProgram(..)
  , init_
  , new
  , functions
  , functionsSelector
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
init_ :: IsMLModelStructureProgram mlModelStructureProgram => mlModelStructureProgram -> IO (Id MLModelStructureProgram)
init_ mlModelStructureProgram =
  sendOwnedMessage mlModelStructureProgram initSelector

-- | @+ new@
new :: IO (Id MLModelStructureProgram)
new  =
  do
    cls' <- getRequiredClass "MLModelStructureProgram"
    sendOwnedClassMessage cls' newSelector

-- | The functions in the program.
--
-- ObjC selector: @- functions@
functions :: IsMLModelStructureProgram mlModelStructureProgram => mlModelStructureProgram -> IO (Id NSDictionary)
functions mlModelStructureProgram =
  sendMessage mlModelStructureProgram functionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLModelStructureProgram)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLModelStructureProgram)
newSelector = mkSelector "new"

-- | @Selector@ for @functions@
functionsSelector :: Selector '[] (Id NSDictionary)
functionsSelector = mkSelector "functions"

