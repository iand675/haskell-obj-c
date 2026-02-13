{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class representing a block in the Program.
--
-- Generated bindings for @MLModelStructureProgramBlock@.
module ObjC.CoreML.MLModelStructureProgramBlock
  ( MLModelStructureProgramBlock
  , IsMLModelStructureProgramBlock(..)
  , init_
  , new
  , inputs
  , outputNames
  , operations
  , initSelector
  , inputsSelector
  , newSelector
  , operationsSelector
  , outputNamesSelector


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
init_ :: IsMLModelStructureProgramBlock mlModelStructureProgramBlock => mlModelStructureProgramBlock -> IO (Id MLModelStructureProgramBlock)
init_ mlModelStructureProgramBlock =
  sendOwnedMessage mlModelStructureProgramBlock initSelector

-- | @+ new@
new :: IO (Id MLModelStructureProgramBlock)
new  =
  do
    cls' <- getRequiredClass "MLModelStructureProgramBlock"
    sendOwnedClassMessage cls' newSelector

-- | The named inputs to the block.
--
-- ObjC selector: @- inputs@
inputs :: IsMLModelStructureProgramBlock mlModelStructureProgramBlock => mlModelStructureProgramBlock -> IO (Id NSArray)
inputs mlModelStructureProgramBlock =
  sendMessage mlModelStructureProgramBlock inputsSelector

-- | The output names.
--
-- ObjC selector: @- outputNames@
outputNames :: IsMLModelStructureProgramBlock mlModelStructureProgramBlock => mlModelStructureProgramBlock -> IO (Id NSArray)
outputNames mlModelStructureProgramBlock =
  sendMessage mlModelStructureProgramBlock outputNamesSelector

-- | The list of topologically sorted operations in the block.
--
-- ObjC selector: @- operations@
operations :: IsMLModelStructureProgramBlock mlModelStructureProgramBlock => mlModelStructureProgramBlock -> IO (Id NSArray)
operations mlModelStructureProgramBlock =
  sendMessage mlModelStructureProgramBlock operationsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLModelStructureProgramBlock)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLModelStructureProgramBlock)
newSelector = mkSelector "new"

-- | @Selector@ for @inputs@
inputsSelector :: Selector '[] (Id NSArray)
inputsSelector = mkSelector "inputs"

-- | @Selector@ for @outputNames@
outputNamesSelector :: Selector '[] (Id NSArray)
outputNamesSelector = mkSelector "outputNames"

-- | @Selector@ for @operations@
operationsSelector :: Selector '[] (Id NSArray)
operationsSelector = mkSelector "operations"

