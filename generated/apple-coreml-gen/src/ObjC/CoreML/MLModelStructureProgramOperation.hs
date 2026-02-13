{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class representing an Operation in a Program.
--
-- Generated bindings for @MLModelStructureProgramOperation@.
module ObjC.CoreML.MLModelStructureProgramOperation
  ( MLModelStructureProgramOperation
  , IsMLModelStructureProgramOperation(..)
  , init_
  , new
  , operatorName
  , inputs
  , outputs
  , blocks
  , blocksSelector
  , initSelector
  , inputsSelector
  , newSelector
  , operatorNameSelector
  , outputsSelector


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
init_ :: IsMLModelStructureProgramOperation mlModelStructureProgramOperation => mlModelStructureProgramOperation -> IO (Id MLModelStructureProgramOperation)
init_ mlModelStructureProgramOperation =
  sendOwnedMessage mlModelStructureProgramOperation initSelector

-- | @+ new@
new :: IO (Id MLModelStructureProgramOperation)
new  =
  do
    cls' <- getRequiredClass "MLModelStructureProgramOperation"
    sendOwnedClassMessage cls' newSelector

-- | The name of the operator, e.g., "conv", "pool", "softmax", etc.
--
-- ObjC selector: @- operatorName@
operatorName :: IsMLModelStructureProgramOperation mlModelStructureProgramOperation => mlModelStructureProgramOperation -> IO (Id NSString)
operatorName mlModelStructureProgramOperation =
  sendMessage mlModelStructureProgramOperation operatorNameSelector

-- | The arguments to the Operation.
--
-- ObjC selector: @- inputs@
inputs :: IsMLModelStructureProgramOperation mlModelStructureProgramOperation => mlModelStructureProgramOperation -> IO (Id NSDictionary)
inputs mlModelStructureProgramOperation =
  sendMessage mlModelStructureProgramOperation inputsSelector

-- | The outputs of the Operation.
--
-- ObjC selector: @- outputs@
outputs :: IsMLModelStructureProgramOperation mlModelStructureProgramOperation => mlModelStructureProgramOperation -> IO (Id NSArray)
outputs mlModelStructureProgramOperation =
  sendMessage mlModelStructureProgramOperation outputsSelector

-- | Nested blocks for loops and conditionals, e.g., a conditional block will have two entries here.
--
-- ObjC selector: @- blocks@
blocks :: IsMLModelStructureProgramOperation mlModelStructureProgramOperation => mlModelStructureProgramOperation -> IO (Id NSArray)
blocks mlModelStructureProgramOperation =
  sendMessage mlModelStructureProgramOperation blocksSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLModelStructureProgramOperation)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLModelStructureProgramOperation)
newSelector = mkSelector "new"

-- | @Selector@ for @operatorName@
operatorNameSelector :: Selector '[] (Id NSString)
operatorNameSelector = mkSelector "operatorName"

-- | @Selector@ for @inputs@
inputsSelector :: Selector '[] (Id NSDictionary)
inputsSelector = mkSelector "inputs"

-- | @Selector@ for @outputs@
outputsSelector :: Selector '[] (Id NSArray)
outputsSelector = mkSelector "outputs"

-- | @Selector@ for @blocks@
blocksSelector :: Selector '[] (Id NSArray)
blocksSelector = mkSelector "blocks"

