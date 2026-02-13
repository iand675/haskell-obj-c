{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class representing a function in the Program.
--
-- Generated bindings for @MLModelStructureProgramFunction@.
module ObjC.CoreML.MLModelStructureProgramFunction
  ( MLModelStructureProgramFunction
  , IsMLModelStructureProgramFunction(..)
  , init_
  , new
  , inputs
  , block
  , blockSelector
  , initSelector
  , inputsSelector
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
init_ :: IsMLModelStructureProgramFunction mlModelStructureProgramFunction => mlModelStructureProgramFunction -> IO (Id MLModelStructureProgramFunction)
init_ mlModelStructureProgramFunction =
  sendOwnedMessage mlModelStructureProgramFunction initSelector

-- | @+ new@
new :: IO (Id MLModelStructureProgramFunction)
new  =
  do
    cls' <- getRequiredClass "MLModelStructureProgramFunction"
    sendOwnedClassMessage cls' newSelector

-- | The named inputs to the function.
--
-- ObjC selector: @- inputs@
inputs :: IsMLModelStructureProgramFunction mlModelStructureProgramFunction => mlModelStructureProgramFunction -> IO (Id NSArray)
inputs mlModelStructureProgramFunction =
  sendMessage mlModelStructureProgramFunction inputsSelector

-- | The active block in the function.
--
-- ObjC selector: @- block@
block :: IsMLModelStructureProgramFunction mlModelStructureProgramFunction => mlModelStructureProgramFunction -> IO (Id MLModelStructureProgramBlock)
block mlModelStructureProgramFunction =
  sendMessage mlModelStructureProgramFunction blockSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLModelStructureProgramFunction)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLModelStructureProgramFunction)
newSelector = mkSelector "new"

-- | @Selector@ for @inputs@
inputsSelector :: Selector '[] (Id NSArray)
inputsSelector = mkSelector "inputs"

-- | @Selector@ for @block@
blockSelector :: Selector '[] (Id MLModelStructureProgramBlock)
blockSelector = mkSelector "block"

