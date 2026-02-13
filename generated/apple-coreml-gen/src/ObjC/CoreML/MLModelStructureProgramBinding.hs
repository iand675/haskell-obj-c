{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class representing a binding in the Program
--
-- A Binding is either a previously defined name of a variable or a constant value in the Program.
--
-- Generated bindings for @MLModelStructureProgramBinding@.
module ObjC.CoreML.MLModelStructureProgramBinding
  ( MLModelStructureProgramBinding
  , IsMLModelStructureProgramBinding(..)
  , init_
  , new
  , name
  , value
  , initSelector
  , nameSelector
  , newSelector
  , valueSelector


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
init_ :: IsMLModelStructureProgramBinding mlModelStructureProgramBinding => mlModelStructureProgramBinding -> IO (Id MLModelStructureProgramBinding)
init_ mlModelStructureProgramBinding =
  sendOwnedMessage mlModelStructureProgramBinding initSelector

-- | @+ new@
new :: IO (Id MLModelStructureProgramBinding)
new  =
  do
    cls' <- getRequiredClass "MLModelStructureProgramBinding"
    sendOwnedClassMessage cls' newSelector

-- | The name of the variable in the Program.
--
-- ObjC selector: @- name@
name :: IsMLModelStructureProgramBinding mlModelStructureProgramBinding => mlModelStructureProgramBinding -> IO (Id NSString)
name mlModelStructureProgramBinding =
  sendMessage mlModelStructureProgramBinding nameSelector

-- | The compile time constant value in the Program.
--
-- ObjC selector: @- value@
value :: IsMLModelStructureProgramBinding mlModelStructureProgramBinding => mlModelStructureProgramBinding -> IO (Id MLModelStructureProgramValue)
value mlModelStructureProgramBinding =
  sendMessage mlModelStructureProgramBinding valueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLModelStructureProgramBinding)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLModelStructureProgramBinding)
newSelector = mkSelector "new"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id MLModelStructureProgramValue)
valueSelector = mkSelector "value"

