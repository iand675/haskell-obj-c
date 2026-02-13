{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class representing an argument in the Program.
--
-- Generated bindings for @MLModelStructureProgramArgument@.
module ObjC.CoreML.MLModelStructureProgramArgument
  ( MLModelStructureProgramArgument
  , IsMLModelStructureProgramArgument(..)
  , init_
  , new
  , bindings
  , bindingsSelector
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
init_ :: IsMLModelStructureProgramArgument mlModelStructureProgramArgument => mlModelStructureProgramArgument -> IO (Id MLModelStructureProgramArgument)
init_ mlModelStructureProgramArgument =
  sendOwnedMessage mlModelStructureProgramArgument initSelector

-- | @+ new@
new :: IO (Id MLModelStructureProgramArgument)
new  =
  do
    cls' <- getRequiredClass "MLModelStructureProgramArgument"
    sendOwnedClassMessage cls' newSelector

-- | The array of bindings.
--
-- ObjC selector: @- bindings@
bindings :: IsMLModelStructureProgramArgument mlModelStructureProgramArgument => mlModelStructureProgramArgument -> IO (Id NSArray)
bindings mlModelStructureProgramArgument =
  sendMessage mlModelStructureProgramArgument bindingsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLModelStructureProgramArgument)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLModelStructureProgramArgument)
newSelector = mkSelector "new"

-- | @Selector@ for @bindings@
bindingsSelector :: Selector '[] (Id NSArray)
bindingsSelector = mkSelector "bindings"

