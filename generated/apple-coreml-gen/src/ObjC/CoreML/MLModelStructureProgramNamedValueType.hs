{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class representing a named value type in a Program.
--
-- Generated bindings for @MLModelStructureProgramNamedValueType@.
module ObjC.CoreML.MLModelStructureProgramNamedValueType
  ( MLModelStructureProgramNamedValueType
  , IsMLModelStructureProgramNamedValueType(..)
  , init_
  , new
  , name
  , type_
  , initSelector
  , nameSelector
  , newSelector
  , typeSelector


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
init_ :: IsMLModelStructureProgramNamedValueType mlModelStructureProgramNamedValueType => mlModelStructureProgramNamedValueType -> IO (Id MLModelStructureProgramNamedValueType)
init_ mlModelStructureProgramNamedValueType =
  sendOwnedMessage mlModelStructureProgramNamedValueType initSelector

-- | @+ new@
new :: IO (Id MLModelStructureProgramNamedValueType)
new  =
  do
    cls' <- getRequiredClass "MLModelStructureProgramNamedValueType"
    sendOwnedClassMessage cls' newSelector

-- | The name of the parameter.
--
-- ObjC selector: @- name@
name :: IsMLModelStructureProgramNamedValueType mlModelStructureProgramNamedValueType => mlModelStructureProgramNamedValueType -> IO (Id NSString)
name mlModelStructureProgramNamedValueType =
  sendMessage mlModelStructureProgramNamedValueType nameSelector

-- | The type of the parameter.
--
-- ObjC selector: @- type@
type_ :: IsMLModelStructureProgramNamedValueType mlModelStructureProgramNamedValueType => mlModelStructureProgramNamedValueType -> IO (Id MLModelStructureProgramValueType)
type_ mlModelStructureProgramNamedValueType =
  sendMessage mlModelStructureProgramNamedValueType typeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLModelStructureProgramNamedValueType)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLModelStructureProgramNamedValueType)
newSelector = mkSelector "new"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id MLModelStructureProgramValueType)
typeSelector = mkSelector "type"

