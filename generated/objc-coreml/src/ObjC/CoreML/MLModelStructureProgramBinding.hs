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
  , newSelector
  , nameSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMLModelStructureProgramBinding mlModelStructureProgramBinding => mlModelStructureProgramBinding -> IO (Id MLModelStructureProgramBinding)
init_ mlModelStructureProgramBinding  =
  sendMsg mlModelStructureProgramBinding (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MLModelStructureProgramBinding)
new  =
  do
    cls' <- getRequiredClass "MLModelStructureProgramBinding"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The name of the variable in the Program.
--
-- ObjC selector: @- name@
name :: IsMLModelStructureProgramBinding mlModelStructureProgramBinding => mlModelStructureProgramBinding -> IO (Id NSString)
name mlModelStructureProgramBinding  =
  sendMsg mlModelStructureProgramBinding (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The compile time constant value in the Program.
--
-- ObjC selector: @- value@
value :: IsMLModelStructureProgramBinding mlModelStructureProgramBinding => mlModelStructureProgramBinding -> IO (Id MLModelStructureProgramValue)
value mlModelStructureProgramBinding  =
  sendMsg mlModelStructureProgramBinding (mkSelector "value") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

