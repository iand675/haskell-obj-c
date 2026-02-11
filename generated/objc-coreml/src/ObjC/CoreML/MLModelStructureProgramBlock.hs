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
  , newSelector
  , inputsSelector
  , outputNamesSelector
  , operationsSelector


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
init_ :: IsMLModelStructureProgramBlock mlModelStructureProgramBlock => mlModelStructureProgramBlock -> IO (Id MLModelStructureProgramBlock)
init_ mlModelStructureProgramBlock  =
  sendMsg mlModelStructureProgramBlock (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MLModelStructureProgramBlock)
new  =
  do
    cls' <- getRequiredClass "MLModelStructureProgramBlock"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The named inputs to the block.
--
-- ObjC selector: @- inputs@
inputs :: IsMLModelStructureProgramBlock mlModelStructureProgramBlock => mlModelStructureProgramBlock -> IO (Id NSArray)
inputs mlModelStructureProgramBlock  =
  sendMsg mlModelStructureProgramBlock (mkSelector "inputs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The output names.
--
-- ObjC selector: @- outputNames@
outputNames :: IsMLModelStructureProgramBlock mlModelStructureProgramBlock => mlModelStructureProgramBlock -> IO (Id NSArray)
outputNames mlModelStructureProgramBlock  =
  sendMsg mlModelStructureProgramBlock (mkSelector "outputNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The list of topologically sorted operations in the block.
--
-- ObjC selector: @- operations@
operations :: IsMLModelStructureProgramBlock mlModelStructureProgramBlock => mlModelStructureProgramBlock -> IO (Id NSArray)
operations mlModelStructureProgramBlock  =
  sendMsg mlModelStructureProgramBlock (mkSelector "operations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @inputs@
inputsSelector :: Selector
inputsSelector = mkSelector "inputs"

-- | @Selector@ for @outputNames@
outputNamesSelector :: Selector
outputNamesSelector = mkSelector "outputNames"

-- | @Selector@ for @operations@
operationsSelector :: Selector
operationsSelector = mkSelector "operations"

