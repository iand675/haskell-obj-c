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
  , initSelector
  , newSelector
  , operatorNameSelector
  , inputsSelector
  , outputsSelector
  , blocksSelector


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
init_ :: IsMLModelStructureProgramOperation mlModelStructureProgramOperation => mlModelStructureProgramOperation -> IO (Id MLModelStructureProgramOperation)
init_ mlModelStructureProgramOperation  =
  sendMsg mlModelStructureProgramOperation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MLModelStructureProgramOperation)
new  =
  do
    cls' <- getRequiredClass "MLModelStructureProgramOperation"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The name of the operator, e.g., "conv", "pool", "softmax", etc.
--
-- ObjC selector: @- operatorName@
operatorName :: IsMLModelStructureProgramOperation mlModelStructureProgramOperation => mlModelStructureProgramOperation -> IO (Id NSString)
operatorName mlModelStructureProgramOperation  =
  sendMsg mlModelStructureProgramOperation (mkSelector "operatorName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The arguments to the Operation.
--
-- ObjC selector: @- inputs@
inputs :: IsMLModelStructureProgramOperation mlModelStructureProgramOperation => mlModelStructureProgramOperation -> IO (Id NSDictionary)
inputs mlModelStructureProgramOperation  =
  sendMsg mlModelStructureProgramOperation (mkSelector "inputs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The outputs of the Operation.
--
-- ObjC selector: @- outputs@
outputs :: IsMLModelStructureProgramOperation mlModelStructureProgramOperation => mlModelStructureProgramOperation -> IO (Id NSArray)
outputs mlModelStructureProgramOperation  =
  sendMsg mlModelStructureProgramOperation (mkSelector "outputs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Nested blocks for loops and conditionals, e.g., a conditional block will have two entries here.
--
-- ObjC selector: @- blocks@
blocks :: IsMLModelStructureProgramOperation mlModelStructureProgramOperation => mlModelStructureProgramOperation -> IO (Id NSArray)
blocks mlModelStructureProgramOperation  =
  sendMsg mlModelStructureProgramOperation (mkSelector "blocks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @operatorName@
operatorNameSelector :: Selector
operatorNameSelector = mkSelector "operatorName"

-- | @Selector@ for @inputs@
inputsSelector :: Selector
inputsSelector = mkSelector "inputs"

-- | @Selector@ for @outputs@
outputsSelector :: Selector
outputsSelector = mkSelector "outputs"

-- | @Selector@ for @blocks@
blocksSelector :: Selector
blocksSelector = mkSelector "blocks"

