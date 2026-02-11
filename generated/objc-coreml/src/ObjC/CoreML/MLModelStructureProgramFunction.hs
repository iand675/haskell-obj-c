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
  , initSelector
  , newSelector
  , inputsSelector
  , blockSelector


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
init_ :: IsMLModelStructureProgramFunction mlModelStructureProgramFunction => mlModelStructureProgramFunction -> IO (Id MLModelStructureProgramFunction)
init_ mlModelStructureProgramFunction  =
  sendMsg mlModelStructureProgramFunction (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MLModelStructureProgramFunction)
new  =
  do
    cls' <- getRequiredClass "MLModelStructureProgramFunction"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The named inputs to the function.
--
-- ObjC selector: @- inputs@
inputs :: IsMLModelStructureProgramFunction mlModelStructureProgramFunction => mlModelStructureProgramFunction -> IO (Id NSArray)
inputs mlModelStructureProgramFunction  =
  sendMsg mlModelStructureProgramFunction (mkSelector "inputs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The active block in the function.
--
-- ObjC selector: @- block@
block :: IsMLModelStructureProgramFunction mlModelStructureProgramFunction => mlModelStructureProgramFunction -> IO (Id MLModelStructureProgramBlock)
block mlModelStructureProgramFunction  =
  sendMsg mlModelStructureProgramFunction (mkSelector "block") (retPtr retVoid) [] >>= retainedObject . castPtr

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

-- | @Selector@ for @block@
blockSelector :: Selector
blockSelector = mkSelector "block"

