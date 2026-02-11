{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class representing the structure of an ML Program model.
--
-- Generated bindings for @MLModelStructureProgram@.
module ObjC.CoreML.MLModelStructureProgram
  ( MLModelStructureProgram
  , IsMLModelStructureProgram(..)
  , init_
  , new
  , functions
  , initSelector
  , newSelector
  , functionsSelector


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
init_ :: IsMLModelStructureProgram mlModelStructureProgram => mlModelStructureProgram -> IO (Id MLModelStructureProgram)
init_ mlModelStructureProgram  =
  sendMsg mlModelStructureProgram (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MLModelStructureProgram)
new  =
  do
    cls' <- getRequiredClass "MLModelStructureProgram"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The functions in the program.
--
-- ObjC selector: @- functions@
functions :: IsMLModelStructureProgram mlModelStructureProgram => mlModelStructureProgram -> IO (Id NSDictionary)
functions mlModelStructureProgram  =
  sendMsg mlModelStructureProgram (mkSelector "functions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @functions@
functionsSelector :: Selector
functionsSelector = mkSelector "functions"

