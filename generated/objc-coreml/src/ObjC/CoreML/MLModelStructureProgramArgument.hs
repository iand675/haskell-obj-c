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
  , initSelector
  , newSelector
  , bindingsSelector


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
init_ :: IsMLModelStructureProgramArgument mlModelStructureProgramArgument => mlModelStructureProgramArgument -> IO (Id MLModelStructureProgramArgument)
init_ mlModelStructureProgramArgument  =
  sendMsg mlModelStructureProgramArgument (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MLModelStructureProgramArgument)
new  =
  do
    cls' <- getRequiredClass "MLModelStructureProgramArgument"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The array of bindings.
--
-- ObjC selector: @- bindings@
bindings :: IsMLModelStructureProgramArgument mlModelStructureProgramArgument => mlModelStructureProgramArgument -> IO (Id NSArray)
bindings mlModelStructureProgramArgument  =
  sendMsg mlModelStructureProgramArgument (mkSelector "bindings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @bindings@
bindingsSelector :: Selector
bindingsSelector = mkSelector "bindings"

