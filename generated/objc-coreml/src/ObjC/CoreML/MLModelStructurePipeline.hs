{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class representing the structure of a Pipeline model.
--
-- Generated bindings for @MLModelStructurePipeline@.
module ObjC.CoreML.MLModelStructurePipeline
  ( MLModelStructurePipeline
  , IsMLModelStructurePipeline(..)
  , init_
  , new
  , subModelNames
  , subModels
  , initSelector
  , newSelector
  , subModelNamesSelector
  , subModelsSelector


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
init_ :: IsMLModelStructurePipeline mlModelStructurePipeline => mlModelStructurePipeline -> IO (Id MLModelStructurePipeline)
init_ mlModelStructurePipeline  =
  sendMsg mlModelStructurePipeline (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MLModelStructurePipeline)
new  =
  do
    cls' <- getRequiredClass "MLModelStructurePipeline"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The names of the sub models in the pipeline.
--
-- ObjC selector: @- subModelNames@
subModelNames :: IsMLModelStructurePipeline mlModelStructurePipeline => mlModelStructurePipeline -> IO (Id NSArray)
subModelNames mlModelStructurePipeline  =
  sendMsg mlModelStructurePipeline (mkSelector "subModelNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The structure of the sub models in the pipeline.
--
-- ObjC selector: @- subModels@
subModels :: IsMLModelStructurePipeline mlModelStructurePipeline => mlModelStructurePipeline -> IO (Id NSArray)
subModels mlModelStructurePipeline  =
  sendMsg mlModelStructurePipeline (mkSelector "subModels") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @subModelNames@
subModelNamesSelector :: Selector
subModelNamesSelector = mkSelector "subModelNames"

-- | @Selector@ for @subModels@
subModelsSelector :: Selector
subModelsSelector = mkSelector "subModels"

