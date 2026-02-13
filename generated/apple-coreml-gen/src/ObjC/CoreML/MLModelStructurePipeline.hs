{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMLModelStructurePipeline mlModelStructurePipeline => mlModelStructurePipeline -> IO (Id MLModelStructurePipeline)
init_ mlModelStructurePipeline =
  sendOwnedMessage mlModelStructurePipeline initSelector

-- | @+ new@
new :: IO (Id MLModelStructurePipeline)
new  =
  do
    cls' <- getRequiredClass "MLModelStructurePipeline"
    sendOwnedClassMessage cls' newSelector

-- | The names of the sub models in the pipeline.
--
-- ObjC selector: @- subModelNames@
subModelNames :: IsMLModelStructurePipeline mlModelStructurePipeline => mlModelStructurePipeline -> IO (Id NSArray)
subModelNames mlModelStructurePipeline =
  sendMessage mlModelStructurePipeline subModelNamesSelector

-- | The structure of the sub models in the pipeline.
--
-- ObjC selector: @- subModels@
subModels :: IsMLModelStructurePipeline mlModelStructurePipeline => mlModelStructurePipeline -> IO (Id NSArray)
subModels mlModelStructurePipeline =
  sendMessage mlModelStructurePipeline subModelsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLModelStructurePipeline)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLModelStructurePipeline)
newSelector = mkSelector "new"

-- | @Selector@ for @subModelNames@
subModelNamesSelector :: Selector '[] (Id NSArray)
subModelNamesSelector = mkSelector "subModelNames"

-- | @Selector@ for @subModels@
subModelsSelector :: Selector '[] (Id NSArray)
subModelsSelector = mkSelector "subModels"

