{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNLabelsNode
--
-- The labels and weights for each MPSImage are passed in               separately to the graph in a MPSNNLabels object. If               the batch interface is used then there will be a               MPSStateBatch of these of the same size as the MPSImageBatch               that holds the images.  The MPSNNLabelsNode is a place               holder in the graph for these nodes. The MPSNNLabels node               is taken as an input to the Loss node
--
-- Generated bindings for @MPSNNLabelsNode@.
module ObjC.MetalPerformanceShaders.MPSNNLabelsNode
  ( MPSNNLabelsNode
  , IsMPSNNLabelsNode(..)


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

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

