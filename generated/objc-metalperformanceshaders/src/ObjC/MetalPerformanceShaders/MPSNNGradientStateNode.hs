{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNGradientStateNode
--
-- During training, each MPSNNFilterNode has a corresponding              MPSNNGradientFilterNode for the gradient computation for              trainable parameter update. The two communicate through a              MPSNNGradientStateNode or subclass which carries information              about the inference pass settings to the gradient pass.              You can avoid managing these -- there will be many! -- by              using -[MPSNNFilterNode gradientFilterWithSources:] to make              the MPSNNGradientFilterNodes. That method will append              the necessary extra information like MPSNNGradientState              nodes and inference filter source image nodes to the object as              needed.
--
-- Generated bindings for @MPSNNGradientStateNode@.
module ObjC.MetalPerformanceShaders.MPSNNGradientStateNode
  ( MPSNNGradientStateNode
  , IsMPSNNGradientStateNode(..)


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

