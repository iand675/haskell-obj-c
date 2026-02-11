{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | returns elementwise comparison of left and right
--
-- Generated bindings for @MPSNNComparisonNode@.
module ObjC.MetalPerformanceShaders.MPSNNComparisonNode
  ( MPSNNComparisonNode
  , IsMPSNNComparisonNode(..)
  , comparisonType
  , setComparisonType
  , comparisonTypeSelector
  , setComparisonTypeSelector

  -- * Enum types
  , MPSNNComparisonType(MPSNNComparisonType)
  , pattern MPSNNComparisonTypeEqual
  , pattern MPSNNComparisonTypeNotEqual
  , pattern MPSNNComparisonTypeLess
  , pattern MPSNNComparisonTypeLessOrEqual
  , pattern MPSNNComparisonTypeGreater
  , pattern MPSNNComparisonTypeGreaterOrEqual

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
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | comparisonType
--
-- The comparison type to set on the underlying kernel.  Defaults              to MPSNNComparisonTypeEqual.
--
-- ObjC selector: @- comparisonType@
comparisonType :: IsMPSNNComparisonNode mpsnnComparisonNode => mpsnnComparisonNode -> IO MPSNNComparisonType
comparisonType mpsnnComparisonNode  =
  fmap (coerce :: CULong -> MPSNNComparisonType) $ sendMsg mpsnnComparisonNode (mkSelector "comparisonType") retCULong []

-- | comparisonType
--
-- The comparison type to set on the underlying kernel.  Defaults              to MPSNNComparisonTypeEqual.
--
-- ObjC selector: @- setComparisonType:@
setComparisonType :: IsMPSNNComparisonNode mpsnnComparisonNode => mpsnnComparisonNode -> MPSNNComparisonType -> IO ()
setComparisonType mpsnnComparisonNode  value =
  sendMsg mpsnnComparisonNode (mkSelector "setComparisonType:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @comparisonType@
comparisonTypeSelector :: Selector
comparisonTypeSelector = mkSelector "comparisonType"

-- | @Selector@ for @setComparisonType:@
setComparisonTypeSelector :: Selector
setComparisonTypeSelector = mkSelector "setComparisonType:"

