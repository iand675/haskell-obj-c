{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
comparisonType mpsnnComparisonNode =
  sendMessage mpsnnComparisonNode comparisonTypeSelector

-- | comparisonType
--
-- The comparison type to set on the underlying kernel.  Defaults              to MPSNNComparisonTypeEqual.
--
-- ObjC selector: @- setComparisonType:@
setComparisonType :: IsMPSNNComparisonNode mpsnnComparisonNode => mpsnnComparisonNode -> MPSNNComparisonType -> IO ()
setComparisonType mpsnnComparisonNode value =
  sendMessage mpsnnComparisonNode setComparisonTypeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @comparisonType@
comparisonTypeSelector :: Selector '[] MPSNNComparisonType
comparisonTypeSelector = mkSelector "comparisonType"

-- | @Selector@ for @setComparisonType:@
setComparisonTypeSelector :: Selector '[MPSNNComparisonType] ()
setComparisonTypeSelector = mkSelector "setComparisonType:"

