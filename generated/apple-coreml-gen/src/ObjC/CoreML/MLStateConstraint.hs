{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Constraint of a state feature value.
--
-- Generated bindings for @MLStateConstraint@.
module ObjC.CoreML.MLStateConstraint
  ( MLStateConstraint
  , IsMLStateConstraint(..)
  , bufferShape
  , dataType
  , bufferShapeSelector
  , dataTypeSelector

  -- * Enum types
  , MLMultiArrayDataType(MLMultiArrayDataType)
  , pattern MLMultiArrayDataTypeDouble
  , pattern MLMultiArrayDataTypeFloat64
  , pattern MLMultiArrayDataTypeFloat32
  , pattern MLMultiArrayDataTypeFloat16
  , pattern MLMultiArrayDataTypeFloat
  , pattern MLMultiArrayDataTypeInt32
  , pattern MLMultiArrayDataTypeInt8

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.CoreML.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The shape of the state buffer.
--
-- ObjC selector: @- bufferShape@
bufferShape :: IsMLStateConstraint mlStateConstraint => mlStateConstraint -> IO (Id NSArray)
bufferShape mlStateConstraint =
  sendMessage mlStateConstraint bufferShapeSelector

-- | The data type of scalars in the state buffer.
--
-- ObjC selector: @- dataType@
dataType :: IsMLStateConstraint mlStateConstraint => mlStateConstraint -> IO MLMultiArrayDataType
dataType mlStateConstraint =
  sendMessage mlStateConstraint dataTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bufferShape@
bufferShapeSelector :: Selector '[] (Id NSArray)
bufferShapeSelector = mkSelector "bufferShape"

-- | @Selector@ for @dataType@
dataTypeSelector :: Selector '[] MLMultiArrayDataType
dataTypeSelector = mkSelector "dataType"

