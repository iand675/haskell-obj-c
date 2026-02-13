{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Describes a model parameter along with a default value and any applicable constaint on the values.
--
-- Generated bindings for @MLParameterDescription@.
module ObjC.CoreML.MLParameterDescription
  ( MLParameterDescription
  , IsMLParameterDescription(..)
  , key
  , defaultValue
  , numericConstraint
  , defaultValueSelector
  , keySelector
  , numericConstraintSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- key@
key :: IsMLParameterDescription mlParameterDescription => mlParameterDescription -> IO (Id MLParameterKey)
key mlParameterDescription =
  sendMessage mlParameterDescription keySelector

-- | @- defaultValue@
defaultValue :: IsMLParameterDescription mlParameterDescription => mlParameterDescription -> IO RawId
defaultValue mlParameterDescription =
  sendMessage mlParameterDescription defaultValueSelector

-- | @- numericConstraint@
numericConstraint :: IsMLParameterDescription mlParameterDescription => mlParameterDescription -> IO (Id MLNumericConstraint)
numericConstraint mlParameterDescription =
  sendMessage mlParameterDescription numericConstraintSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @key@
keySelector :: Selector '[] (Id MLParameterKey)
keySelector = mkSelector "key"

-- | @Selector@ for @defaultValue@
defaultValueSelector :: Selector '[] RawId
defaultValueSelector = mkSelector "defaultValue"

-- | @Selector@ for @numericConstraint@
numericConstraintSelector :: Selector '[] (Id MLNumericConstraint)
numericConstraintSelector = mkSelector "numericConstraint"

