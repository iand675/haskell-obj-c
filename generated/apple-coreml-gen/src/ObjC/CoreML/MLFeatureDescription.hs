{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Description of a feature
--
-- Generated bindings for @MLFeatureDescription@.
module ObjC.CoreML.MLFeatureDescription
  ( MLFeatureDescription
  , IsMLFeatureDescription(..)
  , isAllowedValue
  , name
  , type_
  , optional
  , multiArrayConstraint
  , imageConstraint
  , dictionaryConstraint
  , sequenceConstraint
  , stateConstraint
  , dictionaryConstraintSelector
  , imageConstraintSelector
  , isAllowedValueSelector
  , multiArrayConstraintSelector
  , nameSelector
  , optionalSelector
  , sequenceConstraintSelector
  , stateConstraintSelector
  , typeSelector

  -- * Enum types
  , MLFeatureType(MLFeatureType)
  , pattern MLFeatureTypeInvalid
  , pattern MLFeatureTypeInt64
  , pattern MLFeatureTypeDouble
  , pattern MLFeatureTypeString
  , pattern MLFeatureTypeImage
  , pattern MLFeatureTypeMultiArray
  , pattern MLFeatureTypeDictionary
  , pattern MLFeatureTypeSequence
  , pattern MLFeatureTypeState

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

-- | Check if MLFeatureValue is valid based on this description
--
-- ObjC selector: @- isAllowedValue:@
isAllowedValue :: (IsMLFeatureDescription mlFeatureDescription, IsMLFeatureValue value) => mlFeatureDescription -> value -> IO Bool
isAllowedValue mlFeatureDescription value =
  sendMessage mlFeatureDescription isAllowedValueSelector (toMLFeatureValue value)

-- | Name of feature
--
-- ObjC selector: @- name@
name :: IsMLFeatureDescription mlFeatureDescription => mlFeatureDescription -> IO (Id NSString)
name mlFeatureDescription =
  sendMessage mlFeatureDescription nameSelector

-- | Type of data
--
-- ObjC selector: @- type@
type_ :: IsMLFeatureDescription mlFeatureDescription => mlFeatureDescription -> IO MLFeatureType
type_ mlFeatureDescription =
  sendMessage mlFeatureDescription typeSelector

-- | Whether this feature can take an undefined value or not
--
-- ObjC selector: @- optional@
optional :: IsMLFeatureDescription mlFeatureDescription => mlFeatureDescription -> IO Bool
optional mlFeatureDescription =
  sendMessage mlFeatureDescription optionalSelector

-- | Constraint when type == MLFeatureTypeMultiArray, nil otherwise
--
-- ObjC selector: @- multiArrayConstraint@
multiArrayConstraint :: IsMLFeatureDescription mlFeatureDescription => mlFeatureDescription -> IO (Id MLMultiArrayConstraint)
multiArrayConstraint mlFeatureDescription =
  sendMessage mlFeatureDescription multiArrayConstraintSelector

-- | Constraint when type == MLFeatureTypeImage, nil otherwise
--
-- ObjC selector: @- imageConstraint@
imageConstraint :: IsMLFeatureDescription mlFeatureDescription => mlFeatureDescription -> IO (Id MLImageConstraint)
imageConstraint mlFeatureDescription =
  sendMessage mlFeatureDescription imageConstraintSelector

-- | Constraint when type == MLFeatureTypeDictionary, nil otherwise
--
-- ObjC selector: @- dictionaryConstraint@
dictionaryConstraint :: IsMLFeatureDescription mlFeatureDescription => mlFeatureDescription -> IO (Id MLDictionaryConstraint)
dictionaryConstraint mlFeatureDescription =
  sendMessage mlFeatureDescription dictionaryConstraintSelector

-- | Constraint when type == MLFeatureTypeSequence, nil otherwise
--
-- ObjC selector: @- sequenceConstraint@
sequenceConstraint :: IsMLFeatureDescription mlFeatureDescription => mlFeatureDescription -> IO (Id MLSequenceConstraint)
sequenceConstraint mlFeatureDescription =
  sendMessage mlFeatureDescription sequenceConstraintSelector

-- | The state feature value constraint.
--
-- The property has a value when @.type == MLFeatureTypeState@.
--
-- ObjC selector: @- stateConstraint@
stateConstraint :: IsMLFeatureDescription mlFeatureDescription => mlFeatureDescription -> IO (Id MLStateConstraint)
stateConstraint mlFeatureDescription =
  sendMessage mlFeatureDescription stateConstraintSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isAllowedValue:@
isAllowedValueSelector :: Selector '[Id MLFeatureValue] Bool
isAllowedValueSelector = mkSelector "isAllowedValue:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @type@
typeSelector :: Selector '[] MLFeatureType
typeSelector = mkSelector "type"

-- | @Selector@ for @optional@
optionalSelector :: Selector '[] Bool
optionalSelector = mkSelector "optional"

-- | @Selector@ for @multiArrayConstraint@
multiArrayConstraintSelector :: Selector '[] (Id MLMultiArrayConstraint)
multiArrayConstraintSelector = mkSelector "multiArrayConstraint"

-- | @Selector@ for @imageConstraint@
imageConstraintSelector :: Selector '[] (Id MLImageConstraint)
imageConstraintSelector = mkSelector "imageConstraint"

-- | @Selector@ for @dictionaryConstraint@
dictionaryConstraintSelector :: Selector '[] (Id MLDictionaryConstraint)
dictionaryConstraintSelector = mkSelector "dictionaryConstraint"

-- | @Selector@ for @sequenceConstraint@
sequenceConstraintSelector :: Selector '[] (Id MLSequenceConstraint)
sequenceConstraintSelector = mkSelector "sequenceConstraint"

-- | @Selector@ for @stateConstraint@
stateConstraintSelector :: Selector '[] (Id MLStateConstraint)
stateConstraintSelector = mkSelector "stateConstraint"

