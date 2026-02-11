{-# LANGUAGE PatternSynonyms #-}
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
  , isAllowedValueSelector
  , nameSelector
  , typeSelector
  , optionalSelector
  , multiArrayConstraintSelector
  , imageConstraintSelector
  , dictionaryConstraintSelector
  , sequenceConstraintSelector
  , stateConstraintSelector

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
import ObjC.CoreML.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Check if MLFeatureValue is valid based on this description
--
-- ObjC selector: @- isAllowedValue:@
isAllowedValue :: (IsMLFeatureDescription mlFeatureDescription, IsMLFeatureValue value) => mlFeatureDescription -> value -> IO Bool
isAllowedValue mlFeatureDescription  value =
withObjCPtr value $ \raw_value ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlFeatureDescription (mkSelector "isAllowedValue:") retCULong [argPtr (castPtr raw_value :: Ptr ())]

-- | Name of feature
--
-- ObjC selector: @- name@
name :: IsMLFeatureDescription mlFeatureDescription => mlFeatureDescription -> IO (Id NSString)
name mlFeatureDescription  =
  sendMsg mlFeatureDescription (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Type of data
--
-- ObjC selector: @- type@
type_ :: IsMLFeatureDescription mlFeatureDescription => mlFeatureDescription -> IO MLFeatureType
type_ mlFeatureDescription  =
  fmap (coerce :: CLong -> MLFeatureType) $ sendMsg mlFeatureDescription (mkSelector "type") retCLong []

-- | Whether this feature can take an undefined value or not
--
-- ObjC selector: @- optional@
optional :: IsMLFeatureDescription mlFeatureDescription => mlFeatureDescription -> IO Bool
optional mlFeatureDescription  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlFeatureDescription (mkSelector "optional") retCULong []

-- | Constraint when type == MLFeatureTypeMultiArray, nil otherwise
--
-- ObjC selector: @- multiArrayConstraint@
multiArrayConstraint :: IsMLFeatureDescription mlFeatureDescription => mlFeatureDescription -> IO (Id MLMultiArrayConstraint)
multiArrayConstraint mlFeatureDescription  =
  sendMsg mlFeatureDescription (mkSelector "multiArrayConstraint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Constraint when type == MLFeatureTypeImage, nil otherwise
--
-- ObjC selector: @- imageConstraint@
imageConstraint :: IsMLFeatureDescription mlFeatureDescription => mlFeatureDescription -> IO (Id MLImageConstraint)
imageConstraint mlFeatureDescription  =
  sendMsg mlFeatureDescription (mkSelector "imageConstraint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Constraint when type == MLFeatureTypeDictionary, nil otherwise
--
-- ObjC selector: @- dictionaryConstraint@
dictionaryConstraint :: IsMLFeatureDescription mlFeatureDescription => mlFeatureDescription -> IO (Id MLDictionaryConstraint)
dictionaryConstraint mlFeatureDescription  =
  sendMsg mlFeatureDescription (mkSelector "dictionaryConstraint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Constraint when type == MLFeatureTypeSequence, nil otherwise
--
-- ObjC selector: @- sequenceConstraint@
sequenceConstraint :: IsMLFeatureDescription mlFeatureDescription => mlFeatureDescription -> IO (Id MLSequenceConstraint)
sequenceConstraint mlFeatureDescription  =
  sendMsg mlFeatureDescription (mkSelector "sequenceConstraint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The state feature value constraint.
--
-- The property has a value when @.type == MLFeatureTypeState@.
--
-- ObjC selector: @- stateConstraint@
stateConstraint :: IsMLFeatureDescription mlFeatureDescription => mlFeatureDescription -> IO (Id MLStateConstraint)
stateConstraint mlFeatureDescription  =
  sendMsg mlFeatureDescription (mkSelector "stateConstraint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isAllowedValue:@
isAllowedValueSelector :: Selector
isAllowedValueSelector = mkSelector "isAllowedValue:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @optional@
optionalSelector :: Selector
optionalSelector = mkSelector "optional"

-- | @Selector@ for @multiArrayConstraint@
multiArrayConstraintSelector :: Selector
multiArrayConstraintSelector = mkSelector "multiArrayConstraint"

-- | @Selector@ for @imageConstraint@
imageConstraintSelector :: Selector
imageConstraintSelector = mkSelector "imageConstraint"

-- | @Selector@ for @dictionaryConstraint@
dictionaryConstraintSelector :: Selector
dictionaryConstraintSelector = mkSelector "dictionaryConstraint"

-- | @Selector@ for @sequenceConstraint@
sequenceConstraintSelector :: Selector
sequenceConstraintSelector = mkSelector "sequenceConstraint"

-- | @Selector@ for @stateConstraint@
stateConstraintSelector :: Selector
stateConstraintSelector = mkSelector "stateConstraint"

