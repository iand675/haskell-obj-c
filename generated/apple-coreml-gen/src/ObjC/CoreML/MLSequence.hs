{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An immutable container holding an ordered collection of feature values of the same type.
--
-- Generated bindings for @MLSequence@.
module ObjC.CoreML.MLSequence
  ( MLSequence
  , IsMLSequence(..)
  , emptySequenceWithType
  , sequenceWithStringArray
  , sequenceWithInt64Array
  , type_
  , stringValues
  , int64Values
  , emptySequenceWithTypeSelector
  , int64ValuesSelector
  , sequenceWithInt64ArraySelector
  , sequenceWithStringArraySelector
  , stringValuesSelector
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

-- | Empty sequence of a sepcific type
--
-- ObjC selector: @+ emptySequenceWithType:@
emptySequenceWithType :: MLFeatureType -> IO (Id MLSequence)
emptySequenceWithType type_ =
  do
    cls' <- getRequiredClass "MLSequence"
    sendClassMessage cls' emptySequenceWithTypeSelector type_

-- | String sequences, property will be empty array if type is MLFeatureTypeString
--
-- ObjC selector: @+ sequenceWithStringArray:@
sequenceWithStringArray :: IsNSArray stringValues => stringValues -> IO (Id MLSequence)
sequenceWithStringArray stringValues =
  do
    cls' <- getRequiredClass "MLSequence"
    sendClassMessage cls' sequenceWithStringArraySelector (toNSArray stringValues)

-- | int64 sequence, propery will be empty array if type is MLFeatureTypeInt64
--
-- ObjC selector: @+ sequenceWithInt64Array:@
sequenceWithInt64Array :: IsNSArray int64Values => int64Values -> IO (Id MLSequence)
sequenceWithInt64Array int64Values =
  do
    cls' <- getRequiredClass "MLSequence"
    sendClassMessage cls' sequenceWithInt64ArraySelector (toNSArray int64Values)

-- | Type of values held
--
-- ObjC selector: @- type@
type_ :: IsMLSequence mlSequence => mlSequence -> IO MLFeatureType
type_ mlSequence =
  sendMessage mlSequence typeSelector

-- | @- stringValues@
stringValues :: IsMLSequence mlSequence => mlSequence -> IO (Id NSArray)
stringValues mlSequence =
  sendMessage mlSequence stringValuesSelector

-- | @- int64Values@
int64Values :: IsMLSequence mlSequence => mlSequence -> IO (Id NSArray)
int64Values mlSequence =
  sendMessage mlSequence int64ValuesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @emptySequenceWithType:@
emptySequenceWithTypeSelector :: Selector '[MLFeatureType] (Id MLSequence)
emptySequenceWithTypeSelector = mkSelector "emptySequenceWithType:"

-- | @Selector@ for @sequenceWithStringArray:@
sequenceWithStringArraySelector :: Selector '[Id NSArray] (Id MLSequence)
sequenceWithStringArraySelector = mkSelector "sequenceWithStringArray:"

-- | @Selector@ for @sequenceWithInt64Array:@
sequenceWithInt64ArraySelector :: Selector '[Id NSArray] (Id MLSequence)
sequenceWithInt64ArraySelector = mkSelector "sequenceWithInt64Array:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] MLFeatureType
typeSelector = mkSelector "type"

-- | @Selector@ for @stringValues@
stringValuesSelector :: Selector '[] (Id NSArray)
stringValuesSelector = mkSelector "stringValues"

-- | @Selector@ for @int64Values@
int64ValuesSelector :: Selector '[] (Id NSArray)
int64ValuesSelector = mkSelector "int64Values"

