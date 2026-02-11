{-# LANGUAGE PatternSynonyms #-}
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
  , sequenceWithStringArraySelector
  , sequenceWithInt64ArraySelector
  , typeSelector
  , stringValuesSelector
  , int64ValuesSelector

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

-- | Empty sequence of a sepcific type
--
-- ObjC selector: @+ emptySequenceWithType:@
emptySequenceWithType :: MLFeatureType -> IO (Id MLSequence)
emptySequenceWithType type_ =
  do
    cls' <- getRequiredClass "MLSequence"
    sendClassMsg cls' (mkSelector "emptySequenceWithType:") (retPtr retVoid) [argCLong (coerce type_)] >>= retainedObject . castPtr

-- | String sequences, property will be empty array if type is MLFeatureTypeString
--
-- ObjC selector: @+ sequenceWithStringArray:@
sequenceWithStringArray :: IsNSArray stringValues => stringValues -> IO (Id MLSequence)
sequenceWithStringArray stringValues =
  do
    cls' <- getRequiredClass "MLSequence"
    withObjCPtr stringValues $ \raw_stringValues ->
      sendClassMsg cls' (mkSelector "sequenceWithStringArray:") (retPtr retVoid) [argPtr (castPtr raw_stringValues :: Ptr ())] >>= retainedObject . castPtr

-- | int64 sequence, propery will be empty array if type is MLFeatureTypeInt64
--
-- ObjC selector: @+ sequenceWithInt64Array:@
sequenceWithInt64Array :: IsNSArray int64Values => int64Values -> IO (Id MLSequence)
sequenceWithInt64Array int64Values =
  do
    cls' <- getRequiredClass "MLSequence"
    withObjCPtr int64Values $ \raw_int64Values ->
      sendClassMsg cls' (mkSelector "sequenceWithInt64Array:") (retPtr retVoid) [argPtr (castPtr raw_int64Values :: Ptr ())] >>= retainedObject . castPtr

-- | Type of values held
--
-- ObjC selector: @- type@
type_ :: IsMLSequence mlSequence => mlSequence -> IO MLFeatureType
type_ mlSequence  =
  fmap (coerce :: CLong -> MLFeatureType) $ sendMsg mlSequence (mkSelector "type") retCLong []

-- | @- stringValues@
stringValues :: IsMLSequence mlSequence => mlSequence -> IO (Id NSArray)
stringValues mlSequence  =
  sendMsg mlSequence (mkSelector "stringValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- int64Values@
int64Values :: IsMLSequence mlSequence => mlSequence -> IO (Id NSArray)
int64Values mlSequence  =
  sendMsg mlSequence (mkSelector "int64Values") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @emptySequenceWithType:@
emptySequenceWithTypeSelector :: Selector
emptySequenceWithTypeSelector = mkSelector "emptySequenceWithType:"

-- | @Selector@ for @sequenceWithStringArray:@
sequenceWithStringArraySelector :: Selector
sequenceWithStringArraySelector = mkSelector "sequenceWithStringArray:"

-- | @Selector@ for @sequenceWithInt64Array:@
sequenceWithInt64ArraySelector :: Selector
sequenceWithInt64ArraySelector = mkSelector "sequenceWithInt64Array:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @stringValues@
stringValuesSelector :: Selector
stringValuesSelector = mkSelector "stringValues"

-- | @Selector@ for @int64Values@
int64ValuesSelector :: Selector
int64ValuesSelector = mkSelector "int64Values"

