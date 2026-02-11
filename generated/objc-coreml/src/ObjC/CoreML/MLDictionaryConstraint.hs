{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLDictionaryConstraint
--
-- Constraint describing expected NSDictionary properties
--
-- Generated bindings for @MLDictionaryConstraint@.
module ObjC.CoreML.MLDictionaryConstraint
  ( MLDictionaryConstraint
  , IsMLDictionaryConstraint(..)
  , keyType
  , keyTypeSelector

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

-- | Required key type, described as MLFeatureType
--
-- ObjC selector: @- keyType@
keyType :: IsMLDictionaryConstraint mlDictionaryConstraint => mlDictionaryConstraint -> IO MLFeatureType
keyType mlDictionaryConstraint  =
  fmap (coerce :: CLong -> MLFeatureType) $ sendMsg mlDictionaryConstraint (mkSelector "keyType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @keyType@
keyTypeSelector :: Selector
keyTypeSelector = mkSelector "keyType"

