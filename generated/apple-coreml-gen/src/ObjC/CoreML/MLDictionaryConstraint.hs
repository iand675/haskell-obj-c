{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.CoreML.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Required key type, described as MLFeatureType
--
-- ObjC selector: @- keyType@
keyType :: IsMLDictionaryConstraint mlDictionaryConstraint => mlDictionaryConstraint -> IO MLFeatureType
keyType mlDictionaryConstraint =
  sendMessage mlDictionaryConstraint keyTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @keyType@
keyTypeSelector :: Selector '[] MLFeatureType
keyTypeSelector = mkSelector "keyType"

