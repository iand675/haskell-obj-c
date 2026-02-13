{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A concrete convenience class conforming to MLFeatureProvider.
--
-- Generated bindings for @MLDictionaryFeatureProvider@.
module ObjC.CoreML.MLDictionaryFeatureProvider
  ( MLDictionaryFeatureProvider
  , IsMLDictionaryFeatureProvider(..)
  , initWithDictionary_error
  , objectForKeyedSubscript
  , dictionary
  , dictionarySelector
  , initWithDictionary_errorSelector
  , objectForKeyedSubscriptSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create from a generic dictionary by converting all values to MLFeatureValues or from a dictionary with values already stored as MLFeatureValues.
--
-- An error results if the values are not or cannot be represented as MLFeatureValues.
--
-- ObjC selector: @- initWithDictionary:error:@
initWithDictionary_error :: (IsMLDictionaryFeatureProvider mlDictionaryFeatureProvider, IsNSDictionary dictionary, IsNSError error_) => mlDictionaryFeatureProvider -> dictionary -> error_ -> IO (Id MLDictionaryFeatureProvider)
initWithDictionary_error mlDictionaryFeatureProvider dictionary error_ =
  sendOwnedMessage mlDictionaryFeatureProvider initWithDictionary_errorSelector (toNSDictionary dictionary) (toNSError error_)

-- | Get the value for specified feature
--
-- ObjC selector: @- objectForKeyedSubscript:@
objectForKeyedSubscript :: (IsMLDictionaryFeatureProvider mlDictionaryFeatureProvider, IsNSString featureName) => mlDictionaryFeatureProvider -> featureName -> IO (Id MLFeatureValue)
objectForKeyedSubscript mlDictionaryFeatureProvider featureName =
  sendMessage mlDictionaryFeatureProvider objectForKeyedSubscriptSelector (toNSString featureName)

-- | Dictionary holding the feature values
--
-- ObjC selector: @- dictionary@
dictionary :: IsMLDictionaryFeatureProvider mlDictionaryFeatureProvider => mlDictionaryFeatureProvider -> IO (Id NSDictionary)
dictionary mlDictionaryFeatureProvider =
  sendMessage mlDictionaryFeatureProvider dictionarySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDictionary:error:@
initWithDictionary_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MLDictionaryFeatureProvider)
initWithDictionary_errorSelector = mkSelector "initWithDictionary:error:"

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector '[Id NSString] (Id MLFeatureValue)
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @dictionary@
dictionarySelector :: Selector '[] (Id NSDictionary)
dictionarySelector = mkSelector "dictionary"

