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
  , initWithDictionary_errorSelector
  , objectForKeyedSubscriptSelector
  , dictionarySelector


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
import ObjC.Foundation.Internal.Classes

-- | Create from a generic dictionary by converting all values to MLFeatureValues or from a dictionary with values already stored as MLFeatureValues.
--
-- An error results if the values are not or cannot be represented as MLFeatureValues.
--
-- ObjC selector: @- initWithDictionary:error:@
initWithDictionary_error :: (IsMLDictionaryFeatureProvider mlDictionaryFeatureProvider, IsNSDictionary dictionary, IsNSError error_) => mlDictionaryFeatureProvider -> dictionary -> error_ -> IO (Id MLDictionaryFeatureProvider)
initWithDictionary_error mlDictionaryFeatureProvider  dictionary error_ =
withObjCPtr dictionary $ \raw_dictionary ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg mlDictionaryFeatureProvider (mkSelector "initWithDictionary:error:") (retPtr retVoid) [argPtr (castPtr raw_dictionary :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Get the value for specified feature
--
-- ObjC selector: @- objectForKeyedSubscript:@
objectForKeyedSubscript :: (IsMLDictionaryFeatureProvider mlDictionaryFeatureProvider, IsNSString featureName) => mlDictionaryFeatureProvider -> featureName -> IO (Id MLFeatureValue)
objectForKeyedSubscript mlDictionaryFeatureProvider  featureName =
withObjCPtr featureName $ \raw_featureName ->
    sendMsg mlDictionaryFeatureProvider (mkSelector "objectForKeyedSubscript:") (retPtr retVoid) [argPtr (castPtr raw_featureName :: Ptr ())] >>= retainedObject . castPtr

-- | Dictionary holding the feature values
--
-- ObjC selector: @- dictionary@
dictionary :: IsMLDictionaryFeatureProvider mlDictionaryFeatureProvider => mlDictionaryFeatureProvider -> IO (Id NSDictionary)
dictionary mlDictionaryFeatureProvider  =
  sendMsg mlDictionaryFeatureProvider (mkSelector "dictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDictionary:error:@
initWithDictionary_errorSelector :: Selector
initWithDictionary_errorSelector = mkSelector "initWithDictionary:error:"

-- | @Selector@ for @objectForKeyedSubscript:@
objectForKeyedSubscriptSelector :: Selector
objectForKeyedSubscriptSelector = mkSelector "objectForKeyedSubscript:"

-- | @Selector@ for @dictionary@
dictionarySelector :: Selector
dictionarySelector = mkSelector "dictionary"

