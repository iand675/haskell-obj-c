{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A concrete convenience class conforming to MLBatchProvider.
--
-- Generated bindings for @MLArrayBatchProvider@.
module ObjC.CoreML.MLArrayBatchProvider
  ( MLArrayBatchProvider
  , IsMLArrayBatchProvider(..)
  , initWithFeatureProviderArray
  , initWithDictionary_error
  , array
  , arraySelector
  , initWithDictionary_errorSelector
  , initWithFeatureProviderArraySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initalize with an array of feature providers
--
-- ObjC selector: @- initWithFeatureProviderArray:@
initWithFeatureProviderArray :: (IsMLArrayBatchProvider mlArrayBatchProvider, IsNSArray array) => mlArrayBatchProvider -> array -> IO (Id MLArrayBatchProvider)
initWithFeatureProviderArray mlArrayBatchProvider array =
  sendOwnedMessage mlArrayBatchProvider initWithFeatureProviderArraySelector (toNSArray array)

-- | Initialize with a dictionary which maps feature names to an array of values [String : [Any]] Error is returned if all arrays do not have equal length or if array values for a specific feature name do not have the same type or not expressible as MLFeatureValue
--
-- ObjC selector: @- initWithDictionary:error:@
initWithDictionary_error :: (IsMLArrayBatchProvider mlArrayBatchProvider, IsNSDictionary dictionary, IsNSError error_) => mlArrayBatchProvider -> dictionary -> error_ -> IO (Id MLArrayBatchProvider)
initWithDictionary_error mlArrayBatchProvider dictionary error_ =
  sendOwnedMessage mlArrayBatchProvider initWithDictionary_errorSelector (toNSDictionary dictionary) (toNSError error_)

-- | @- array@
array :: IsMLArrayBatchProvider mlArrayBatchProvider => mlArrayBatchProvider -> IO (Id NSArray)
array mlArrayBatchProvider =
  sendMessage mlArrayBatchProvider arraySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFeatureProviderArray:@
initWithFeatureProviderArraySelector :: Selector '[Id NSArray] (Id MLArrayBatchProvider)
initWithFeatureProviderArraySelector = mkSelector "initWithFeatureProviderArray:"

-- | @Selector@ for @initWithDictionary:error:@
initWithDictionary_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MLArrayBatchProvider)
initWithDictionary_errorSelector = mkSelector "initWithDictionary:error:"

-- | @Selector@ for @array@
arraySelector :: Selector '[] (Id NSArray)
arraySelector = mkSelector "array"

