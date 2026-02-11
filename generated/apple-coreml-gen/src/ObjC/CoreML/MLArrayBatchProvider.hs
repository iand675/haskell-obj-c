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
  , initWithFeatureProviderArraySelector
  , initWithDictionary_errorSelector
  , arraySelector


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

-- | Initalize with an array of feature providers
--
-- ObjC selector: @- initWithFeatureProviderArray:@
initWithFeatureProviderArray :: (IsMLArrayBatchProvider mlArrayBatchProvider, IsNSArray array) => mlArrayBatchProvider -> array -> IO (Id MLArrayBatchProvider)
initWithFeatureProviderArray mlArrayBatchProvider  array =
  withObjCPtr array $ \raw_array ->
      sendMsg mlArrayBatchProvider (mkSelector "initWithFeatureProviderArray:") (retPtr retVoid) [argPtr (castPtr raw_array :: Ptr ())] >>= ownedObject . castPtr

-- | Initialize with a dictionary which maps feature names to an array of values [String : [Any]] Error is returned if all arrays do not have equal length or if array values for a specific feature name do not have the same type or not expressible as MLFeatureValue
--
-- ObjC selector: @- initWithDictionary:error:@
initWithDictionary_error :: (IsMLArrayBatchProvider mlArrayBatchProvider, IsNSDictionary dictionary, IsNSError error_) => mlArrayBatchProvider -> dictionary -> error_ -> IO (Id MLArrayBatchProvider)
initWithDictionary_error mlArrayBatchProvider  dictionary error_ =
  withObjCPtr dictionary $ \raw_dictionary ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mlArrayBatchProvider (mkSelector "initWithDictionary:error:") (retPtr retVoid) [argPtr (castPtr raw_dictionary :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- array@
array :: IsMLArrayBatchProvider mlArrayBatchProvider => mlArrayBatchProvider -> IO (Id NSArray)
array mlArrayBatchProvider  =
    sendMsg mlArrayBatchProvider (mkSelector "array") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFeatureProviderArray:@
initWithFeatureProviderArraySelector :: Selector
initWithFeatureProviderArraySelector = mkSelector "initWithFeatureProviderArray:"

-- | @Selector@ for @initWithDictionary:error:@
initWithDictionary_errorSelector :: Selector
initWithDictionary_errorSelector = mkSelector "initWithDictionary:error:"

-- | @Selector@ for @array@
arraySelector :: Selector
arraySelector = mkSelector "array"

