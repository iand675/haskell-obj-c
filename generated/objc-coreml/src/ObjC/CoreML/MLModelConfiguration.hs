{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object to hold options for loading a model.
--
-- Generated bindings for @MLModelConfiguration@.
module ObjC.CoreML.MLModelConfiguration
  ( MLModelConfiguration
  , IsMLModelConfiguration(..)
  , modelDisplayName
  , setModelDisplayName
  , computeUnits
  , setComputeUnits
  , functionName
  , setFunctionName
  , parameters
  , setParameters
  , allowLowPrecisionAccumulationOnGPU
  , setAllowLowPrecisionAccumulationOnGPU
  , modelDisplayNameSelector
  , setModelDisplayNameSelector
  , computeUnitsSelector
  , setComputeUnitsSelector
  , functionNameSelector
  , setFunctionNameSelector
  , parametersSelector
  , setParametersSelector
  , allowLowPrecisionAccumulationOnGPUSelector
  , setAllowLowPrecisionAccumulationOnGPUSelector

  -- * Enum types
  , MLComputeUnits(MLComputeUnits)
  , pattern MLComputeUnitsCPUOnly
  , pattern MLComputeUnitsCPUAndGPU
  , pattern MLComputeUnitsAll
  , pattern MLComputeUnitsCPUAndNeuralEngine

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

-- | A human readable name of a MLModel instance for display purposes.
--
-- Use this property to set a name of a model instance so that runtime analysis tools (e.g. Instruments and os_log) can display that name in the user interface.
--
-- CoreML framework doesn't parse nor filter the text. It is the client's responsibility to use appropriate text, which may involve localization and privacy considerations.
--
-- When the property is nil, CoreML framework provides a default.
--
-- ObjC selector: @- modelDisplayName@
modelDisplayName :: IsMLModelConfiguration mlModelConfiguration => mlModelConfiguration -> IO (Id NSString)
modelDisplayName mlModelConfiguration  =
  sendMsg mlModelConfiguration (mkSelector "modelDisplayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A human readable name of a MLModel instance for display purposes.
--
-- Use this property to set a name of a model instance so that runtime analysis tools (e.g. Instruments and os_log) can display that name in the user interface.
--
-- CoreML framework doesn't parse nor filter the text. It is the client's responsibility to use appropriate text, which may involve localization and privacy considerations.
--
-- When the property is nil, CoreML framework provides a default.
--
-- ObjC selector: @- setModelDisplayName:@
setModelDisplayName :: (IsMLModelConfiguration mlModelConfiguration, IsNSString value) => mlModelConfiguration -> value -> IO ()
setModelDisplayName mlModelConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg mlModelConfiguration (mkSelector "setModelDisplayName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- computeUnits@
computeUnits :: IsMLModelConfiguration mlModelConfiguration => mlModelConfiguration -> IO MLComputeUnits
computeUnits mlModelConfiguration  =
  fmap (coerce :: CLong -> MLComputeUnits) $ sendMsg mlModelConfiguration (mkSelector "computeUnits") retCLong []

-- | @- setComputeUnits:@
setComputeUnits :: IsMLModelConfiguration mlModelConfiguration => mlModelConfiguration -> MLComputeUnits -> IO ()
setComputeUnits mlModelConfiguration  value =
  sendMsg mlModelConfiguration (mkSelector "setComputeUnits:") retVoid [argCLong (coerce value)]

-- | Function name that @MLModel@ will use.
--
-- Some model types (e.g. ML Program) supports multiple functions in a model asset, where each @MLModel@ instance is associated with a particular function.
--
-- Use @MLModelAsset@ to get the list of available functions. Use @nil@ to use a default function.
--
-- ```swift let configuration = MLModelConfiguration() configuration.functionName = "my_function" ```
--
-- ObjC selector: @- functionName@
functionName :: IsMLModelConfiguration mlModelConfiguration => mlModelConfiguration -> IO (Id NSString)
functionName mlModelConfiguration  =
  sendMsg mlModelConfiguration (mkSelector "functionName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Function name that @MLModel@ will use.
--
-- Some model types (e.g. ML Program) supports multiple functions in a model asset, where each @MLModel@ instance is associated with a particular function.
--
-- Use @MLModelAsset@ to get the list of available functions. Use @nil@ to use a default function.
--
-- ```swift let configuration = MLModelConfiguration() configuration.functionName = "my_function" ```
--
-- ObjC selector: @- setFunctionName:@
setFunctionName :: (IsMLModelConfiguration mlModelConfiguration, IsNSString value) => mlModelConfiguration -> value -> IO ()
setFunctionName mlModelConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg mlModelConfiguration (mkSelector "setFunctionName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- parameters@
parameters :: IsMLModelConfiguration mlModelConfiguration => mlModelConfiguration -> IO (Id NSDictionary)
parameters mlModelConfiguration  =
  sendMsg mlModelConfiguration (mkSelector "parameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setParameters:@
setParameters :: (IsMLModelConfiguration mlModelConfiguration, IsNSDictionary value) => mlModelConfiguration -> value -> IO ()
setParameters mlModelConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg mlModelConfiguration (mkSelector "setParameters:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Set to YES to allow low precision accumulation on GPU when available. Defaults to NO
--
-- ObjC selector: @- allowLowPrecisionAccumulationOnGPU@
allowLowPrecisionAccumulationOnGPU :: IsMLModelConfiguration mlModelConfiguration => mlModelConfiguration -> IO Bool
allowLowPrecisionAccumulationOnGPU mlModelConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlModelConfiguration (mkSelector "allowLowPrecisionAccumulationOnGPU") retCULong []

-- | Set to YES to allow low precision accumulation on GPU when available. Defaults to NO
--
-- ObjC selector: @- setAllowLowPrecisionAccumulationOnGPU:@
setAllowLowPrecisionAccumulationOnGPU :: IsMLModelConfiguration mlModelConfiguration => mlModelConfiguration -> Bool -> IO ()
setAllowLowPrecisionAccumulationOnGPU mlModelConfiguration  value =
  sendMsg mlModelConfiguration (mkSelector "setAllowLowPrecisionAccumulationOnGPU:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @modelDisplayName@
modelDisplayNameSelector :: Selector
modelDisplayNameSelector = mkSelector "modelDisplayName"

-- | @Selector@ for @setModelDisplayName:@
setModelDisplayNameSelector :: Selector
setModelDisplayNameSelector = mkSelector "setModelDisplayName:"

-- | @Selector@ for @computeUnits@
computeUnitsSelector :: Selector
computeUnitsSelector = mkSelector "computeUnits"

-- | @Selector@ for @setComputeUnits:@
setComputeUnitsSelector :: Selector
setComputeUnitsSelector = mkSelector "setComputeUnits:"

-- | @Selector@ for @functionName@
functionNameSelector :: Selector
functionNameSelector = mkSelector "functionName"

-- | @Selector@ for @setFunctionName:@
setFunctionNameSelector :: Selector
setFunctionNameSelector = mkSelector "setFunctionName:"

-- | @Selector@ for @parameters@
parametersSelector :: Selector
parametersSelector = mkSelector "parameters"

-- | @Selector@ for @setParameters:@
setParametersSelector :: Selector
setParametersSelector = mkSelector "setParameters:"

-- | @Selector@ for @allowLowPrecisionAccumulationOnGPU@
allowLowPrecisionAccumulationOnGPUSelector :: Selector
allowLowPrecisionAccumulationOnGPUSelector = mkSelector "allowLowPrecisionAccumulationOnGPU"

-- | @Selector@ for @setAllowLowPrecisionAccumulationOnGPU:@
setAllowLowPrecisionAccumulationOnGPUSelector :: Selector
setAllowLowPrecisionAccumulationOnGPUSelector = mkSelector "setAllowLowPrecisionAccumulationOnGPU:"

