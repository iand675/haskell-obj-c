{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class that consists of all the levers  to serialize an executable.
--
-- Generated bindings for @MPSGraphExecutableSerializationDescriptor@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphExecutableSerializationDescriptor
  ( MPSGraphExecutableSerializationDescriptor
  , IsMPSGraphExecutableSerializationDescriptor(..)
  , append
  , setAppend
  , deploymentPlatform
  , setDeploymentPlatform
  , minimumDeploymentTarget
  , setMinimumDeploymentTarget
  , appendSelector
  , setAppendSelector
  , deploymentPlatformSelector
  , setDeploymentPlatformSelector
  , minimumDeploymentTargetSelector
  , setMinimumDeploymentTargetSelector

  -- * Enum types
  , MPSGraphDeploymentPlatform(MPSGraphDeploymentPlatform)
  , pattern MPSGraphDeploymentPlatformMacOS
  , pattern MPSGraphDeploymentPlatformIOS
  , pattern MPSGraphDeploymentPlatformTvOS
  , pattern MPSGraphDeploymentPlatformVisionOS

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

import ObjC.MetalPerformanceShadersGraph.Internal.Classes
import ObjC.MetalPerformanceShadersGraph.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Flag to append to an existing .mpsgraphpackage if found at provided url.
--
-- If false, the exisiting .mpsgraphpackage will be overwritten.
--
-- ObjC selector: @- append@
append :: IsMPSGraphExecutableSerializationDescriptor mpsGraphExecutableSerializationDescriptor => mpsGraphExecutableSerializationDescriptor -> IO Bool
append mpsGraphExecutableSerializationDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsGraphExecutableSerializationDescriptor (mkSelector "append") retCULong []

-- | Flag to append to an existing .mpsgraphpackage if found at provided url.
--
-- If false, the exisiting .mpsgraphpackage will be overwritten.
--
-- ObjC selector: @- setAppend:@
setAppend :: IsMPSGraphExecutableSerializationDescriptor mpsGraphExecutableSerializationDescriptor => mpsGraphExecutableSerializationDescriptor -> Bool -> IO ()
setAppend mpsGraphExecutableSerializationDescriptor  value =
  sendMsg mpsGraphExecutableSerializationDescriptor (mkSelector "setAppend:") retVoid [argCULong (if value then 1 else 0)]

-- | The deployment platform used to serialize the executable.
--
-- Defaults to the current platform.
--
-- ObjC selector: @- deploymentPlatform@
deploymentPlatform :: IsMPSGraphExecutableSerializationDescriptor mpsGraphExecutableSerializationDescriptor => mpsGraphExecutableSerializationDescriptor -> IO MPSGraphDeploymentPlatform
deploymentPlatform mpsGraphExecutableSerializationDescriptor  =
  fmap (coerce :: CULong -> MPSGraphDeploymentPlatform) $ sendMsg mpsGraphExecutableSerializationDescriptor (mkSelector "deploymentPlatform") retCULong []

-- | The deployment platform used to serialize the executable.
--
-- Defaults to the current platform.
--
-- ObjC selector: @- setDeploymentPlatform:@
setDeploymentPlatform :: IsMPSGraphExecutableSerializationDescriptor mpsGraphExecutableSerializationDescriptor => mpsGraphExecutableSerializationDescriptor -> MPSGraphDeploymentPlatform -> IO ()
setDeploymentPlatform mpsGraphExecutableSerializationDescriptor  value =
  sendMsg mpsGraphExecutableSerializationDescriptor (mkSelector "setDeploymentPlatform:") retVoid [argCULong (coerce value)]

-- | The minimum deployment target to serialize the executable.
--
-- If not set, the package created will target the latest version of the @deploymentPlatform@ set.
--
-- ObjC selector: @- minimumDeploymentTarget@
minimumDeploymentTarget :: IsMPSGraphExecutableSerializationDescriptor mpsGraphExecutableSerializationDescriptor => mpsGraphExecutableSerializationDescriptor -> IO (Id NSString)
minimumDeploymentTarget mpsGraphExecutableSerializationDescriptor  =
  sendMsg mpsGraphExecutableSerializationDescriptor (mkSelector "minimumDeploymentTarget") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The minimum deployment target to serialize the executable.
--
-- If not set, the package created will target the latest version of the @deploymentPlatform@ set.
--
-- ObjC selector: @- setMinimumDeploymentTarget:@
setMinimumDeploymentTarget :: (IsMPSGraphExecutableSerializationDescriptor mpsGraphExecutableSerializationDescriptor, IsNSString value) => mpsGraphExecutableSerializationDescriptor -> value -> IO ()
setMinimumDeploymentTarget mpsGraphExecutableSerializationDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpsGraphExecutableSerializationDescriptor (mkSelector "setMinimumDeploymentTarget:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @append@
appendSelector :: Selector
appendSelector = mkSelector "append"

-- | @Selector@ for @setAppend:@
setAppendSelector :: Selector
setAppendSelector = mkSelector "setAppend:"

-- | @Selector@ for @deploymentPlatform@
deploymentPlatformSelector :: Selector
deploymentPlatformSelector = mkSelector "deploymentPlatform"

-- | @Selector@ for @setDeploymentPlatform:@
setDeploymentPlatformSelector :: Selector
setDeploymentPlatformSelector = mkSelector "setDeploymentPlatform:"

-- | @Selector@ for @minimumDeploymentTarget@
minimumDeploymentTargetSelector :: Selector
minimumDeploymentTargetSelector = mkSelector "minimumDeploymentTarget"

-- | @Selector@ for @setMinimumDeploymentTarget:@
setMinimumDeploymentTargetSelector :: Selector
setMinimumDeploymentTargetSelector = mkSelector "setMinimumDeploymentTarget:"

