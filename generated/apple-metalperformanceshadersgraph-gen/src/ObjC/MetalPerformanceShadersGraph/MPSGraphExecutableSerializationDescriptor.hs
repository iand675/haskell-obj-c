{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , deploymentPlatformSelector
  , minimumDeploymentTargetSelector
  , setAppendSelector
  , setDeploymentPlatformSelector
  , setMinimumDeploymentTargetSelector

  -- * Enum types
  , MPSGraphDeploymentPlatform(MPSGraphDeploymentPlatform)
  , pattern MPSGraphDeploymentPlatformMacOS
  , pattern MPSGraphDeploymentPlatformIOS
  , pattern MPSGraphDeploymentPlatformTvOS
  , pattern MPSGraphDeploymentPlatformVisionOS

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
append mpsGraphExecutableSerializationDescriptor =
  sendMessage mpsGraphExecutableSerializationDescriptor appendSelector

-- | Flag to append to an existing .mpsgraphpackage if found at provided url.
--
-- If false, the exisiting .mpsgraphpackage will be overwritten.
--
-- ObjC selector: @- setAppend:@
setAppend :: IsMPSGraphExecutableSerializationDescriptor mpsGraphExecutableSerializationDescriptor => mpsGraphExecutableSerializationDescriptor -> Bool -> IO ()
setAppend mpsGraphExecutableSerializationDescriptor value =
  sendMessage mpsGraphExecutableSerializationDescriptor setAppendSelector value

-- | The deployment platform used to serialize the executable.
--
-- Defaults to the current platform.
--
-- ObjC selector: @- deploymentPlatform@
deploymentPlatform :: IsMPSGraphExecutableSerializationDescriptor mpsGraphExecutableSerializationDescriptor => mpsGraphExecutableSerializationDescriptor -> IO MPSGraphDeploymentPlatform
deploymentPlatform mpsGraphExecutableSerializationDescriptor =
  sendMessage mpsGraphExecutableSerializationDescriptor deploymentPlatformSelector

-- | The deployment platform used to serialize the executable.
--
-- Defaults to the current platform.
--
-- ObjC selector: @- setDeploymentPlatform:@
setDeploymentPlatform :: IsMPSGraphExecutableSerializationDescriptor mpsGraphExecutableSerializationDescriptor => mpsGraphExecutableSerializationDescriptor -> MPSGraphDeploymentPlatform -> IO ()
setDeploymentPlatform mpsGraphExecutableSerializationDescriptor value =
  sendMessage mpsGraphExecutableSerializationDescriptor setDeploymentPlatformSelector value

-- | The minimum deployment target to serialize the executable.
--
-- If not set, the package created will target the latest version of the @deploymentPlatform@ set.
--
-- ObjC selector: @- minimumDeploymentTarget@
minimumDeploymentTarget :: IsMPSGraphExecutableSerializationDescriptor mpsGraphExecutableSerializationDescriptor => mpsGraphExecutableSerializationDescriptor -> IO (Id NSString)
minimumDeploymentTarget mpsGraphExecutableSerializationDescriptor =
  sendMessage mpsGraphExecutableSerializationDescriptor minimumDeploymentTargetSelector

-- | The minimum deployment target to serialize the executable.
--
-- If not set, the package created will target the latest version of the @deploymentPlatform@ set.
--
-- ObjC selector: @- setMinimumDeploymentTarget:@
setMinimumDeploymentTarget :: (IsMPSGraphExecutableSerializationDescriptor mpsGraphExecutableSerializationDescriptor, IsNSString value) => mpsGraphExecutableSerializationDescriptor -> value -> IO ()
setMinimumDeploymentTarget mpsGraphExecutableSerializationDescriptor value =
  sendMessage mpsGraphExecutableSerializationDescriptor setMinimumDeploymentTargetSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @append@
appendSelector :: Selector '[] Bool
appendSelector = mkSelector "append"

-- | @Selector@ for @setAppend:@
setAppendSelector :: Selector '[Bool] ()
setAppendSelector = mkSelector "setAppend:"

-- | @Selector@ for @deploymentPlatform@
deploymentPlatformSelector :: Selector '[] MPSGraphDeploymentPlatform
deploymentPlatformSelector = mkSelector "deploymentPlatform"

-- | @Selector@ for @setDeploymentPlatform:@
setDeploymentPlatformSelector :: Selector '[MPSGraphDeploymentPlatform] ()
setDeploymentPlatformSelector = mkSelector "setDeploymentPlatform:"

-- | @Selector@ for @minimumDeploymentTarget@
minimumDeploymentTargetSelector :: Selector '[] (Id NSString)
minimumDeploymentTargetSelector = mkSelector "minimumDeploymentTarget"

-- | @Selector@ for @setMinimumDeploymentTarget:@
setMinimumDeploymentTargetSelector :: Selector '[Id NSString] ()
setMinimumDeploymentTargetSelector = mkSelector "setMinimumDeploymentTarget:"

