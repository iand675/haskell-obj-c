{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterSceneInfoStruct@.
module ObjC.Matter.MTRScenesManagementClusterSceneInfoStruct
  ( MTRScenesManagementClusterSceneInfoStruct
  , IsMTRScenesManagementClusterSceneInfoStruct(..)
  , sceneCount
  , setSceneCount
  , currentScene
  , setCurrentScene
  , currentGroup
  , setCurrentGroup
  , sceneValid
  , setSceneValid
  , remainingCapacity
  , setRemainingCapacity
  , fabricIndex
  , setFabricIndex
  , currentGroupSelector
  , currentSceneSelector
  , fabricIndexSelector
  , remainingCapacitySelector
  , sceneCountSelector
  , sceneValidSelector
  , setCurrentGroupSelector
  , setCurrentSceneSelector
  , setFabricIndexSelector
  , setRemainingCapacitySelector
  , setSceneCountSelector
  , setSceneValidSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- sceneCount@
sceneCount :: IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct => mtrScenesManagementClusterSceneInfoStruct -> IO (Id NSNumber)
sceneCount mtrScenesManagementClusterSceneInfoStruct =
  sendMessage mtrScenesManagementClusterSceneInfoStruct sceneCountSelector

-- | @- setSceneCount:@
setSceneCount :: (IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct, IsNSNumber value) => mtrScenesManagementClusterSceneInfoStruct -> value -> IO ()
setSceneCount mtrScenesManagementClusterSceneInfoStruct value =
  sendMessage mtrScenesManagementClusterSceneInfoStruct setSceneCountSelector (toNSNumber value)

-- | @- currentScene@
currentScene :: IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct => mtrScenesManagementClusterSceneInfoStruct -> IO (Id NSNumber)
currentScene mtrScenesManagementClusterSceneInfoStruct =
  sendMessage mtrScenesManagementClusterSceneInfoStruct currentSceneSelector

-- | @- setCurrentScene:@
setCurrentScene :: (IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct, IsNSNumber value) => mtrScenesManagementClusterSceneInfoStruct -> value -> IO ()
setCurrentScene mtrScenesManagementClusterSceneInfoStruct value =
  sendMessage mtrScenesManagementClusterSceneInfoStruct setCurrentSceneSelector (toNSNumber value)

-- | @- currentGroup@
currentGroup :: IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct => mtrScenesManagementClusterSceneInfoStruct -> IO (Id NSNumber)
currentGroup mtrScenesManagementClusterSceneInfoStruct =
  sendMessage mtrScenesManagementClusterSceneInfoStruct currentGroupSelector

-- | @- setCurrentGroup:@
setCurrentGroup :: (IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct, IsNSNumber value) => mtrScenesManagementClusterSceneInfoStruct -> value -> IO ()
setCurrentGroup mtrScenesManagementClusterSceneInfoStruct value =
  sendMessage mtrScenesManagementClusterSceneInfoStruct setCurrentGroupSelector (toNSNumber value)

-- | @- sceneValid@
sceneValid :: IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct => mtrScenesManagementClusterSceneInfoStruct -> IO (Id NSNumber)
sceneValid mtrScenesManagementClusterSceneInfoStruct =
  sendMessage mtrScenesManagementClusterSceneInfoStruct sceneValidSelector

-- | @- setSceneValid:@
setSceneValid :: (IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct, IsNSNumber value) => mtrScenesManagementClusterSceneInfoStruct -> value -> IO ()
setSceneValid mtrScenesManagementClusterSceneInfoStruct value =
  sendMessage mtrScenesManagementClusterSceneInfoStruct setSceneValidSelector (toNSNumber value)

-- | @- remainingCapacity@
remainingCapacity :: IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct => mtrScenesManagementClusterSceneInfoStruct -> IO (Id NSNumber)
remainingCapacity mtrScenesManagementClusterSceneInfoStruct =
  sendMessage mtrScenesManagementClusterSceneInfoStruct remainingCapacitySelector

-- | @- setRemainingCapacity:@
setRemainingCapacity :: (IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct, IsNSNumber value) => mtrScenesManagementClusterSceneInfoStruct -> value -> IO ()
setRemainingCapacity mtrScenesManagementClusterSceneInfoStruct value =
  sendMessage mtrScenesManagementClusterSceneInfoStruct setRemainingCapacitySelector (toNSNumber value)

-- | @- fabricIndex@
fabricIndex :: IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct => mtrScenesManagementClusterSceneInfoStruct -> IO (Id NSNumber)
fabricIndex mtrScenesManagementClusterSceneInfoStruct =
  sendMessage mtrScenesManagementClusterSceneInfoStruct fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRScenesManagementClusterSceneInfoStruct mtrScenesManagementClusterSceneInfoStruct, IsNSNumber value) => mtrScenesManagementClusterSceneInfoStruct -> value -> IO ()
setFabricIndex mtrScenesManagementClusterSceneInfoStruct value =
  sendMessage mtrScenesManagementClusterSceneInfoStruct setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sceneCount@
sceneCountSelector :: Selector '[] (Id NSNumber)
sceneCountSelector = mkSelector "sceneCount"

-- | @Selector@ for @setSceneCount:@
setSceneCountSelector :: Selector '[Id NSNumber] ()
setSceneCountSelector = mkSelector "setSceneCount:"

-- | @Selector@ for @currentScene@
currentSceneSelector :: Selector '[] (Id NSNumber)
currentSceneSelector = mkSelector "currentScene"

-- | @Selector@ for @setCurrentScene:@
setCurrentSceneSelector :: Selector '[Id NSNumber] ()
setCurrentSceneSelector = mkSelector "setCurrentScene:"

-- | @Selector@ for @currentGroup@
currentGroupSelector :: Selector '[] (Id NSNumber)
currentGroupSelector = mkSelector "currentGroup"

-- | @Selector@ for @setCurrentGroup:@
setCurrentGroupSelector :: Selector '[Id NSNumber] ()
setCurrentGroupSelector = mkSelector "setCurrentGroup:"

-- | @Selector@ for @sceneValid@
sceneValidSelector :: Selector '[] (Id NSNumber)
sceneValidSelector = mkSelector "sceneValid"

-- | @Selector@ for @setSceneValid:@
setSceneValidSelector :: Selector '[Id NSNumber] ()
setSceneValidSelector = mkSelector "setSceneValid:"

-- | @Selector@ for @remainingCapacity@
remainingCapacitySelector :: Selector '[] (Id NSNumber)
remainingCapacitySelector = mkSelector "remainingCapacity"

-- | @Selector@ for @setRemainingCapacity:@
setRemainingCapacitySelector :: Selector '[Id NSNumber] ()
setRemainingCapacitySelector = mkSelector "setRemainingCapacity:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

