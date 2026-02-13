{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNLayer
--
-- A SCNLayer is a layer that can display a SCNScene.
--
-- Generated bindings for @SCNLayer@.
module ObjC.SceneKit.SCNLayer
  ( SCNLayer
  , IsSCNLayer(..)
  , scene
  , setScene
  , sceneSelector
  , setSceneSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- | scene
--
-- Specifies the scene of the receiver
--
-- ObjC selector: @- scene@
scene :: IsSCNLayer scnLayer => scnLayer -> IO (Id SCNScene)
scene scnLayer =
  sendMessage scnLayer sceneSelector

-- | scene
--
-- Specifies the scene of the receiver
--
-- ObjC selector: @- setScene:@
setScene :: (IsSCNLayer scnLayer, IsSCNScene value) => scnLayer -> value -> IO ()
setScene scnLayer value =
  sendMessage scnLayer setSceneSelector (toSCNScene value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scene@
sceneSelector :: Selector '[] (Id SCNScene)
sceneSelector = mkSelector "scene"

-- | @Selector@ for @setScene:@
setSceneSelector :: Selector '[Id SCNScene] ()
setSceneSelector = mkSelector "setScene:"

