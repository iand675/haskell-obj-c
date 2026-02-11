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

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.QuartzCore.Internal.Classes

-- | scene
--
-- Specifies the scene of the receiver
--
-- ObjC selector: @- scene@
scene :: IsSCNLayer scnLayer => scnLayer -> IO (Id SCNScene)
scene scnLayer  =
  sendMsg scnLayer (mkSelector "scene") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | scene
--
-- Specifies the scene of the receiver
--
-- ObjC selector: @- setScene:@
setScene :: (IsSCNLayer scnLayer, IsSCNScene value) => scnLayer -> value -> IO ()
setScene scnLayer  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnLayer (mkSelector "setScene:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scene@
sceneSelector :: Selector
sceneSelector = mkSelector "scene"

-- | @Selector@ for @setScene:@
setSceneSelector :: Selector
setSceneSelector = mkSelector "setScene:"

