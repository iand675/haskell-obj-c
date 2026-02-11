{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SK3DNode@.
module ObjC.SpriteKit.SK3DNode
  ( SK3DNode
  , IsSK3DNode(..)
  , initWithCoder
  , scnScene
  , setScnScene
  , sceneTime
  , setSceneTime
  , playing
  , setPlaying
  , loops
  , setLoops
  , pointOfView
  , setPointOfView
  , autoenablesDefaultLighting
  , setAutoenablesDefaultLighting
  , initWithCoderSelector
  , scnSceneSelector
  , setScnSceneSelector
  , sceneTimeSelector
  , setSceneTimeSelector
  , playingSelector
  , setPlayingSelector
  , loopsSelector
  , setLoopsSelector
  , pointOfViewSelector
  , setPointOfViewSelector
  , autoenablesDefaultLightingSelector
  , setAutoenablesDefaultLightingSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.SceneKit.Internal.Classes

-- | Support coding and decoding via NSKeyedArchiver.
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsSK3DNode sK3DNode, IsNSCoder aDecoder) => sK3DNode -> aDecoder -> IO (Id SK3DNode)
initWithCoder sK3DNode  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg sK3DNode (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | A SceneKit scene
--
-- ObjC selector: @- scnScene@
scnScene :: IsSK3DNode sK3DNode => sK3DNode -> IO (Id SCNScene)
scnScene sK3DNode  =
  sendMsg sK3DNode (mkSelector "scnScene") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A SceneKit scene
--
-- ObjC selector: @- setScnScene:@
setScnScene :: (IsSK3DNode sK3DNode, IsSCNScene value) => sK3DNode -> value -> IO ()
setScnScene sK3DNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg sK3DNode (mkSelector "setScnScene:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | sceneTime
--
-- Specifies the current time to display the scene.
--
-- ObjC selector: @- sceneTime@
sceneTime :: IsSK3DNode sK3DNode => sK3DNode -> IO CDouble
sceneTime sK3DNode  =
  sendMsg sK3DNode (mkSelector "sceneTime") retCDouble []

-- | sceneTime
--
-- Specifies the current time to display the scene.
--
-- ObjC selector: @- setSceneTime:@
setSceneTime :: IsSK3DNode sK3DNode => sK3DNode -> CDouble -> IO ()
setSceneTime sK3DNode  value =
  sendMsg sK3DNode (mkSelector "setSceneTime:") retVoid [argCDouble (fromIntegral value)]

-- | playing
--
-- Returns YES if the scene is playing, NO otherwise.
--
-- ObjC selector: @- playing@
playing :: IsSK3DNode sK3DNode => sK3DNode -> IO Bool
playing sK3DNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg sK3DNode (mkSelector "playing") retCULong []

-- | playing
--
-- Returns YES if the scene is playing, NO otherwise.
--
-- ObjC selector: @- setPlaying:@
setPlaying :: IsSK3DNode sK3DNode => sK3DNode -> Bool -> IO ()
setPlaying sK3DNode  value =
  sendMsg sK3DNode (mkSelector "setPlaying:") retVoid [argCULong (if value then 1 else 0)]

-- | loops
--
-- Indicates whether the receiver restarts playback when it reaches the end of its content. Default: YES.
--
-- YES when the receiver restarts playback when it finishes, NO otherwise.
--
-- ObjC selector: @- loops@
loops :: IsSK3DNode sK3DNode => sK3DNode -> IO Bool
loops sK3DNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg sK3DNode (mkSelector "loops") retCULong []

-- | loops
--
-- Indicates whether the receiver restarts playback when it reaches the end of its content. Default: YES.
--
-- YES when the receiver restarts playback when it finishes, NO otherwise.
--
-- ObjC selector: @- setLoops:@
setLoops :: IsSK3DNode sK3DNode => sK3DNode -> Bool -> IO ()
setLoops sK3DNode  value =
  sendMsg sK3DNode (mkSelector "setLoops:") retVoid [argCULong (if value then 1 else 0)]

-- | pointOfView
--
-- Specifies the point of view used to render the scene.
--
-- A point of view must have either a camera or a spot light attached.
--
-- ObjC selector: @- pointOfView@
pointOfView :: IsSK3DNode sK3DNode => sK3DNode -> IO (Id SCNNode)
pointOfView sK3DNode  =
  sendMsg sK3DNode (mkSelector "pointOfView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | pointOfView
--
-- Specifies the point of view used to render the scene.
--
-- A point of view must have either a camera or a spot light attached.
--
-- ObjC selector: @- setPointOfView:@
setPointOfView :: (IsSK3DNode sK3DNode, IsSCNNode value) => sK3DNode -> value -> IO ()
setPointOfView sK3DNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg sK3DNode (mkSelector "setPointOfView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | autoenablesDefaultLighting
--
-- Specifies whether the receiver should automatically light up scenes that have no light source. The default is NO.
--
-- When enabled, a diffuse light is automatically added and placed while rendering scenes that have no light or only ambient lights.
--
-- ObjC selector: @- autoenablesDefaultLighting@
autoenablesDefaultLighting :: IsSK3DNode sK3DNode => sK3DNode -> IO Bool
autoenablesDefaultLighting sK3DNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg sK3DNode (mkSelector "autoenablesDefaultLighting") retCULong []

-- | autoenablesDefaultLighting
--
-- Specifies whether the receiver should automatically light up scenes that have no light source. The default is NO.
--
-- When enabled, a diffuse light is automatically added and placed while rendering scenes that have no light or only ambient lights.
--
-- ObjC selector: @- setAutoenablesDefaultLighting:@
setAutoenablesDefaultLighting :: IsSK3DNode sK3DNode => sK3DNode -> Bool -> IO ()
setAutoenablesDefaultLighting sK3DNode  value =
  sendMsg sK3DNode (mkSelector "setAutoenablesDefaultLighting:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @scnScene@
scnSceneSelector :: Selector
scnSceneSelector = mkSelector "scnScene"

-- | @Selector@ for @setScnScene:@
setScnSceneSelector :: Selector
setScnSceneSelector = mkSelector "setScnScene:"

-- | @Selector@ for @sceneTime@
sceneTimeSelector :: Selector
sceneTimeSelector = mkSelector "sceneTime"

-- | @Selector@ for @setSceneTime:@
setSceneTimeSelector :: Selector
setSceneTimeSelector = mkSelector "setSceneTime:"

-- | @Selector@ for @playing@
playingSelector :: Selector
playingSelector = mkSelector "playing"

-- | @Selector@ for @setPlaying:@
setPlayingSelector :: Selector
setPlayingSelector = mkSelector "setPlaying:"

-- | @Selector@ for @loops@
loopsSelector :: Selector
loopsSelector = mkSelector "loops"

-- | @Selector@ for @setLoops:@
setLoopsSelector :: Selector
setLoopsSelector = mkSelector "setLoops:"

-- | @Selector@ for @pointOfView@
pointOfViewSelector :: Selector
pointOfViewSelector = mkSelector "pointOfView"

-- | @Selector@ for @setPointOfView:@
setPointOfViewSelector :: Selector
setPointOfViewSelector = mkSelector "setPointOfView:"

-- | @Selector@ for @autoenablesDefaultLighting@
autoenablesDefaultLightingSelector :: Selector
autoenablesDefaultLightingSelector = mkSelector "autoenablesDefaultLighting"

-- | @Selector@ for @setAutoenablesDefaultLighting:@
setAutoenablesDefaultLightingSelector :: Selector
setAutoenablesDefaultLightingSelector = mkSelector "setAutoenablesDefaultLighting:"

