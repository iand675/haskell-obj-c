{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SK3DNode@.
module ObjC.SpriteKit.SK3DNode
  ( SK3DNode
  , IsSK3DNode(..)
  , initWithCoder
  , sceneTime
  , setSceneTime
  , playing
  , setPlaying
  , loops
  , setLoops
  , autoenablesDefaultLighting
  , setAutoenablesDefaultLighting
  , autoenablesDefaultLightingSelector
  , initWithCoderSelector
  , loopsSelector
  , playingSelector
  , sceneTimeSelector
  , setAutoenablesDefaultLightingSelector
  , setLoopsSelector
  , setPlayingSelector
  , setSceneTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Support coding and decoding via NSKeyedArchiver.
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsSK3DNode sK3DNode, IsNSCoder aDecoder) => sK3DNode -> aDecoder -> IO (Id SK3DNode)
initWithCoder sK3DNode aDecoder =
  sendOwnedMessage sK3DNode initWithCoderSelector (toNSCoder aDecoder)

-- | sceneTime
--
-- Specifies the current time to display the scene.
--
-- ObjC selector: @- sceneTime@
sceneTime :: IsSK3DNode sK3DNode => sK3DNode -> IO CDouble
sceneTime sK3DNode =
  sendMessage sK3DNode sceneTimeSelector

-- | sceneTime
--
-- Specifies the current time to display the scene.
--
-- ObjC selector: @- setSceneTime:@
setSceneTime :: IsSK3DNode sK3DNode => sK3DNode -> CDouble -> IO ()
setSceneTime sK3DNode value =
  sendMessage sK3DNode setSceneTimeSelector value

-- | playing
--
-- Returns YES if the scene is playing, NO otherwise.
--
-- ObjC selector: @- playing@
playing :: IsSK3DNode sK3DNode => sK3DNode -> IO Bool
playing sK3DNode =
  sendMessage sK3DNode playingSelector

-- | playing
--
-- Returns YES if the scene is playing, NO otherwise.
--
-- ObjC selector: @- setPlaying:@
setPlaying :: IsSK3DNode sK3DNode => sK3DNode -> Bool -> IO ()
setPlaying sK3DNode value =
  sendMessage sK3DNode setPlayingSelector value

-- | loops
--
-- Indicates whether the receiver restarts playback when it reaches the end of its content. Default: YES.
--
-- YES when the receiver restarts playback when it finishes, NO otherwise.
--
-- ObjC selector: @- loops@
loops :: IsSK3DNode sK3DNode => sK3DNode -> IO Bool
loops sK3DNode =
  sendMessage sK3DNode loopsSelector

-- | loops
--
-- Indicates whether the receiver restarts playback when it reaches the end of its content. Default: YES.
--
-- YES when the receiver restarts playback when it finishes, NO otherwise.
--
-- ObjC selector: @- setLoops:@
setLoops :: IsSK3DNode sK3DNode => sK3DNode -> Bool -> IO ()
setLoops sK3DNode value =
  sendMessage sK3DNode setLoopsSelector value

-- | autoenablesDefaultLighting
--
-- Specifies whether the receiver should automatically light up scenes that have no light source. The default is NO.
--
-- When enabled, a diffuse light is automatically added and placed while rendering scenes that have no light or only ambient lights.
--
-- ObjC selector: @- autoenablesDefaultLighting@
autoenablesDefaultLighting :: IsSK3DNode sK3DNode => sK3DNode -> IO Bool
autoenablesDefaultLighting sK3DNode =
  sendMessage sK3DNode autoenablesDefaultLightingSelector

-- | autoenablesDefaultLighting
--
-- Specifies whether the receiver should automatically light up scenes that have no light source. The default is NO.
--
-- When enabled, a diffuse light is automatically added and placed while rendering scenes that have no light or only ambient lights.
--
-- ObjC selector: @- setAutoenablesDefaultLighting:@
setAutoenablesDefaultLighting :: IsSK3DNode sK3DNode => sK3DNode -> Bool -> IO ()
setAutoenablesDefaultLighting sK3DNode value =
  sendMessage sK3DNode setAutoenablesDefaultLightingSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id SK3DNode)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @sceneTime@
sceneTimeSelector :: Selector '[] CDouble
sceneTimeSelector = mkSelector "sceneTime"

-- | @Selector@ for @setSceneTime:@
setSceneTimeSelector :: Selector '[CDouble] ()
setSceneTimeSelector = mkSelector "setSceneTime:"

-- | @Selector@ for @playing@
playingSelector :: Selector '[] Bool
playingSelector = mkSelector "playing"

-- | @Selector@ for @setPlaying:@
setPlayingSelector :: Selector '[Bool] ()
setPlayingSelector = mkSelector "setPlaying:"

-- | @Selector@ for @loops@
loopsSelector :: Selector '[] Bool
loopsSelector = mkSelector "loops"

-- | @Selector@ for @setLoops:@
setLoopsSelector :: Selector '[Bool] ()
setLoopsSelector = mkSelector "setLoops:"

-- | @Selector@ for @autoenablesDefaultLighting@
autoenablesDefaultLightingSelector :: Selector '[] Bool
autoenablesDefaultLightingSelector = mkSelector "autoenablesDefaultLighting"

-- | @Selector@ for @setAutoenablesDefaultLighting:@
setAutoenablesDefaultLightingSelector :: Selector '[Bool] ()
setAutoenablesDefaultLightingSelector = mkSelector "setAutoenablesDefaultLighting:"

