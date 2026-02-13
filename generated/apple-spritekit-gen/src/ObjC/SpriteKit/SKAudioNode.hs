{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A SpriteKit scene graph audio node that provides a way to link audio graphs to a SpriteKit scene. The currently presented scene is responsible for mixing the audio from nodes in the scene.
--
-- Positional sounds will use their relative location and velocity to the scene's listener to apply distance attenuation, doppler shift and pan.
--
-- See: AVAudio3DMixing
--
-- See: SKScene.listener
--
-- Generated bindings for @SKAudioNode@.
module ObjC.SpriteKit.SKAudioNode
  ( SKAudioNode
  , IsSKAudioNode(..)
  , initWithAVAudioNode
  , initWithCoder
  , initWithFileNamed
  , initWithURL
  , avAudioNode
  , setAvAudioNode
  , autoplayLooped
  , setAutoplayLooped
  , positional
  , setPositional
  , autoplayLoopedSelector
  , avAudioNodeSelector
  , initWithAVAudioNodeSelector
  , initWithCoderSelector
  , initWithFileNamedSelector
  , initWithURLSelector
  , positionalSelector
  , setAutoplayLoopedSelector
  , setAvAudioNodeSelector
  , setPositionalSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.AVFAudio.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a SpriteKit scene graph audio node from the given AVAudioNode.
--
-- See: AVAudioNode
--
-- ObjC selector: @- initWithAVAudioNode:@
initWithAVAudioNode :: (IsSKAudioNode skAudioNode, IsAVAudioNode node) => skAudioNode -> node -> IO (Id SKAudioNode)
initWithAVAudioNode skAudioNode node =
  sendOwnedMessage skAudioNode initWithAVAudioNodeSelector (toAVAudioNode node)

-- | @- initWithCoder:@
initWithCoder :: (IsSKAudioNode skAudioNode, IsNSCoder aDecoder) => skAudioNode -> aDecoder -> IO (Id SKAudioNode)
initWithCoder skAudioNode aDecoder =
  sendOwnedMessage skAudioNode initWithCoderSelector (toNSCoder aDecoder)

-- | Convenience initializer that creates an AVAudioNode from the named audio asset in the main bundle.
--
-- See: initWithAVAudioNode
--
-- ObjC selector: @- initWithFileNamed:@
initWithFileNamed :: (IsSKAudioNode skAudioNode, IsNSString name) => skAudioNode -> name -> IO (Id SKAudioNode)
initWithFileNamed skAudioNode name =
  sendOwnedMessage skAudioNode initWithFileNamedSelector (toNSString name)

-- | Convenience initializer that creates an AVAudioNode from the URL that contain a audio asset.
--
-- See: initWithAVAudioNode
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsSKAudioNode skAudioNode, IsNSURL url) => skAudioNode -> url -> IO (Id SKAudioNode)
initWithURL skAudioNode url =
  sendOwnedMessage skAudioNode initWithURLSelector (toNSURL url)

-- | Sets or gets the current AVAudioNode used by this instance.
--
-- ObjC selector: @- avAudioNode@
avAudioNode :: IsSKAudioNode skAudioNode => skAudioNode -> IO (Id AVAudioNode)
avAudioNode skAudioNode =
  sendMessage skAudioNode avAudioNodeSelector

-- | Sets or gets the current AVAudioNode used by this instance.
--
-- ObjC selector: @- setAvAudioNode:@
setAvAudioNode :: (IsSKAudioNode skAudioNode, IsAVAudioNode value) => skAudioNode -> value -> IO ()
setAvAudioNode skAudioNode value =
  sendMessage skAudioNode setAvAudioNodeSelector (toAVAudioNode value)

-- | Specifies whether the node is to automatically play sound when added to a scene. If autoplaysLooped is NO, the node and its sound must be explicitly scheduled and played using the scene's engine.
--
-- If YES, the node will automatically play sound when added to a scene.
--
-- Defaults to YES.
--
-- See: SKView.paused
--
-- ObjC selector: @- autoplayLooped@
autoplayLooped :: IsSKAudioNode skAudioNode => skAudioNode -> IO Bool
autoplayLooped skAudioNode =
  sendMessage skAudioNode autoplayLoopedSelector

-- | Specifies whether the node is to automatically play sound when added to a scene. If autoplaysLooped is NO, the node and its sound must be explicitly scheduled and played using the scene's engine.
--
-- If YES, the node will automatically play sound when added to a scene.
--
-- Defaults to YES.
--
-- See: SKView.paused
--
-- ObjC selector: @- setAutoplayLooped:@
setAutoplayLooped :: IsSKAudioNode skAudioNode => skAudioNode -> Bool -> IO ()
setAutoplayLooped skAudioNode value =
  sendMessage skAudioNode setAutoplayLoopedSelector value

-- | Marks the audio source as positional so that the audio mix considers relative position and velocity with regards to the scene's current listener node.
--
-- See: AVAudio3DMixing
--
-- See: SKScene.listener
--
-- ObjC selector: @- positional@
positional :: IsSKAudioNode skAudioNode => skAudioNode -> IO Bool
positional skAudioNode =
  sendMessage skAudioNode positionalSelector

-- | Marks the audio source as positional so that the audio mix considers relative position and velocity with regards to the scene's current listener node.
--
-- See: AVAudio3DMixing
--
-- See: SKScene.listener
--
-- ObjC selector: @- setPositional:@
setPositional :: IsSKAudioNode skAudioNode => skAudioNode -> Bool -> IO ()
setPositional skAudioNode value =
  sendMessage skAudioNode setPositionalSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAVAudioNode:@
initWithAVAudioNodeSelector :: Selector '[Id AVAudioNode] (Id SKAudioNode)
initWithAVAudioNodeSelector = mkSelector "initWithAVAudioNode:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id SKAudioNode)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithFileNamed:@
initWithFileNamedSelector :: Selector '[Id NSString] (Id SKAudioNode)
initWithFileNamedSelector = mkSelector "initWithFileNamed:"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector '[Id NSURL] (Id SKAudioNode)
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @avAudioNode@
avAudioNodeSelector :: Selector '[] (Id AVAudioNode)
avAudioNodeSelector = mkSelector "avAudioNode"

-- | @Selector@ for @setAvAudioNode:@
setAvAudioNodeSelector :: Selector '[Id AVAudioNode] ()
setAvAudioNodeSelector = mkSelector "setAvAudioNode:"

-- | @Selector@ for @autoplayLooped@
autoplayLoopedSelector :: Selector '[] Bool
autoplayLoopedSelector = mkSelector "autoplayLooped"

-- | @Selector@ for @setAutoplayLooped:@
setAutoplayLoopedSelector :: Selector '[Bool] ()
setAutoplayLoopedSelector = mkSelector "setAutoplayLooped:"

-- | @Selector@ for @positional@
positionalSelector :: Selector '[] Bool
positionalSelector = mkSelector "positional"

-- | @Selector@ for @setPositional:@
setPositionalSelector :: Selector '[Bool] ()
setPositionalSelector = mkSelector "setPositional:"

