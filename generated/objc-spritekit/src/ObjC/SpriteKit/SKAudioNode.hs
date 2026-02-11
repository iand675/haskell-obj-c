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
  , initWithAVAudioNodeSelector
  , initWithCoderSelector
  , initWithFileNamedSelector
  , initWithURLSelector
  , avAudioNodeSelector
  , setAvAudioNodeSelector
  , autoplayLoopedSelector
  , setAutoplayLoopedSelector
  , positionalSelector
  , setPositionalSelector


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
initWithAVAudioNode skAudioNode  node =
withObjCPtr node $ \raw_node ->
    sendMsg skAudioNode (mkSelector "initWithAVAudioNode:") (retPtr retVoid) [argPtr (castPtr raw_node :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsSKAudioNode skAudioNode, IsNSCoder aDecoder) => skAudioNode -> aDecoder -> IO (Id SKAudioNode)
initWithCoder skAudioNode  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg skAudioNode (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | Convenience initializer that creates an AVAudioNode from the named audio asset in the main bundle.
--
-- See: initWithAVAudioNode
--
-- ObjC selector: @- initWithFileNamed:@
initWithFileNamed :: (IsSKAudioNode skAudioNode, IsNSString name) => skAudioNode -> name -> IO (Id SKAudioNode)
initWithFileNamed skAudioNode  name =
withObjCPtr name $ \raw_name ->
    sendMsg skAudioNode (mkSelector "initWithFileNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= ownedObject . castPtr

-- | Convenience initializer that creates an AVAudioNode from the URL that contain a audio asset.
--
-- See: initWithAVAudioNode
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsSKAudioNode skAudioNode, IsNSURL url) => skAudioNode -> url -> IO (Id SKAudioNode)
initWithURL skAudioNode  url =
withObjCPtr url $ \raw_url ->
    sendMsg skAudioNode (mkSelector "initWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | Sets or gets the current AVAudioNode used by this instance.
--
-- ObjC selector: @- avAudioNode@
avAudioNode :: IsSKAudioNode skAudioNode => skAudioNode -> IO (Id AVAudioNode)
avAudioNode skAudioNode  =
  sendMsg skAudioNode (mkSelector "avAudioNode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets or gets the current AVAudioNode used by this instance.
--
-- ObjC selector: @- setAvAudioNode:@
setAvAudioNode :: (IsSKAudioNode skAudioNode, IsAVAudioNode value) => skAudioNode -> value -> IO ()
setAvAudioNode skAudioNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg skAudioNode (mkSelector "setAvAudioNode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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
autoplayLooped skAudioNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skAudioNode (mkSelector "autoplayLooped") retCULong []

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
setAutoplayLooped skAudioNode  value =
  sendMsg skAudioNode (mkSelector "setAutoplayLooped:") retVoid [argCULong (if value then 1 else 0)]

-- | Marks the audio source as positional so that the audio mix considers relative position and velocity with regards to the scene's current listener node.
--
-- See: AVAudio3DMixing
--
-- See: SKScene.listener
--
-- ObjC selector: @- positional@
positional :: IsSKAudioNode skAudioNode => skAudioNode -> IO Bool
positional skAudioNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg skAudioNode (mkSelector "positional") retCULong []

-- | Marks the audio source as positional so that the audio mix considers relative position and velocity with regards to the scene's current listener node.
--
-- See: AVAudio3DMixing
--
-- See: SKScene.listener
--
-- ObjC selector: @- setPositional:@
setPositional :: IsSKAudioNode skAudioNode => skAudioNode -> Bool -> IO ()
setPositional skAudioNode  value =
  sendMsg skAudioNode (mkSelector "setPositional:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAVAudioNode:@
initWithAVAudioNodeSelector :: Selector
initWithAVAudioNodeSelector = mkSelector "initWithAVAudioNode:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithFileNamed:@
initWithFileNamedSelector :: Selector
initWithFileNamedSelector = mkSelector "initWithFileNamed:"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @avAudioNode@
avAudioNodeSelector :: Selector
avAudioNodeSelector = mkSelector "avAudioNode"

-- | @Selector@ for @setAvAudioNode:@
setAvAudioNodeSelector :: Selector
setAvAudioNodeSelector = mkSelector "setAvAudioNode:"

-- | @Selector@ for @autoplayLooped@
autoplayLoopedSelector :: Selector
autoplayLoopedSelector = mkSelector "autoplayLooped"

-- | @Selector@ for @setAutoplayLooped:@
setAutoplayLoopedSelector :: Selector
setAutoplayLoopedSelector = mkSelector "setAutoplayLooped:"

-- | @Selector@ for @positional@
positionalSelector :: Selector
positionalSelector = mkSelector "positional"

-- | @Selector@ for @setPositional:@
setPositionalSelector :: Selector
setPositionalSelector = mkSelector "setPositional:"

