{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKVideoNode@.
module ObjC.SpriteKit.SKVideoNode
  ( SKVideoNode
  , IsSKVideoNode(..)
  , videoNodeWithAVPlayer
  , videoNodeWithVideoFileNamed
  , videoNodeWithFileNamed
  , videoNodeWithVideoURL
  , videoNodeWithURL
  , initWithAVPlayer
  , initWithVideoFileNamed
  , initWithFileNamed
  , initWithVideoURL
  , initWithURL
  , initWithCoder
  , play
  , pause
  , videoNodeWithAVPlayerSelector
  , videoNodeWithVideoFileNamedSelector
  , videoNodeWithFileNamedSelector
  , videoNodeWithVideoURLSelector
  , videoNodeWithURLSelector
  , initWithAVPlayerSelector
  , initWithVideoFileNamedSelector
  , initWithFileNamedSelector
  , initWithVideoURLSelector
  , initWithURLSelector
  , initWithCoderSelector
  , playSelector
  , pauseSelector


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
import ObjC.AVFoundation.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a video node from an AVPlayer. You can use the AVPlayer to control playback.
--
-- ObjC selector: @+ videoNodeWithAVPlayer:@
videoNodeWithAVPlayer :: IsAVPlayer player => player -> IO (Id SKVideoNode)
videoNodeWithAVPlayer player =
  do
    cls' <- getRequiredClass "SKVideoNode"
    withObjCPtr player $ \raw_player ->
      sendClassMsg cls' (mkSelector "videoNodeWithAVPlayer:") (retPtr retVoid) [argPtr (castPtr raw_player :: Ptr ())] >>= retainedObject . castPtr

-- | Create a video node from a file.
--
-- ObjC selector: @+ videoNodeWithVideoFileNamed:@
videoNodeWithVideoFileNamed :: IsNSString videoFile => videoFile -> IO (Id SKVideoNode)
videoNodeWithVideoFileNamed videoFile =
  do
    cls' <- getRequiredClass "SKVideoNode"
    withObjCPtr videoFile $ \raw_videoFile ->
      sendClassMsg cls' (mkSelector "videoNodeWithVideoFileNamed:") (retPtr retVoid) [argPtr (castPtr raw_videoFile :: Ptr ())] >>= retainedObject . castPtr

-- | @+ videoNodeWithFileNamed:@
videoNodeWithFileNamed :: IsNSString videoFile => videoFile -> IO (Id SKVideoNode)
videoNodeWithFileNamed videoFile =
  do
    cls' <- getRequiredClass "SKVideoNode"
    withObjCPtr videoFile $ \raw_videoFile ->
      sendClassMsg cls' (mkSelector "videoNodeWithFileNamed:") (retPtr retVoid) [argPtr (castPtr raw_videoFile :: Ptr ())] >>= retainedObject . castPtr

-- | Create a video node from a URL.
--
-- ObjC selector: @+ videoNodeWithVideoURL:@
videoNodeWithVideoURL :: IsNSURL videoURL => videoURL -> IO (Id SKVideoNode)
videoNodeWithVideoURL videoURL =
  do
    cls' <- getRequiredClass "SKVideoNode"
    withObjCPtr videoURL $ \raw_videoURL ->
      sendClassMsg cls' (mkSelector "videoNodeWithVideoURL:") (retPtr retVoid) [argPtr (castPtr raw_videoURL :: Ptr ())] >>= retainedObject . castPtr

-- | @+ videoNodeWithURL:@
videoNodeWithURL :: IsNSURL videoURL => videoURL -> IO (Id SKVideoNode)
videoNodeWithURL videoURL =
  do
    cls' <- getRequiredClass "SKVideoNode"
    withObjCPtr videoURL $ \raw_videoURL ->
      sendClassMsg cls' (mkSelector "videoNodeWithURL:") (retPtr retVoid) [argPtr (castPtr raw_videoURL :: Ptr ())] >>= retainedObject . castPtr

-- | Designated Initializer.
--
-- Initialize a video node from an AVPlayer. You can use the AVPlayer to control playback.
--
-- ObjC selector: @- initWithAVPlayer:@
initWithAVPlayer :: (IsSKVideoNode skVideoNode, IsAVPlayer player) => skVideoNode -> player -> IO (Id SKVideoNode)
initWithAVPlayer skVideoNode  player =
withObjCPtr player $ \raw_player ->
    sendMsg skVideoNode (mkSelector "initWithAVPlayer:") (retPtr retVoid) [argPtr (castPtr raw_player :: Ptr ())] >>= ownedObject . castPtr

-- | Initialize a video node from a file.
--
-- ObjC selector: @- initWithVideoFileNamed:@
initWithVideoFileNamed :: (IsSKVideoNode skVideoNode, IsNSString videoFile) => skVideoNode -> videoFile -> IO (Id SKVideoNode)
initWithVideoFileNamed skVideoNode  videoFile =
withObjCPtr videoFile $ \raw_videoFile ->
    sendMsg skVideoNode (mkSelector "initWithVideoFileNamed:") (retPtr retVoid) [argPtr (castPtr raw_videoFile :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithFileNamed:@
initWithFileNamed :: (IsSKVideoNode skVideoNode, IsNSString videoFile) => skVideoNode -> videoFile -> IO (Id SKVideoNode)
initWithFileNamed skVideoNode  videoFile =
withObjCPtr videoFile $ \raw_videoFile ->
    sendMsg skVideoNode (mkSelector "initWithFileNamed:") (retPtr retVoid) [argPtr (castPtr raw_videoFile :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithVideoURL:@
initWithVideoURL :: (IsSKVideoNode skVideoNode, IsNSURL url) => skVideoNode -> url -> IO (Id SKVideoNode)
initWithVideoURL skVideoNode  url =
withObjCPtr url $ \raw_url ->
    sendMsg skVideoNode (mkSelector "initWithVideoURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithURL:@
initWithURL :: (IsSKVideoNode skVideoNode, IsNSURL url) => skVideoNode -> url -> IO (Id SKVideoNode)
initWithURL skVideoNode  url =
withObjCPtr url $ \raw_url ->
    sendMsg skVideoNode (mkSelector "initWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | Support coding and decoding via NSKeyedArchiver.
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsSKVideoNode skVideoNode, IsNSCoder aDecoder) => skVideoNode -> aDecoder -> IO (Id SKVideoNode)
initWithCoder skVideoNode  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg skVideoNode (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- play@
play :: IsSKVideoNode skVideoNode => skVideoNode -> IO ()
play skVideoNode  =
  sendMsg skVideoNode (mkSelector "play") retVoid []

-- | @- pause@
pause :: IsSKVideoNode skVideoNode => skVideoNode -> IO ()
pause skVideoNode  =
  sendMsg skVideoNode (mkSelector "pause") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoNodeWithAVPlayer:@
videoNodeWithAVPlayerSelector :: Selector
videoNodeWithAVPlayerSelector = mkSelector "videoNodeWithAVPlayer:"

-- | @Selector@ for @videoNodeWithVideoFileNamed:@
videoNodeWithVideoFileNamedSelector :: Selector
videoNodeWithVideoFileNamedSelector = mkSelector "videoNodeWithVideoFileNamed:"

-- | @Selector@ for @videoNodeWithFileNamed:@
videoNodeWithFileNamedSelector :: Selector
videoNodeWithFileNamedSelector = mkSelector "videoNodeWithFileNamed:"

-- | @Selector@ for @videoNodeWithVideoURL:@
videoNodeWithVideoURLSelector :: Selector
videoNodeWithVideoURLSelector = mkSelector "videoNodeWithVideoURL:"

-- | @Selector@ for @videoNodeWithURL:@
videoNodeWithURLSelector :: Selector
videoNodeWithURLSelector = mkSelector "videoNodeWithURL:"

-- | @Selector@ for @initWithAVPlayer:@
initWithAVPlayerSelector :: Selector
initWithAVPlayerSelector = mkSelector "initWithAVPlayer:"

-- | @Selector@ for @initWithVideoFileNamed:@
initWithVideoFileNamedSelector :: Selector
initWithVideoFileNamedSelector = mkSelector "initWithVideoFileNamed:"

-- | @Selector@ for @initWithFileNamed:@
initWithFileNamedSelector :: Selector
initWithFileNamedSelector = mkSelector "initWithFileNamed:"

-- | @Selector@ for @initWithVideoURL:@
initWithVideoURLSelector :: Selector
initWithVideoURLSelector = mkSelector "initWithVideoURL:"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @play@
playSelector :: Selector
playSelector = mkSelector "play"

-- | @Selector@ for @pause@
pauseSelector :: Selector
pauseSelector = mkSelector "pause"

