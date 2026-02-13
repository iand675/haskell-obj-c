{-# LANGUAGE DataKinds #-}
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
  , initWithAVPlayerSelector
  , initWithCoderSelector
  , initWithFileNamedSelector
  , initWithURLSelector
  , initWithVideoFileNamedSelector
  , initWithVideoURLSelector
  , pauseSelector
  , playSelector
  , videoNodeWithAVPlayerSelector
  , videoNodeWithFileNamedSelector
  , videoNodeWithURLSelector
  , videoNodeWithVideoFileNamedSelector
  , videoNodeWithVideoURLSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' videoNodeWithAVPlayerSelector (toAVPlayer player)

-- | Create a video node from a file.
--
-- ObjC selector: @+ videoNodeWithVideoFileNamed:@
videoNodeWithVideoFileNamed :: IsNSString videoFile => videoFile -> IO (Id SKVideoNode)
videoNodeWithVideoFileNamed videoFile =
  do
    cls' <- getRequiredClass "SKVideoNode"
    sendClassMessage cls' videoNodeWithVideoFileNamedSelector (toNSString videoFile)

-- | @+ videoNodeWithFileNamed:@
videoNodeWithFileNamed :: IsNSString videoFile => videoFile -> IO (Id SKVideoNode)
videoNodeWithFileNamed videoFile =
  do
    cls' <- getRequiredClass "SKVideoNode"
    sendClassMessage cls' videoNodeWithFileNamedSelector (toNSString videoFile)

-- | Create a video node from a URL.
--
-- ObjC selector: @+ videoNodeWithVideoURL:@
videoNodeWithVideoURL :: IsNSURL videoURL => videoURL -> IO (Id SKVideoNode)
videoNodeWithVideoURL videoURL =
  do
    cls' <- getRequiredClass "SKVideoNode"
    sendClassMessage cls' videoNodeWithVideoURLSelector (toNSURL videoURL)

-- | @+ videoNodeWithURL:@
videoNodeWithURL :: IsNSURL videoURL => videoURL -> IO (Id SKVideoNode)
videoNodeWithURL videoURL =
  do
    cls' <- getRequiredClass "SKVideoNode"
    sendClassMessage cls' videoNodeWithURLSelector (toNSURL videoURL)

-- | Designated Initializer.
--
-- Initialize a video node from an AVPlayer. You can use the AVPlayer to control playback.
--
-- ObjC selector: @- initWithAVPlayer:@
initWithAVPlayer :: (IsSKVideoNode skVideoNode, IsAVPlayer player) => skVideoNode -> player -> IO (Id SKVideoNode)
initWithAVPlayer skVideoNode player =
  sendOwnedMessage skVideoNode initWithAVPlayerSelector (toAVPlayer player)

-- | Initialize a video node from a file.
--
-- ObjC selector: @- initWithVideoFileNamed:@
initWithVideoFileNamed :: (IsSKVideoNode skVideoNode, IsNSString videoFile) => skVideoNode -> videoFile -> IO (Id SKVideoNode)
initWithVideoFileNamed skVideoNode videoFile =
  sendOwnedMessage skVideoNode initWithVideoFileNamedSelector (toNSString videoFile)

-- | @- initWithFileNamed:@
initWithFileNamed :: (IsSKVideoNode skVideoNode, IsNSString videoFile) => skVideoNode -> videoFile -> IO (Id SKVideoNode)
initWithFileNamed skVideoNode videoFile =
  sendOwnedMessage skVideoNode initWithFileNamedSelector (toNSString videoFile)

-- | @- initWithVideoURL:@
initWithVideoURL :: (IsSKVideoNode skVideoNode, IsNSURL url) => skVideoNode -> url -> IO (Id SKVideoNode)
initWithVideoURL skVideoNode url =
  sendOwnedMessage skVideoNode initWithVideoURLSelector (toNSURL url)

-- | @- initWithURL:@
initWithURL :: (IsSKVideoNode skVideoNode, IsNSURL url) => skVideoNode -> url -> IO (Id SKVideoNode)
initWithURL skVideoNode url =
  sendOwnedMessage skVideoNode initWithURLSelector (toNSURL url)

-- | Support coding and decoding via NSKeyedArchiver.
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsSKVideoNode skVideoNode, IsNSCoder aDecoder) => skVideoNode -> aDecoder -> IO (Id SKVideoNode)
initWithCoder skVideoNode aDecoder =
  sendOwnedMessage skVideoNode initWithCoderSelector (toNSCoder aDecoder)

-- | @- play@
play :: IsSKVideoNode skVideoNode => skVideoNode -> IO ()
play skVideoNode =
  sendMessage skVideoNode playSelector

-- | @- pause@
pause :: IsSKVideoNode skVideoNode => skVideoNode -> IO ()
pause skVideoNode =
  sendMessage skVideoNode pauseSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoNodeWithAVPlayer:@
videoNodeWithAVPlayerSelector :: Selector '[Id AVPlayer] (Id SKVideoNode)
videoNodeWithAVPlayerSelector = mkSelector "videoNodeWithAVPlayer:"

-- | @Selector@ for @videoNodeWithVideoFileNamed:@
videoNodeWithVideoFileNamedSelector :: Selector '[Id NSString] (Id SKVideoNode)
videoNodeWithVideoFileNamedSelector = mkSelector "videoNodeWithVideoFileNamed:"

-- | @Selector@ for @videoNodeWithFileNamed:@
videoNodeWithFileNamedSelector :: Selector '[Id NSString] (Id SKVideoNode)
videoNodeWithFileNamedSelector = mkSelector "videoNodeWithFileNamed:"

-- | @Selector@ for @videoNodeWithVideoURL:@
videoNodeWithVideoURLSelector :: Selector '[Id NSURL] (Id SKVideoNode)
videoNodeWithVideoURLSelector = mkSelector "videoNodeWithVideoURL:"

-- | @Selector@ for @videoNodeWithURL:@
videoNodeWithURLSelector :: Selector '[Id NSURL] (Id SKVideoNode)
videoNodeWithURLSelector = mkSelector "videoNodeWithURL:"

-- | @Selector@ for @initWithAVPlayer:@
initWithAVPlayerSelector :: Selector '[Id AVPlayer] (Id SKVideoNode)
initWithAVPlayerSelector = mkSelector "initWithAVPlayer:"

-- | @Selector@ for @initWithVideoFileNamed:@
initWithVideoFileNamedSelector :: Selector '[Id NSString] (Id SKVideoNode)
initWithVideoFileNamedSelector = mkSelector "initWithVideoFileNamed:"

-- | @Selector@ for @initWithFileNamed:@
initWithFileNamedSelector :: Selector '[Id NSString] (Id SKVideoNode)
initWithFileNamedSelector = mkSelector "initWithFileNamed:"

-- | @Selector@ for @initWithVideoURL:@
initWithVideoURLSelector :: Selector '[Id NSURL] (Id SKVideoNode)
initWithVideoURLSelector = mkSelector "initWithVideoURL:"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector '[Id NSURL] (Id SKVideoNode)
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id SKVideoNode)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @play@
playSelector :: Selector '[] ()
playSelector = mkSelector "play"

-- | @Selector@ for @pause@
pauseSelector :: Selector '[] ()
pauseSelector = mkSelector "pause"

