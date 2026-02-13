{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSound@.
module ObjC.AppKit.NSSound
  ( NSSound
  , IsNSSound(..)
  , soundNamed
  , initWithContentsOfURL_byReference
  , initWithContentsOfFile_byReference
  , initWithData
  , setName
  , canInitWithPasteboard
  , initWithPasteboard
  , writeToPasteboard
  , play
  , pause
  , resume
  , stop
  , setChannelMapping
  , channelMapping
  , soundUnfilteredFileTypes
  , soundUnfilteredPasteboardTypes
  , name
  , soundUnfilteredTypes
  , playing
  , delegate
  , setDelegate
  , duration
  , volume
  , setVolume
  , currentTime
  , setCurrentTime
  , loops
  , setLoops
  , playbackDeviceIdentifier
  , setPlaybackDeviceIdentifier
  , canInitWithPasteboardSelector
  , channelMappingSelector
  , currentTimeSelector
  , delegateSelector
  , durationSelector
  , initWithContentsOfFile_byReferenceSelector
  , initWithContentsOfURL_byReferenceSelector
  , initWithDataSelector
  , initWithPasteboardSelector
  , loopsSelector
  , nameSelector
  , pauseSelector
  , playSelector
  , playbackDeviceIdentifierSelector
  , playingSelector
  , resumeSelector
  , setChannelMappingSelector
  , setCurrentTimeSelector
  , setDelegateSelector
  , setLoopsSelector
  , setNameSelector
  , setPlaybackDeviceIdentifierSelector
  , setVolumeSelector
  , soundNamedSelector
  , soundUnfilteredFileTypesSelector
  , soundUnfilteredPasteboardTypesSelector
  , soundUnfilteredTypesSelector
  , stopSelector
  , volumeSelector
  , writeToPasteboardSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ soundNamed:@
soundNamed :: IsNSString name => name -> IO (Id NSSound)
soundNamed name =
  do
    cls' <- getRequiredClass "NSSound"
    sendClassMessage cls' soundNamedSelector (toNSString name)

-- | @- initWithContentsOfURL:byReference:@
initWithContentsOfURL_byReference :: (IsNSSound nsSound, IsNSURL url) => nsSound -> url -> Bool -> IO (Id NSSound)
initWithContentsOfURL_byReference nsSound url byRef =
  sendOwnedMessage nsSound initWithContentsOfURL_byReferenceSelector (toNSURL url) byRef

-- | @- initWithContentsOfFile:byReference:@
initWithContentsOfFile_byReference :: (IsNSSound nsSound, IsNSString path) => nsSound -> path -> Bool -> IO (Id NSSound)
initWithContentsOfFile_byReference nsSound path byRef =
  sendOwnedMessage nsSound initWithContentsOfFile_byReferenceSelector (toNSString path) byRef

-- | @- initWithData:@
initWithData :: (IsNSSound nsSound, IsNSData data_) => nsSound -> data_ -> IO (Id NSSound)
initWithData nsSound data_ =
  sendOwnedMessage nsSound initWithDataSelector (toNSData data_)

-- | @- setName:@
setName :: (IsNSSound nsSound, IsNSString string) => nsSound -> string -> IO Bool
setName nsSound string =
  sendMessage nsSound setNameSelector (toNSString string)

-- | @+ canInitWithPasteboard:@
canInitWithPasteboard :: IsNSPasteboard pasteboard => pasteboard -> IO Bool
canInitWithPasteboard pasteboard =
  do
    cls' <- getRequiredClass "NSSound"
    sendClassMessage cls' canInitWithPasteboardSelector (toNSPasteboard pasteboard)

-- | @- initWithPasteboard:@
initWithPasteboard :: (IsNSSound nsSound, IsNSPasteboard pasteboard) => nsSound -> pasteboard -> IO (Id NSSound)
initWithPasteboard nsSound pasteboard =
  sendOwnedMessage nsSound initWithPasteboardSelector (toNSPasteboard pasteboard)

-- | @- writeToPasteboard:@
writeToPasteboard :: (IsNSSound nsSound, IsNSPasteboard pasteboard) => nsSound -> pasteboard -> IO ()
writeToPasteboard nsSound pasteboard =
  sendMessage nsSound writeToPasteboardSelector (toNSPasteboard pasteboard)

-- | @- play@
play :: IsNSSound nsSound => nsSound -> IO Bool
play nsSound =
  sendMessage nsSound playSelector

-- | @- pause@
pause :: IsNSSound nsSound => nsSound -> IO Bool
pause nsSound =
  sendMessage nsSound pauseSelector

-- | @- resume@
resume :: IsNSSound nsSound => nsSound -> IO Bool
resume nsSound =
  sendMessage nsSound resumeSelector

-- | @- stop@
stop :: IsNSSound nsSound => nsSound -> IO Bool
stop nsSound =
  sendMessage nsSound stopSelector

-- | @- setChannelMapping:@
setChannelMapping :: (IsNSSound nsSound, IsNSArray channelMapping) => nsSound -> channelMapping -> IO ()
setChannelMapping nsSound channelMapping =
  sendMessage nsSound setChannelMappingSelector (toNSArray channelMapping)

-- | @- channelMapping@
channelMapping :: IsNSSound nsSound => nsSound -> IO (Id NSArray)
channelMapping nsSound =
  sendMessage nsSound channelMappingSelector

-- | @+ soundUnfilteredFileTypes@
soundUnfilteredFileTypes :: IO (Id NSArray)
soundUnfilteredFileTypes  =
  do
    cls' <- getRequiredClass "NSSound"
    sendClassMessage cls' soundUnfilteredFileTypesSelector

-- | @+ soundUnfilteredPasteboardTypes@
soundUnfilteredPasteboardTypes :: IO (Id NSArray)
soundUnfilteredPasteboardTypes  =
  do
    cls' <- getRequiredClass "NSSound"
    sendClassMessage cls' soundUnfilteredPasteboardTypesSelector

-- | @- name@
name :: IsNSSound nsSound => nsSound -> IO (Id NSString)
name nsSound =
  sendMessage nsSound nameSelector

-- | @+ soundUnfilteredTypes@
soundUnfilteredTypes :: IO (Id NSArray)
soundUnfilteredTypes  =
  do
    cls' <- getRequiredClass "NSSound"
    sendClassMessage cls' soundUnfilteredTypesSelector

-- | @- playing@
playing :: IsNSSound nsSound => nsSound -> IO Bool
playing nsSound =
  sendMessage nsSound playingSelector

-- | @- delegate@
delegate :: IsNSSound nsSound => nsSound -> IO RawId
delegate nsSound =
  sendMessage nsSound delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSSound nsSound => nsSound -> RawId -> IO ()
setDelegate nsSound value =
  sendMessage nsSound setDelegateSelector value

-- | @- duration@
duration :: IsNSSound nsSound => nsSound -> IO CDouble
duration nsSound =
  sendMessage nsSound durationSelector

-- | @- volume@
volume :: IsNSSound nsSound => nsSound -> IO CFloat
volume nsSound =
  sendMessage nsSound volumeSelector

-- | @- setVolume:@
setVolume :: IsNSSound nsSound => nsSound -> CFloat -> IO ()
setVolume nsSound value =
  sendMessage nsSound setVolumeSelector value

-- | @- currentTime@
currentTime :: IsNSSound nsSound => nsSound -> IO CDouble
currentTime nsSound =
  sendMessage nsSound currentTimeSelector

-- | @- setCurrentTime:@
setCurrentTime :: IsNSSound nsSound => nsSound -> CDouble -> IO ()
setCurrentTime nsSound value =
  sendMessage nsSound setCurrentTimeSelector value

-- | @- loops@
loops :: IsNSSound nsSound => nsSound -> IO Bool
loops nsSound =
  sendMessage nsSound loopsSelector

-- | @- setLoops:@
setLoops :: IsNSSound nsSound => nsSound -> Bool -> IO ()
setLoops nsSound value =
  sendMessage nsSound setLoopsSelector value

-- | @- playbackDeviceIdentifier@
playbackDeviceIdentifier :: IsNSSound nsSound => nsSound -> IO (Id NSString)
playbackDeviceIdentifier nsSound =
  sendMessage nsSound playbackDeviceIdentifierSelector

-- | @- setPlaybackDeviceIdentifier:@
setPlaybackDeviceIdentifier :: (IsNSSound nsSound, IsNSString value) => nsSound -> value -> IO ()
setPlaybackDeviceIdentifier nsSound value =
  sendMessage nsSound setPlaybackDeviceIdentifierSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @soundNamed:@
soundNamedSelector :: Selector '[Id NSString] (Id NSSound)
soundNamedSelector = mkSelector "soundNamed:"

-- | @Selector@ for @initWithContentsOfURL:byReference:@
initWithContentsOfURL_byReferenceSelector :: Selector '[Id NSURL, Bool] (Id NSSound)
initWithContentsOfURL_byReferenceSelector = mkSelector "initWithContentsOfURL:byReference:"

-- | @Selector@ for @initWithContentsOfFile:byReference:@
initWithContentsOfFile_byReferenceSelector :: Selector '[Id NSString, Bool] (Id NSSound)
initWithContentsOfFile_byReferenceSelector = mkSelector "initWithContentsOfFile:byReference:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector '[Id NSData] (Id NSSound)
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] Bool
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @canInitWithPasteboard:@
canInitWithPasteboardSelector :: Selector '[Id NSPasteboard] Bool
canInitWithPasteboardSelector = mkSelector "canInitWithPasteboard:"

-- | @Selector@ for @initWithPasteboard:@
initWithPasteboardSelector :: Selector '[Id NSPasteboard] (Id NSSound)
initWithPasteboardSelector = mkSelector "initWithPasteboard:"

-- | @Selector@ for @writeToPasteboard:@
writeToPasteboardSelector :: Selector '[Id NSPasteboard] ()
writeToPasteboardSelector = mkSelector "writeToPasteboard:"

-- | @Selector@ for @play@
playSelector :: Selector '[] Bool
playSelector = mkSelector "play"

-- | @Selector@ for @pause@
pauseSelector :: Selector '[] Bool
pauseSelector = mkSelector "pause"

-- | @Selector@ for @resume@
resumeSelector :: Selector '[] Bool
resumeSelector = mkSelector "resume"

-- | @Selector@ for @stop@
stopSelector :: Selector '[] Bool
stopSelector = mkSelector "stop"

-- | @Selector@ for @setChannelMapping:@
setChannelMappingSelector :: Selector '[Id NSArray] ()
setChannelMappingSelector = mkSelector "setChannelMapping:"

-- | @Selector@ for @channelMapping@
channelMappingSelector :: Selector '[] (Id NSArray)
channelMappingSelector = mkSelector "channelMapping"

-- | @Selector@ for @soundUnfilteredFileTypes@
soundUnfilteredFileTypesSelector :: Selector '[] (Id NSArray)
soundUnfilteredFileTypesSelector = mkSelector "soundUnfilteredFileTypes"

-- | @Selector@ for @soundUnfilteredPasteboardTypes@
soundUnfilteredPasteboardTypesSelector :: Selector '[] (Id NSArray)
soundUnfilteredPasteboardTypesSelector = mkSelector "soundUnfilteredPasteboardTypes"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @soundUnfilteredTypes@
soundUnfilteredTypesSelector :: Selector '[] (Id NSArray)
soundUnfilteredTypesSelector = mkSelector "soundUnfilteredTypes"

-- | @Selector@ for @playing@
playingSelector :: Selector '[] Bool
playingSelector = mkSelector "playing"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @volume@
volumeSelector :: Selector '[] CFloat
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector '[CFloat] ()
setVolumeSelector = mkSelector "setVolume:"

-- | @Selector@ for @currentTime@
currentTimeSelector :: Selector '[] CDouble
currentTimeSelector = mkSelector "currentTime"

-- | @Selector@ for @setCurrentTime:@
setCurrentTimeSelector :: Selector '[CDouble] ()
setCurrentTimeSelector = mkSelector "setCurrentTime:"

-- | @Selector@ for @loops@
loopsSelector :: Selector '[] Bool
loopsSelector = mkSelector "loops"

-- | @Selector@ for @setLoops:@
setLoopsSelector :: Selector '[Bool] ()
setLoopsSelector = mkSelector "setLoops:"

-- | @Selector@ for @playbackDeviceIdentifier@
playbackDeviceIdentifierSelector :: Selector '[] (Id NSString)
playbackDeviceIdentifierSelector = mkSelector "playbackDeviceIdentifier"

-- | @Selector@ for @setPlaybackDeviceIdentifier:@
setPlaybackDeviceIdentifierSelector :: Selector '[Id NSString] ()
setPlaybackDeviceIdentifierSelector = mkSelector "setPlaybackDeviceIdentifier:"

