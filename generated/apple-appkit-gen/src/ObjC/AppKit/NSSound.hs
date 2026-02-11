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
  , soundNamedSelector
  , initWithContentsOfURL_byReferenceSelector
  , initWithContentsOfFile_byReferenceSelector
  , initWithDataSelector
  , setNameSelector
  , canInitWithPasteboardSelector
  , initWithPasteboardSelector
  , writeToPasteboardSelector
  , playSelector
  , pauseSelector
  , resumeSelector
  , stopSelector
  , setChannelMappingSelector
  , channelMappingSelector
  , soundUnfilteredFileTypesSelector
  , soundUnfilteredPasteboardTypesSelector
  , nameSelector
  , soundUnfilteredTypesSelector
  , playingSelector
  , delegateSelector
  , setDelegateSelector
  , durationSelector
  , volumeSelector
  , setVolumeSelector
  , currentTimeSelector
  , setCurrentTimeSelector
  , loopsSelector
  , setLoopsSelector
  , playbackDeviceIdentifierSelector
  , setPlaybackDeviceIdentifierSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ soundNamed:@
soundNamed :: IsNSString name => name -> IO (Id NSSound)
soundNamed name =
  do
    cls' <- getRequiredClass "NSSound"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "soundNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithContentsOfURL:byReference:@
initWithContentsOfURL_byReference :: (IsNSSound nsSound, IsNSURL url) => nsSound -> url -> Bool -> IO (Id NSSound)
initWithContentsOfURL_byReference nsSound  url byRef =
  withObjCPtr url $ \raw_url ->
      sendMsg nsSound (mkSelector "initWithContentsOfURL:byReference:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argCULong (if byRef then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithContentsOfFile:byReference:@
initWithContentsOfFile_byReference :: (IsNSSound nsSound, IsNSString path) => nsSound -> path -> Bool -> IO (Id NSSound)
initWithContentsOfFile_byReference nsSound  path byRef =
  withObjCPtr path $ \raw_path ->
      sendMsg nsSound (mkSelector "initWithContentsOfFile:byReference:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ()), argCULong (if byRef then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithData:@
initWithData :: (IsNSSound nsSound, IsNSData data_) => nsSound -> data_ -> IO (Id NSSound)
initWithData nsSound  data_ =
  withObjCPtr data_ $ \raw_data_ ->
      sendMsg nsSound (mkSelector "initWithData:") (retPtr retVoid) [argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- setName:@
setName :: (IsNSSound nsSound, IsNSString string) => nsSound -> string -> IO Bool
setName nsSound  string =
  withObjCPtr string $ \raw_string ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSound (mkSelector "setName:") retCULong [argPtr (castPtr raw_string :: Ptr ())]

-- | @+ canInitWithPasteboard:@
canInitWithPasteboard :: IsNSPasteboard pasteboard => pasteboard -> IO Bool
canInitWithPasteboard pasteboard =
  do
    cls' <- getRequiredClass "NSSound"
    withObjCPtr pasteboard $ \raw_pasteboard ->
      fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "canInitWithPasteboard:") retCULong [argPtr (castPtr raw_pasteboard :: Ptr ())]

-- | @- initWithPasteboard:@
initWithPasteboard :: (IsNSSound nsSound, IsNSPasteboard pasteboard) => nsSound -> pasteboard -> IO (Id NSSound)
initWithPasteboard nsSound  pasteboard =
  withObjCPtr pasteboard $ \raw_pasteboard ->
      sendMsg nsSound (mkSelector "initWithPasteboard:") (retPtr retVoid) [argPtr (castPtr raw_pasteboard :: Ptr ())] >>= ownedObject . castPtr

-- | @- writeToPasteboard:@
writeToPasteboard :: (IsNSSound nsSound, IsNSPasteboard pasteboard) => nsSound -> pasteboard -> IO ()
writeToPasteboard nsSound  pasteboard =
  withObjCPtr pasteboard $ \raw_pasteboard ->
      sendMsg nsSound (mkSelector "writeToPasteboard:") retVoid [argPtr (castPtr raw_pasteboard :: Ptr ())]

-- | @- play@
play :: IsNSSound nsSound => nsSound -> IO Bool
play nsSound  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSound (mkSelector "play") retCULong []

-- | @- pause@
pause :: IsNSSound nsSound => nsSound -> IO Bool
pause nsSound  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSound (mkSelector "pause") retCULong []

-- | @- resume@
resume :: IsNSSound nsSound => nsSound -> IO Bool
resume nsSound  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSound (mkSelector "resume") retCULong []

-- | @- stop@
stop :: IsNSSound nsSound => nsSound -> IO Bool
stop nsSound  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSound (mkSelector "stop") retCULong []

-- | @- setChannelMapping:@
setChannelMapping :: (IsNSSound nsSound, IsNSArray channelMapping) => nsSound -> channelMapping -> IO ()
setChannelMapping nsSound  channelMapping =
  withObjCPtr channelMapping $ \raw_channelMapping ->
      sendMsg nsSound (mkSelector "setChannelMapping:") retVoid [argPtr (castPtr raw_channelMapping :: Ptr ())]

-- | @- channelMapping@
channelMapping :: IsNSSound nsSound => nsSound -> IO (Id NSArray)
channelMapping nsSound  =
    sendMsg nsSound (mkSelector "channelMapping") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ soundUnfilteredFileTypes@
soundUnfilteredFileTypes :: IO (Id NSArray)
soundUnfilteredFileTypes  =
  do
    cls' <- getRequiredClass "NSSound"
    sendClassMsg cls' (mkSelector "soundUnfilteredFileTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ soundUnfilteredPasteboardTypes@
soundUnfilteredPasteboardTypes :: IO (Id NSArray)
soundUnfilteredPasteboardTypes  =
  do
    cls' <- getRequiredClass "NSSound"
    sendClassMsg cls' (mkSelector "soundUnfilteredPasteboardTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- name@
name :: IsNSSound nsSound => nsSound -> IO (Id NSString)
name nsSound  =
    sendMsg nsSound (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ soundUnfilteredTypes@
soundUnfilteredTypes :: IO (Id NSArray)
soundUnfilteredTypes  =
  do
    cls' <- getRequiredClass "NSSound"
    sendClassMsg cls' (mkSelector "soundUnfilteredTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- playing@
playing :: IsNSSound nsSound => nsSound -> IO Bool
playing nsSound  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSound (mkSelector "playing") retCULong []

-- | @- delegate@
delegate :: IsNSSound nsSound => nsSound -> IO RawId
delegate nsSound  =
    fmap (RawId . castPtr) $ sendMsg nsSound (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSSound nsSound => nsSound -> RawId -> IO ()
setDelegate nsSound  value =
    sendMsg nsSound (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- duration@
duration :: IsNSSound nsSound => nsSound -> IO CDouble
duration nsSound  =
    sendMsg nsSound (mkSelector "duration") retCDouble []

-- | @- volume@
volume :: IsNSSound nsSound => nsSound -> IO CFloat
volume nsSound  =
    sendMsg nsSound (mkSelector "volume") retCFloat []

-- | @- setVolume:@
setVolume :: IsNSSound nsSound => nsSound -> CFloat -> IO ()
setVolume nsSound  value =
    sendMsg nsSound (mkSelector "setVolume:") retVoid [argCFloat value]

-- | @- currentTime@
currentTime :: IsNSSound nsSound => nsSound -> IO CDouble
currentTime nsSound  =
    sendMsg nsSound (mkSelector "currentTime") retCDouble []

-- | @- setCurrentTime:@
setCurrentTime :: IsNSSound nsSound => nsSound -> CDouble -> IO ()
setCurrentTime nsSound  value =
    sendMsg nsSound (mkSelector "setCurrentTime:") retVoid [argCDouble value]

-- | @- loops@
loops :: IsNSSound nsSound => nsSound -> IO Bool
loops nsSound  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSound (mkSelector "loops") retCULong []

-- | @- setLoops:@
setLoops :: IsNSSound nsSound => nsSound -> Bool -> IO ()
setLoops nsSound  value =
    sendMsg nsSound (mkSelector "setLoops:") retVoid [argCULong (if value then 1 else 0)]

-- | @- playbackDeviceIdentifier@
playbackDeviceIdentifier :: IsNSSound nsSound => nsSound -> IO (Id NSString)
playbackDeviceIdentifier nsSound  =
    sendMsg nsSound (mkSelector "playbackDeviceIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPlaybackDeviceIdentifier:@
setPlaybackDeviceIdentifier :: (IsNSSound nsSound, IsNSString value) => nsSound -> value -> IO ()
setPlaybackDeviceIdentifier nsSound  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsSound (mkSelector "setPlaybackDeviceIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @soundNamed:@
soundNamedSelector :: Selector
soundNamedSelector = mkSelector "soundNamed:"

-- | @Selector@ for @initWithContentsOfURL:byReference:@
initWithContentsOfURL_byReferenceSelector :: Selector
initWithContentsOfURL_byReferenceSelector = mkSelector "initWithContentsOfURL:byReference:"

-- | @Selector@ for @initWithContentsOfFile:byReference:@
initWithContentsOfFile_byReferenceSelector :: Selector
initWithContentsOfFile_byReferenceSelector = mkSelector "initWithContentsOfFile:byReference:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @canInitWithPasteboard:@
canInitWithPasteboardSelector :: Selector
canInitWithPasteboardSelector = mkSelector "canInitWithPasteboard:"

-- | @Selector@ for @initWithPasteboard:@
initWithPasteboardSelector :: Selector
initWithPasteboardSelector = mkSelector "initWithPasteboard:"

-- | @Selector@ for @writeToPasteboard:@
writeToPasteboardSelector :: Selector
writeToPasteboardSelector = mkSelector "writeToPasteboard:"

-- | @Selector@ for @play@
playSelector :: Selector
playSelector = mkSelector "play"

-- | @Selector@ for @pause@
pauseSelector :: Selector
pauseSelector = mkSelector "pause"

-- | @Selector@ for @resume@
resumeSelector :: Selector
resumeSelector = mkSelector "resume"

-- | @Selector@ for @stop@
stopSelector :: Selector
stopSelector = mkSelector "stop"

-- | @Selector@ for @setChannelMapping:@
setChannelMappingSelector :: Selector
setChannelMappingSelector = mkSelector "setChannelMapping:"

-- | @Selector@ for @channelMapping@
channelMappingSelector :: Selector
channelMappingSelector = mkSelector "channelMapping"

-- | @Selector@ for @soundUnfilteredFileTypes@
soundUnfilteredFileTypesSelector :: Selector
soundUnfilteredFileTypesSelector = mkSelector "soundUnfilteredFileTypes"

-- | @Selector@ for @soundUnfilteredPasteboardTypes@
soundUnfilteredPasteboardTypesSelector :: Selector
soundUnfilteredPasteboardTypesSelector = mkSelector "soundUnfilteredPasteboardTypes"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @soundUnfilteredTypes@
soundUnfilteredTypesSelector :: Selector
soundUnfilteredTypesSelector = mkSelector "soundUnfilteredTypes"

-- | @Selector@ for @playing@
playingSelector :: Selector
playingSelector = mkSelector "playing"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @volume@
volumeSelector :: Selector
volumeSelector = mkSelector "volume"

-- | @Selector@ for @setVolume:@
setVolumeSelector :: Selector
setVolumeSelector = mkSelector "setVolume:"

-- | @Selector@ for @currentTime@
currentTimeSelector :: Selector
currentTimeSelector = mkSelector "currentTime"

-- | @Selector@ for @setCurrentTime:@
setCurrentTimeSelector :: Selector
setCurrentTimeSelector = mkSelector "setCurrentTime:"

-- | @Selector@ for @loops@
loopsSelector :: Selector
loopsSelector = mkSelector "loops"

-- | @Selector@ for @setLoops:@
setLoopsSelector :: Selector
setLoopsSelector = mkSelector "setLoops:"

-- | @Selector@ for @playbackDeviceIdentifier@
playbackDeviceIdentifierSelector :: Selector
playbackDeviceIdentifierSelector = mkSelector "playbackDeviceIdentifier"

-- | @Selector@ for @setPlaybackDeviceIdentifier:@
setPlaybackDeviceIdentifierSelector :: Selector
setPlaybackDeviceIdentifierSelector = mkSelector "setPlaybackDeviceIdentifier:"

