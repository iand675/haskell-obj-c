{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @UNNotificationSound@.
module ObjC.UserNotifications.UNNotificationSound
  ( UNNotificationSound
  , IsUNNotificationSound(..)
  , defaultCriticalSoundWithAudioVolume
  , soundNamed
  , ringtoneSoundNamed
  , criticalSoundNamed
  , criticalSoundNamed_withAudioVolume
  , init_
  , defaultSound
  , defaultRingtoneSound
  , defaultCriticalSound
  , defaultCriticalSoundWithAudioVolumeSelector
  , soundNamedSelector
  , ringtoneSoundNamedSelector
  , criticalSoundNamedSelector
  , criticalSoundNamed_withAudioVolumeSelector
  , initSelector
  , defaultSoundSelector
  , defaultRingtoneSoundSelector
  , defaultCriticalSoundSelector


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

import ObjC.UserNotifications.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ defaultCriticalSoundWithAudioVolume:@
defaultCriticalSoundWithAudioVolume :: CFloat -> IO (Id UNNotificationSound)
defaultCriticalSoundWithAudioVolume volume =
  do
    cls' <- getRequiredClass "UNNotificationSound"
    sendClassMsg cls' (mkSelector "defaultCriticalSoundWithAudioVolume:") (retPtr retVoid) [argCFloat volume] >>= retainedObject . castPtr

-- | @+ soundNamed:@
soundNamed :: IsNSString name => name -> IO (Id UNNotificationSound)
soundNamed name =
  do
    cls' <- getRequiredClass "UNNotificationSound"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "soundNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @+ ringtoneSoundNamed:@
ringtoneSoundNamed :: IsNSString name => name -> IO (Id UNNotificationSound)
ringtoneSoundNamed name =
  do
    cls' <- getRequiredClass "UNNotificationSound"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "ringtoneSoundNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @+ criticalSoundNamed:@
criticalSoundNamed :: IsNSString name => name -> IO (Id UNNotificationSound)
criticalSoundNamed name =
  do
    cls' <- getRequiredClass "UNNotificationSound"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "criticalSoundNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @+ criticalSoundNamed:withAudioVolume:@
criticalSoundNamed_withAudioVolume :: IsNSString name => name -> CFloat -> IO (Id UNNotificationSound)
criticalSoundNamed_withAudioVolume name volume =
  do
    cls' <- getRequiredClass "UNNotificationSound"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "criticalSoundNamed:withAudioVolume:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argCFloat volume] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsUNNotificationSound unNotificationSound => unNotificationSound -> IO (Id UNNotificationSound)
init_ unNotificationSound  =
    sendMsg unNotificationSound (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ defaultSound@
defaultSound :: IO (Id UNNotificationSound)
defaultSound  =
  do
    cls' <- getRequiredClass "UNNotificationSound"
    sendClassMsg cls' (mkSelector "defaultSound") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ defaultRingtoneSound@
defaultRingtoneSound :: IO (Id UNNotificationSound)
defaultRingtoneSound  =
  do
    cls' <- getRequiredClass "UNNotificationSound"
    sendClassMsg cls' (mkSelector "defaultRingtoneSound") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ defaultCriticalSound@
defaultCriticalSound :: IO (Id UNNotificationSound)
defaultCriticalSound  =
  do
    cls' <- getRequiredClass "UNNotificationSound"
    sendClassMsg cls' (mkSelector "defaultCriticalSound") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultCriticalSoundWithAudioVolume:@
defaultCriticalSoundWithAudioVolumeSelector :: Selector
defaultCriticalSoundWithAudioVolumeSelector = mkSelector "defaultCriticalSoundWithAudioVolume:"

-- | @Selector@ for @soundNamed:@
soundNamedSelector :: Selector
soundNamedSelector = mkSelector "soundNamed:"

-- | @Selector@ for @ringtoneSoundNamed:@
ringtoneSoundNamedSelector :: Selector
ringtoneSoundNamedSelector = mkSelector "ringtoneSoundNamed:"

-- | @Selector@ for @criticalSoundNamed:@
criticalSoundNamedSelector :: Selector
criticalSoundNamedSelector = mkSelector "criticalSoundNamed:"

-- | @Selector@ for @criticalSoundNamed:withAudioVolume:@
criticalSoundNamed_withAudioVolumeSelector :: Selector
criticalSoundNamed_withAudioVolumeSelector = mkSelector "criticalSoundNamed:withAudioVolume:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @defaultSound@
defaultSoundSelector :: Selector
defaultSoundSelector = mkSelector "defaultSound"

-- | @Selector@ for @defaultRingtoneSound@
defaultRingtoneSoundSelector :: Selector
defaultRingtoneSoundSelector = mkSelector "defaultRingtoneSound"

-- | @Selector@ for @defaultCriticalSound@
defaultCriticalSoundSelector :: Selector
defaultCriticalSoundSelector = mkSelector "defaultCriticalSound"

