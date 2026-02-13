{-# LANGUAGE DataKinds #-}
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
  , criticalSoundNamedSelector
  , criticalSoundNamed_withAudioVolumeSelector
  , defaultCriticalSoundSelector
  , defaultCriticalSoundWithAudioVolumeSelector
  , defaultRingtoneSoundSelector
  , defaultSoundSelector
  , initSelector
  , ringtoneSoundNamedSelector
  , soundNamedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.UserNotifications.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ defaultCriticalSoundWithAudioVolume:@
defaultCriticalSoundWithAudioVolume :: CFloat -> IO (Id UNNotificationSound)
defaultCriticalSoundWithAudioVolume volume =
  do
    cls' <- getRequiredClass "UNNotificationSound"
    sendClassMessage cls' defaultCriticalSoundWithAudioVolumeSelector volume

-- | @+ soundNamed:@
soundNamed :: IsNSString name => name -> IO (Id UNNotificationSound)
soundNamed name =
  do
    cls' <- getRequiredClass "UNNotificationSound"
    sendClassMessage cls' soundNamedSelector (toNSString name)

-- | @+ ringtoneSoundNamed:@
ringtoneSoundNamed :: IsNSString name => name -> IO (Id UNNotificationSound)
ringtoneSoundNamed name =
  do
    cls' <- getRequiredClass "UNNotificationSound"
    sendClassMessage cls' ringtoneSoundNamedSelector (toNSString name)

-- | @+ criticalSoundNamed:@
criticalSoundNamed :: IsNSString name => name -> IO (Id UNNotificationSound)
criticalSoundNamed name =
  do
    cls' <- getRequiredClass "UNNotificationSound"
    sendClassMessage cls' criticalSoundNamedSelector (toNSString name)

-- | @+ criticalSoundNamed:withAudioVolume:@
criticalSoundNamed_withAudioVolume :: IsNSString name => name -> CFloat -> IO (Id UNNotificationSound)
criticalSoundNamed_withAudioVolume name volume =
  do
    cls' <- getRequiredClass "UNNotificationSound"
    sendClassMessage cls' criticalSoundNamed_withAudioVolumeSelector (toNSString name) volume

-- | @- init@
init_ :: IsUNNotificationSound unNotificationSound => unNotificationSound -> IO (Id UNNotificationSound)
init_ unNotificationSound =
  sendOwnedMessage unNotificationSound initSelector

-- | @+ defaultSound@
defaultSound :: IO (Id UNNotificationSound)
defaultSound  =
  do
    cls' <- getRequiredClass "UNNotificationSound"
    sendClassMessage cls' defaultSoundSelector

-- | @+ defaultRingtoneSound@
defaultRingtoneSound :: IO (Id UNNotificationSound)
defaultRingtoneSound  =
  do
    cls' <- getRequiredClass "UNNotificationSound"
    sendClassMessage cls' defaultRingtoneSoundSelector

-- | @+ defaultCriticalSound@
defaultCriticalSound :: IO (Id UNNotificationSound)
defaultCriticalSound  =
  do
    cls' <- getRequiredClass "UNNotificationSound"
    sendClassMessage cls' defaultCriticalSoundSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultCriticalSoundWithAudioVolume:@
defaultCriticalSoundWithAudioVolumeSelector :: Selector '[CFloat] (Id UNNotificationSound)
defaultCriticalSoundWithAudioVolumeSelector = mkSelector "defaultCriticalSoundWithAudioVolume:"

-- | @Selector@ for @soundNamed:@
soundNamedSelector :: Selector '[Id NSString] (Id UNNotificationSound)
soundNamedSelector = mkSelector "soundNamed:"

-- | @Selector@ for @ringtoneSoundNamed:@
ringtoneSoundNamedSelector :: Selector '[Id NSString] (Id UNNotificationSound)
ringtoneSoundNamedSelector = mkSelector "ringtoneSoundNamed:"

-- | @Selector@ for @criticalSoundNamed:@
criticalSoundNamedSelector :: Selector '[Id NSString] (Id UNNotificationSound)
criticalSoundNamedSelector = mkSelector "criticalSoundNamed:"

-- | @Selector@ for @criticalSoundNamed:withAudioVolume:@
criticalSoundNamed_withAudioVolumeSelector :: Selector '[Id NSString, CFloat] (Id UNNotificationSound)
criticalSoundNamed_withAudioVolumeSelector = mkSelector "criticalSoundNamed:withAudioVolume:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id UNNotificationSound)
initSelector = mkSelector "init"

-- | @Selector@ for @defaultSound@
defaultSoundSelector :: Selector '[] (Id UNNotificationSound)
defaultSoundSelector = mkSelector "defaultSound"

-- | @Selector@ for @defaultRingtoneSound@
defaultRingtoneSoundSelector :: Selector '[] (Id UNNotificationSound)
defaultRingtoneSoundSelector = mkSelector "defaultRingtoneSound"

-- | @Selector@ for @defaultCriticalSound@
defaultCriticalSoundSelector :: Selector '[] (Id UNNotificationSound)
defaultCriticalSoundSelector = mkSelector "defaultCriticalSound"

