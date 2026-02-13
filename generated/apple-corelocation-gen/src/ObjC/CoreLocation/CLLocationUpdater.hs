{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLLocationUpdater@.
module ObjC.CoreLocation.CLLocationUpdater
  ( CLLocationUpdater
  , IsCLLocationUpdater(..)
  , init_
  , new
  , liveUpdaterWithQueue_handler
  , liveUpdaterWithConfiguration_queue_handler
  , resume
  , pause
  , invalidate
  , initSelector
  , invalidateSelector
  , liveUpdaterWithConfiguration_queue_handlerSelector
  , liveUpdaterWithQueue_handlerSelector
  , newSelector
  , pauseSelector
  , resumeSelector

  -- * Enum types
  , CLLiveUpdateConfiguration(CLLiveUpdateConfiguration)
  , pattern CLLiveUpdateConfigurationDefault
  , pattern CLLiveUpdateConfigurationAutomotiveNavigation
  , pattern CLLiveUpdateConfigurationOtherNavigation
  , pattern CLLiveUpdateConfigurationFitness
  , pattern CLLiveUpdateConfigurationAirborne

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreLocation.Internal.Classes
import ObjC.CoreLocation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCLLocationUpdater clLocationUpdater => clLocationUpdater -> IO (Id CLLocationUpdater)
init_ clLocationUpdater =
  sendOwnedMessage clLocationUpdater initSelector

-- | @+ new@
new :: IO (Id CLLocationUpdater)
new  =
  do
    cls' <- getRequiredClass "CLLocationUpdater"
    sendOwnedClassMessage cls' newSelector

-- | @+ liveUpdaterWithQueue:handler:@
liveUpdaterWithQueue_handler :: IsNSObject queue => queue -> Ptr () -> IO (Id CLLocationUpdater)
liveUpdaterWithQueue_handler queue handler =
  do
    cls' <- getRequiredClass "CLLocationUpdater"
    sendClassMessage cls' liveUpdaterWithQueue_handlerSelector (toNSObject queue) handler

-- | @+ liveUpdaterWithConfiguration:queue:handler:@
liveUpdaterWithConfiguration_queue_handler :: IsNSObject queue => CLLiveUpdateConfiguration -> queue -> Ptr () -> IO (Id CLLocationUpdater)
liveUpdaterWithConfiguration_queue_handler configuration queue handler =
  do
    cls' <- getRequiredClass "CLLocationUpdater"
    sendClassMessage cls' liveUpdaterWithConfiguration_queue_handlerSelector configuration (toNSObject queue) handler

-- | @- resume@
resume :: IsCLLocationUpdater clLocationUpdater => clLocationUpdater -> IO ()
resume clLocationUpdater =
  sendMessage clLocationUpdater resumeSelector

-- | @- pause@
pause :: IsCLLocationUpdater clLocationUpdater => clLocationUpdater -> IO ()
pause clLocationUpdater =
  sendMessage clLocationUpdater pauseSelector

-- | @- invalidate@
invalidate :: IsCLLocationUpdater clLocationUpdater => clLocationUpdater -> IO ()
invalidate clLocationUpdater =
  sendMessage clLocationUpdater invalidateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CLLocationUpdater)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CLLocationUpdater)
newSelector = mkSelector "new"

-- | @Selector@ for @liveUpdaterWithQueue:handler:@
liveUpdaterWithQueue_handlerSelector :: Selector '[Id NSObject, Ptr ()] (Id CLLocationUpdater)
liveUpdaterWithQueue_handlerSelector = mkSelector "liveUpdaterWithQueue:handler:"

-- | @Selector@ for @liveUpdaterWithConfiguration:queue:handler:@
liveUpdaterWithConfiguration_queue_handlerSelector :: Selector '[CLLiveUpdateConfiguration, Id NSObject, Ptr ()] (Id CLLocationUpdater)
liveUpdaterWithConfiguration_queue_handlerSelector = mkSelector "liveUpdaterWithConfiguration:queue:handler:"

-- | @Selector@ for @resume@
resumeSelector :: Selector '[] ()
resumeSelector = mkSelector "resume"

-- | @Selector@ for @pause@
pauseSelector :: Selector '[] ()
pauseSelector = mkSelector "pause"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector '[] ()
invalidateSelector = mkSelector "invalidate"

