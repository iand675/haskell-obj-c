{-# LANGUAGE PatternSynonyms #-}
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
  , newSelector
  , liveUpdaterWithQueue_handlerSelector
  , liveUpdaterWithConfiguration_queue_handlerSelector
  , resumeSelector
  , pauseSelector
  , invalidateSelector

  -- * Enum types
  , CLLiveUpdateConfiguration(CLLiveUpdateConfiguration)
  , pattern CLLiveUpdateConfigurationDefault
  , pattern CLLiveUpdateConfigurationAutomotiveNavigation
  , pattern CLLiveUpdateConfigurationOtherNavigation
  , pattern CLLiveUpdateConfigurationFitness
  , pattern CLLiveUpdateConfigurationAirborne

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

import ObjC.CoreLocation.Internal.Classes
import ObjC.CoreLocation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCLLocationUpdater clLocationUpdater => clLocationUpdater -> IO (Id CLLocationUpdater)
init_ clLocationUpdater  =
  sendMsg clLocationUpdater (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CLLocationUpdater)
new  =
  do
    cls' <- getRequiredClass "CLLocationUpdater"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ liveUpdaterWithQueue:handler:@
liveUpdaterWithQueue_handler :: IsNSObject queue => queue -> Ptr () -> IO (Id CLLocationUpdater)
liveUpdaterWithQueue_handler queue handler =
  do
    cls' <- getRequiredClass "CLLocationUpdater"
    withObjCPtr queue $ \raw_queue ->
      sendClassMsg cls' (mkSelector "liveUpdaterWithQueue:handler:") (retPtr retVoid) [argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr handler :: Ptr ())] >>= retainedObject . castPtr

-- | @+ liveUpdaterWithConfiguration:queue:handler:@
liveUpdaterWithConfiguration_queue_handler :: IsNSObject queue => CLLiveUpdateConfiguration -> queue -> Ptr () -> IO (Id CLLocationUpdater)
liveUpdaterWithConfiguration_queue_handler configuration queue handler =
  do
    cls' <- getRequiredClass "CLLocationUpdater"
    withObjCPtr queue $ \raw_queue ->
      sendClassMsg cls' (mkSelector "liveUpdaterWithConfiguration:queue:handler:") (retPtr retVoid) [argCLong (coerce configuration), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr handler :: Ptr ())] >>= retainedObject . castPtr

-- | @- resume@
resume :: IsCLLocationUpdater clLocationUpdater => clLocationUpdater -> IO ()
resume clLocationUpdater  =
  sendMsg clLocationUpdater (mkSelector "resume") retVoid []

-- | @- pause@
pause :: IsCLLocationUpdater clLocationUpdater => clLocationUpdater -> IO ()
pause clLocationUpdater  =
  sendMsg clLocationUpdater (mkSelector "pause") retVoid []

-- | @- invalidate@
invalidate :: IsCLLocationUpdater clLocationUpdater => clLocationUpdater -> IO ()
invalidate clLocationUpdater  =
  sendMsg clLocationUpdater (mkSelector "invalidate") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @liveUpdaterWithQueue:handler:@
liveUpdaterWithQueue_handlerSelector :: Selector
liveUpdaterWithQueue_handlerSelector = mkSelector "liveUpdaterWithQueue:handler:"

-- | @Selector@ for @liveUpdaterWithConfiguration:queue:handler:@
liveUpdaterWithConfiguration_queue_handlerSelector :: Selector
liveUpdaterWithConfiguration_queue_handlerSelector = mkSelector "liveUpdaterWithConfiguration:queue:handler:"

-- | @Selector@ for @resume@
resumeSelector :: Selector
resumeSelector = mkSelector "resume"

-- | @Selector@ for @pause@
pauseSelector :: Selector
pauseSelector = mkSelector "pause"

-- | @Selector@ for @invalidate@
invalidateSelector :: Selector
invalidateSelector = mkSelector "invalidate"

