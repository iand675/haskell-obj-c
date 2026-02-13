{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @GCRacingWheel@.
module ObjC.GameController.GCRacingWheel
  ( GCRacingWheel
  , IsGCRacingWheel(..)
  , init_
  , acquireDeviceWithError
  , relinquishDevice
  , capture
  , connectedRacingWheels
  , acquired
  , wheelInput
  , snapshot
  , acquireDeviceWithErrorSelector
  , acquiredSelector
  , captureSelector
  , connectedRacingWheelsSelector
  , initSelector
  , relinquishDeviceSelector
  , snapshotSelector
  , wheelInputSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsGCRacingWheel gcRacingWheel => gcRacingWheel -> IO (Id GCRacingWheel)
init_ gcRacingWheel =
  sendOwnedMessage gcRacingWheel initSelector

-- | A GCRacingWheel must be acquired before your application can begin receiving events from it.  Prior to acquisition, your application may only query the properties of the racing wheel.  Acquisition is exclusive and may fail.
--
-- ObjC selector: @- acquireDeviceWithError:@
acquireDeviceWithError :: (IsGCRacingWheel gcRacingWheel, IsNSError error_) => gcRacingWheel -> error_ -> IO Bool
acquireDeviceWithError gcRacingWheel error_ =
  sendMessage gcRacingWheel acquireDeviceWithErrorSelector (toNSError error_)

-- | Releases a previous acquisition of the racing wheel.
--
-- ObjC selector: @- relinquishDevice@
relinquishDevice :: IsGCRacingWheel gcRacingWheel => gcRacingWheel -> IO ()
relinquishDevice gcRacingWheel =
  sendMessage gcRacingWheel relinquishDeviceSelector

-- | Polls the state vector of the racing wheel and saves it to a new instance of GCRacingWheel.
--
-- If your application is heavily multithreaded this may also be useful to guarantee atomicity of input handling as a snapshot will not change based on user input once it is taken.
--
-- See: snapshot
--
-- Returns: A new racing wheel with the duplicated state vector of the receiver.
--
-- ObjC selector: @- capture@
capture :: IsGCRacingWheel gcRacingWheel => gcRacingWheel -> IO (Id GCRacingWheel)
capture gcRacingWheel =
  sendMessage gcRacingWheel captureSelector

-- | Get the collection of racing wheels currently attached to the system.
--
-- See: GCRacingWheelDidConnectNotification
--
-- See: GCRacingWheelDidDisconnectNotification
--
-- ObjC selector: @+ connectedRacingWheels@
connectedRacingWheels :: IO (Id NSSet)
connectedRacingWheels  =
  do
    cls' <- getRequiredClass "GCRacingWheel"
    sendClassMessage cls' connectedRacingWheelsSelector

-- | Checks if the racing wheel has been acquired by the application.
--
-- This property is observable.
--
-- ObjC selector: @- acquired@
acquired :: IsGCRacingWheel gcRacingWheel => gcRacingWheel -> IO Bool
acquired gcRacingWheel =
  sendMessage gcRacingWheel acquiredSelector

-- | Get the physical input profile for the racing wheel.
--
-- ObjC selector: @- wheelInput@
wheelInput :: IsGCRacingWheel gcRacingWheel => gcRacingWheel -> IO (Id GCRacingWheelInput)
wheelInput gcRacingWheel =
  sendMessage gcRacingWheel wheelInputSelector

-- | A GCRacingWheel may represent a real device managed by the operating system, or a snapshot created by the developer.
--
-- See: capture
--
-- ObjC selector: @- snapshot@
snapshot :: IsGCRacingWheel gcRacingWheel => gcRacingWheel -> IO Bool
snapshot gcRacingWheel =
  sendMessage gcRacingWheel snapshotSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id GCRacingWheel)
initSelector = mkSelector "init"

-- | @Selector@ for @acquireDeviceWithError:@
acquireDeviceWithErrorSelector :: Selector '[Id NSError] Bool
acquireDeviceWithErrorSelector = mkSelector "acquireDeviceWithError:"

-- | @Selector@ for @relinquishDevice@
relinquishDeviceSelector :: Selector '[] ()
relinquishDeviceSelector = mkSelector "relinquishDevice"

-- | @Selector@ for @capture@
captureSelector :: Selector '[] (Id GCRacingWheel)
captureSelector = mkSelector "capture"

-- | @Selector@ for @connectedRacingWheels@
connectedRacingWheelsSelector :: Selector '[] (Id NSSet)
connectedRacingWheelsSelector = mkSelector "connectedRacingWheels"

-- | @Selector@ for @acquired@
acquiredSelector :: Selector '[] Bool
acquiredSelector = mkSelector "acquired"

-- | @Selector@ for @wheelInput@
wheelInputSelector :: Selector '[] (Id GCRacingWheelInput)
wheelInputSelector = mkSelector "wheelInput"

-- | @Selector@ for @snapshot@
snapshotSelector :: Selector '[] Bool
snapshotSelector = mkSelector "snapshot"

