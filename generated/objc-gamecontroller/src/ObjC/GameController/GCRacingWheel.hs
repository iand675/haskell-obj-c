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
  , initSelector
  , acquireDeviceWithErrorSelector
  , relinquishDeviceSelector
  , captureSelector
  , connectedRacingWheelsSelector
  , acquiredSelector
  , wheelInputSelector
  , snapshotSelector


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

import ObjC.GameController.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsGCRacingWheel gcRacingWheel => gcRacingWheel -> IO (Id GCRacingWheel)
init_ gcRacingWheel  =
  sendMsg gcRacingWheel (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | A GCRacingWheel must be acquired before your application can begin receiving events from it.  Prior to acquisition, your application may only query the properties of the racing wheel.  Acquisition is exclusive and may fail.
--
-- ObjC selector: @- acquireDeviceWithError:@
acquireDeviceWithError :: (IsGCRacingWheel gcRacingWheel, IsNSError error_) => gcRacingWheel -> error_ -> IO Bool
acquireDeviceWithError gcRacingWheel  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg gcRacingWheel (mkSelector "acquireDeviceWithError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | Releases a previous acquisition of the racing wheel.
--
-- ObjC selector: @- relinquishDevice@
relinquishDevice :: IsGCRacingWheel gcRacingWheel => gcRacingWheel -> IO ()
relinquishDevice gcRacingWheel  =
  sendMsg gcRacingWheel (mkSelector "relinquishDevice") retVoid []

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
capture gcRacingWheel  =
  sendMsg gcRacingWheel (mkSelector "capture") (retPtr retVoid) [] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "connectedRacingWheels") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Checks if the racing wheel has been acquired by the application.
--
-- This property is observable.
--
-- ObjC selector: @- acquired@
acquired :: IsGCRacingWheel gcRacingWheel => gcRacingWheel -> IO Bool
acquired gcRacingWheel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gcRacingWheel (mkSelector "acquired") retCULong []

-- | Get the physical input profile for the racing wheel.
--
-- ObjC selector: @- wheelInput@
wheelInput :: IsGCRacingWheel gcRacingWheel => gcRacingWheel -> IO (Id GCRacingWheelInput)
wheelInput gcRacingWheel  =
  sendMsg gcRacingWheel (mkSelector "wheelInput") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A GCRacingWheel may represent a real device managed by the operating system, or a snapshot created by the developer.
--
-- See: capture
--
-- ObjC selector: @- snapshot@
snapshot :: IsGCRacingWheel gcRacingWheel => gcRacingWheel -> IO Bool
snapshot gcRacingWheel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gcRacingWheel (mkSelector "snapshot") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @acquireDeviceWithError:@
acquireDeviceWithErrorSelector :: Selector
acquireDeviceWithErrorSelector = mkSelector "acquireDeviceWithError:"

-- | @Selector@ for @relinquishDevice@
relinquishDeviceSelector :: Selector
relinquishDeviceSelector = mkSelector "relinquishDevice"

-- | @Selector@ for @capture@
captureSelector :: Selector
captureSelector = mkSelector "capture"

-- | @Selector@ for @connectedRacingWheels@
connectedRacingWheelsSelector :: Selector
connectedRacingWheelsSelector = mkSelector "connectedRacingWheels"

-- | @Selector@ for @acquired@
acquiredSelector :: Selector
acquiredSelector = mkSelector "acquired"

-- | @Selector@ for @wheelInput@
wheelInputSelector :: Selector
wheelInputSelector = mkSelector "wheelInput"

-- | @Selector@ for @snapshot@
snapshotSelector :: Selector
snapshotSelector = mkSelector "snapshot"

