{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceControllerFactory@.
module ObjC.Matter.MTRDeviceControllerFactory
  ( MTRDeviceControllerFactory
  , IsMTRDeviceControllerFactory(..)
  , init_
  , new
  , sharedInstance
  , startControllerFactory_error
  , stopControllerFactory
  , createControllerOnExistingFabric_error
  , createControllerOnNewFabric_error
  , preWarmCommissioningSession
  , running
  , knownFabrics
  , createControllerOnExistingFabric_errorSelector
  , createControllerOnNewFabric_errorSelector
  , initSelector
  , knownFabricsSelector
  , newSelector
  , preWarmCommissioningSessionSelector
  , runningSelector
  , sharedInstanceSelector
  , startControllerFactory_errorSelector
  , stopControllerFactorySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMTRDeviceControllerFactory mtrDeviceControllerFactory => mtrDeviceControllerFactory -> IO (Id MTRDeviceControllerFactory)
init_ mtrDeviceControllerFactory =
  sendOwnedMessage mtrDeviceControllerFactory initSelector

-- | @+ new@
new :: IO (Id MTRDeviceControllerFactory)
new  =
  do
    cls' <- getRequiredClass "MTRDeviceControllerFactory"
    sendOwnedClassMessage cls' newSelector

-- | Return the single MTRDeviceControllerFactory we support existing.  It starts off in a "not started" state.
--
-- ObjC selector: @+ sharedInstance@
sharedInstance :: IO (Id MTRDeviceControllerFactory)
sharedInstance  =
  do
    cls' <- getRequiredClass "MTRDeviceControllerFactory"
    sendClassMessage cls' sharedInstanceSelector

-- | Start the controller factory. Repeated calls to startControllerFactory without calls to stopControllerFactory in between are NO-OPs. Use the isRunning property to check whether the controller factory needs to be started up.
--
-- @startupParams@ — data needed to start up the controller factory.
--
-- Returns: Whether startup succeded.
--
-- ObjC selector: @- startControllerFactory:error:@
startControllerFactory_error :: (IsMTRDeviceControllerFactory mtrDeviceControllerFactory, IsMTRDeviceControllerFactoryParams startupParams, IsNSError error_) => mtrDeviceControllerFactory -> startupParams -> error_ -> IO Bool
startControllerFactory_error mtrDeviceControllerFactory startupParams error_ =
  sendMessage mtrDeviceControllerFactory startControllerFactory_errorSelector (toMTRDeviceControllerFactoryParams startupParams) (toNSError error_)

-- | Stop the controller factory. This will shut down any outstanding controllers as part of the factory stopping.
--
-- Repeated calls to stopControllerFactory without calls to startControllerFactory in between are NO-OPs.
--
-- ObjC selector: @- stopControllerFactory@
stopControllerFactory :: IsMTRDeviceControllerFactory mtrDeviceControllerFactory => mtrDeviceControllerFactory -> IO ()
stopControllerFactory mtrDeviceControllerFactory =
  sendMessage mtrDeviceControllerFactory stopControllerFactorySelector

-- | Create a MTRDeviceController on an existing fabric.  Returns nil on failure.
--
-- This method will fail if there is no such fabric or if there is already a controller started for that fabric.
--
-- The fabric is identified by the root public key and fabric id in the startupParams.
--
-- This method can only be used if the factory was initialized with storage. When using per-controller storage, use [MTRDeviceController initWithParameters:error:].
--
-- ObjC selector: @- createControllerOnExistingFabric:error:@
createControllerOnExistingFabric_error :: (IsMTRDeviceControllerFactory mtrDeviceControllerFactory, IsMTRDeviceControllerStartupParams startupParams, IsNSError error_) => mtrDeviceControllerFactory -> startupParams -> error_ -> IO (Id MTRDeviceController)
createControllerOnExistingFabric_error mtrDeviceControllerFactory startupParams error_ =
  sendMessage mtrDeviceControllerFactory createControllerOnExistingFabric_errorSelector (toMTRDeviceControllerStartupParams startupParams) (toNSError error_)

-- | Create a MTRDeviceController on a new fabric.  Returns nil on failure.
--
-- This method will fail if the given fabric already exists.
--
-- The fabric is identified by the root public key and fabric id in the startupParams.
--
-- This method can only be used if the factory was initialized with storage. When using per-controller storage, use [MTRDeviceController initWithParameters:error:].
--
-- ObjC selector: @- createControllerOnNewFabric:error:@
createControllerOnNewFabric_error :: (IsMTRDeviceControllerFactory mtrDeviceControllerFactory, IsMTRDeviceControllerStartupParams startupParams, IsNSError error_) => mtrDeviceControllerFactory -> startupParams -> error_ -> IO (Id MTRDeviceController)
createControllerOnNewFabric_error mtrDeviceControllerFactory startupParams error_ =
  sendMessage mtrDeviceControllerFactory createControllerOnNewFabric_errorSelector (toMTRDeviceControllerStartupParams startupParams) (toNSError error_)

-- | If possible, pre-warm the Matter stack for setting up a commissioning session.
--
-- This may be called before -[MTRDeviceController setupCommissioningSessionWithPayload:] if it is known that a commissioning attempt will soon take place, but the commissioning payload is not known yet.
--
-- The controller factory must be running for pre-warming to take place.  Pre-warming can take place before any controllers are started.
--
-- ObjC selector: @- preWarmCommissioningSession@
preWarmCommissioningSession :: IsMTRDeviceControllerFactory mtrDeviceControllerFactory => mtrDeviceControllerFactory -> IO ()
preWarmCommissioningSession mtrDeviceControllerFactory =
  sendMessage mtrDeviceControllerFactory preWarmCommissioningSessionSelector

-- | If true, the factory is in a state where it can create controllers: startControllerFactory has been called, but stopControllerFactory has not been called since then.
--
-- ObjC selector: @- running@
running :: IsMTRDeviceControllerFactory mtrDeviceControllerFactory => mtrDeviceControllerFactory -> IO Bool
running mtrDeviceControllerFactory =
  sendMessage mtrDeviceControllerFactory runningSelector

-- | Returns the list of MTRFabricInfo representing the fabrics the MTRDeviceControllerFactory knows about and the corresponding node identities of the controller factory on those fabrics.  Returns nil if the factory is not running or if there is an error reading fabric information.
--
-- All entries in this list will have a non-nil rootCertificate.
--
-- ObjC selector: @- knownFabrics@
knownFabrics :: IsMTRDeviceControllerFactory mtrDeviceControllerFactory => mtrDeviceControllerFactory -> IO (Id NSArray)
knownFabrics mtrDeviceControllerFactory =
  sendMessage mtrDeviceControllerFactory knownFabricsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRDeviceControllerFactory)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRDeviceControllerFactory)
newSelector = mkSelector "new"

-- | @Selector@ for @sharedInstance@
sharedInstanceSelector :: Selector '[] (Id MTRDeviceControllerFactory)
sharedInstanceSelector = mkSelector "sharedInstance"

-- | @Selector@ for @startControllerFactory:error:@
startControllerFactory_errorSelector :: Selector '[Id MTRDeviceControllerFactoryParams, Id NSError] Bool
startControllerFactory_errorSelector = mkSelector "startControllerFactory:error:"

-- | @Selector@ for @stopControllerFactory@
stopControllerFactorySelector :: Selector '[] ()
stopControllerFactorySelector = mkSelector "stopControllerFactory"

-- | @Selector@ for @createControllerOnExistingFabric:error:@
createControllerOnExistingFabric_errorSelector :: Selector '[Id MTRDeviceControllerStartupParams, Id NSError] (Id MTRDeviceController)
createControllerOnExistingFabric_errorSelector = mkSelector "createControllerOnExistingFabric:error:"

-- | @Selector@ for @createControllerOnNewFabric:error:@
createControllerOnNewFabric_errorSelector :: Selector '[Id MTRDeviceControllerStartupParams, Id NSError] (Id MTRDeviceController)
createControllerOnNewFabric_errorSelector = mkSelector "createControllerOnNewFabric:error:"

-- | @Selector@ for @preWarmCommissioningSession@
preWarmCommissioningSessionSelector :: Selector '[] ()
preWarmCommissioningSessionSelector = mkSelector "preWarmCommissioningSession"

-- | @Selector@ for @running@
runningSelector :: Selector '[] Bool
runningSelector = mkSelector "running"

-- | @Selector@ for @knownFabrics@
knownFabricsSelector :: Selector '[] (Id NSArray)
knownFabricsSelector = mkSelector "knownFabrics"

