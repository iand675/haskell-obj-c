{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRControllerFactory@.
module ObjC.Matter.MTRControllerFactory
  ( MTRControllerFactory
  , IsMTRControllerFactory(..)
  , sharedInstance
  , startup
  , shutdown
  , startControllerOnExistingFabric
  , startControllerOnNewFabric
  , init_
  , new
  , isRunning
  , initSelector
  , isRunningSelector
  , newSelector
  , sharedInstanceSelector
  , shutdownSelector
  , startControllerOnExistingFabricSelector
  , startControllerOnNewFabricSelector
  , startupSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ sharedInstance@
sharedInstance :: IO (Id MTRControllerFactory)
sharedInstance  =
  do
    cls' <- getRequiredClass "MTRControllerFactory"
    sendClassMessage cls' sharedInstanceSelector

-- | @- startup:@
startup :: (IsMTRControllerFactory mtrControllerFactory, IsMTRControllerFactoryParams startupParams) => mtrControllerFactory -> startupParams -> IO Bool
startup mtrControllerFactory startupParams =
  sendMessage mtrControllerFactory startupSelector (toMTRControllerFactoryParams startupParams)

-- | @- shutdown@
shutdown :: IsMTRControllerFactory mtrControllerFactory => mtrControllerFactory -> IO ()
shutdown mtrControllerFactory =
  sendMessage mtrControllerFactory shutdownSelector

-- | @- startControllerOnExistingFabric:@
startControllerOnExistingFabric :: (IsMTRControllerFactory mtrControllerFactory, IsMTRDeviceControllerStartupParams startupParams) => mtrControllerFactory -> startupParams -> IO (Id MTRDeviceController)
startControllerOnExistingFabric mtrControllerFactory startupParams =
  sendMessage mtrControllerFactory startControllerOnExistingFabricSelector (toMTRDeviceControllerStartupParams startupParams)

-- | @- startControllerOnNewFabric:@
startControllerOnNewFabric :: (IsMTRControllerFactory mtrControllerFactory, IsMTRDeviceControllerStartupParams startupParams) => mtrControllerFactory -> startupParams -> IO (Id MTRDeviceController)
startControllerOnNewFabric mtrControllerFactory startupParams =
  sendMessage mtrControllerFactory startControllerOnNewFabricSelector (toMTRDeviceControllerStartupParams startupParams)

-- | @- init@
init_ :: IsMTRControllerFactory mtrControllerFactory => mtrControllerFactory -> IO (Id MTRControllerFactory)
init_ mtrControllerFactory =
  sendOwnedMessage mtrControllerFactory initSelector

-- | @+ new@
new :: IO (Id MTRControllerFactory)
new  =
  do
    cls' <- getRequiredClass "MTRControllerFactory"
    sendOwnedClassMessage cls' newSelector

-- | @- isRunning@
isRunning :: IsMTRControllerFactory mtrControllerFactory => mtrControllerFactory -> IO Bool
isRunning mtrControllerFactory =
  sendMessage mtrControllerFactory isRunningSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedInstance@
sharedInstanceSelector :: Selector '[] (Id MTRControllerFactory)
sharedInstanceSelector = mkSelector "sharedInstance"

-- | @Selector@ for @startup:@
startupSelector :: Selector '[Id MTRControllerFactoryParams] Bool
startupSelector = mkSelector "startup:"

-- | @Selector@ for @shutdown@
shutdownSelector :: Selector '[] ()
shutdownSelector = mkSelector "shutdown"

-- | @Selector@ for @startControllerOnExistingFabric:@
startControllerOnExistingFabricSelector :: Selector '[Id MTRDeviceControllerStartupParams] (Id MTRDeviceController)
startControllerOnExistingFabricSelector = mkSelector "startControllerOnExistingFabric:"

-- | @Selector@ for @startControllerOnNewFabric:@
startControllerOnNewFabricSelector :: Selector '[Id MTRDeviceControllerStartupParams] (Id MTRDeviceController)
startControllerOnNewFabricSelector = mkSelector "startControllerOnNewFabric:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRControllerFactory)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRControllerFactory)
newSelector = mkSelector "new"

-- | @Selector@ for @isRunning@
isRunningSelector :: Selector '[] Bool
isRunningSelector = mkSelector "isRunning"

