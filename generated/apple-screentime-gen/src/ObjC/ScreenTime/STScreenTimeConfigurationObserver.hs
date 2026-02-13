{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The object you use to observe changes to the current configuration.
--
-- Use this class to start and stop observing the current configuration. For example, you can opt to disable private browsing in your web browser’s view controller when ``STScreenTimeConfiguration/enforcesChildRestrictions`` is @true@.
--
-- Generated bindings for @STScreenTimeConfigurationObserver@.
module ObjC.ScreenTime.STScreenTimeConfigurationObserver
  ( STScreenTimeConfigurationObserver
  , IsSTScreenTimeConfigurationObserver(..)
  , initWithUpdateQueue
  , startObserving
  , stopObserving
  , init_
  , new
  , configuration
  , configurationSelector
  , initSelector
  , initWithUpdateQueueSelector
  , newSelector
  , startObservingSelector
  , stopObservingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ScreenTime.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a configuration observer that reports updates on the queue you specify.
--
-- - Parameters:   - updateQueue: The queue on which to report updates.
--
-- ObjC selector: @- initWithUpdateQueue:@
initWithUpdateQueue :: (IsSTScreenTimeConfigurationObserver stScreenTimeConfigurationObserver, IsNSObject updateQueue) => stScreenTimeConfigurationObserver -> updateQueue -> IO (Id STScreenTimeConfigurationObserver)
initWithUpdateQueue stScreenTimeConfigurationObserver updateQueue =
  sendOwnedMessage stScreenTimeConfigurationObserver initWithUpdateQueueSelector (toNSObject updateQueue)

-- | Starts observing changes to the current configuration.
--
-- ObjC selector: @- startObserving@
startObserving :: IsSTScreenTimeConfigurationObserver stScreenTimeConfigurationObserver => stScreenTimeConfigurationObserver -> IO ()
startObserving stScreenTimeConfigurationObserver =
  sendMessage stScreenTimeConfigurationObserver startObservingSelector

-- | Stops observing changes to the current configuration.
--
-- ObjC selector: @- stopObserving@
stopObserving :: IsSTScreenTimeConfigurationObserver stScreenTimeConfigurationObserver => stScreenTimeConfigurationObserver -> IO ()
stopObserving stScreenTimeConfigurationObserver =
  sendMessage stScreenTimeConfigurationObserver stopObservingSelector

-- | @- init@
init_ :: IsSTScreenTimeConfigurationObserver stScreenTimeConfigurationObserver => stScreenTimeConfigurationObserver -> IO (Id STScreenTimeConfigurationObserver)
init_ stScreenTimeConfigurationObserver =
  sendOwnedMessage stScreenTimeConfigurationObserver initSelector

-- | @+ new@
new :: IO (Id STScreenTimeConfigurationObserver)
new  =
  do
    cls' <- getRequiredClass "STScreenTimeConfigurationObserver"
    sendOwnedClassMessage cls' newSelector

-- | The configuration being observed.
--
-- ObjC selector: @- configuration@
configuration :: IsSTScreenTimeConfigurationObserver stScreenTimeConfigurationObserver => stScreenTimeConfigurationObserver -> IO (Id STScreenTimeConfiguration)
configuration stScreenTimeConfigurationObserver =
  sendMessage stScreenTimeConfigurationObserver configurationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithUpdateQueue:@
initWithUpdateQueueSelector :: Selector '[Id NSObject] (Id STScreenTimeConfigurationObserver)
initWithUpdateQueueSelector = mkSelector "initWithUpdateQueue:"

-- | @Selector@ for @startObserving@
startObservingSelector :: Selector '[] ()
startObservingSelector = mkSelector "startObserving"

-- | @Selector@ for @stopObserving@
stopObservingSelector :: Selector '[] ()
stopObservingSelector = mkSelector "stopObserving"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id STScreenTimeConfigurationObserver)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id STScreenTimeConfigurationObserver)
newSelector = mkSelector "new"

-- | @Selector@ for @configuration@
configurationSelector :: Selector '[] (Id STScreenTimeConfiguration)
configurationSelector = mkSelector "configuration"

