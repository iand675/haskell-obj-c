{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @LAEnvironment@.
module ObjC.LocalAuthentication.LAEnvironment
  ( LAEnvironment
  , IsLAEnvironment(..)
  , new
  , init_
  , addObserver
  , removeObserver
  , currentUser
  , state
  , addObserverSelector
  , currentUserSelector
  , initSelector
  , newSelector
  , removeObserverSelector
  , stateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The clients should use @currentUser@ class property.
--
-- ObjC selector: @+ new@
new :: IO (Id LAEnvironment)
new  =
  do
    cls' <- getRequiredClass "LAEnvironment"
    sendOwnedClassMessage cls' newSelector

-- | The clients should use  @currentUser@ class property.
--
-- ObjC selector: @- init@
init_ :: IsLAEnvironment laEnvironment => laEnvironment -> IO (Id LAEnvironment)
init_ laEnvironment =
  sendOwnedMessage laEnvironment initSelector

-- | Adds observer to monitor changes of the environment.
--
-- The observer will be held weakly so its instance should be kept alive by the caller.
--
-- ObjC selector: @- addObserver:@
addObserver :: IsLAEnvironment laEnvironment => laEnvironment -> RawId -> IO ()
addObserver laEnvironment observer =
  sendMessage laEnvironment addObserverSelector observer

-- | Removes the previously registered observer.
--
-- If the observer is deallocated, it will be removed automatically.
--
-- ObjC selector: @- removeObserver:@
removeObserver :: IsLAEnvironment laEnvironment => laEnvironment -> RawId -> IO ()
removeObserver laEnvironment observer =
  sendMessage laEnvironment removeObserverSelector observer

-- | Environment of the current user.
--
-- ObjC selector: @+ currentUser@
currentUser :: IO (Id LAEnvironment)
currentUser  =
  do
    cls' <- getRequiredClass "LAEnvironment"
    sendClassMessage cls' currentUserSelector

-- | The environment state information.
--
-- ObjC selector: @- state@
state :: IsLAEnvironment laEnvironment => laEnvironment -> IO (Id LAEnvironmentState)
state laEnvironment =
  sendMessage laEnvironment stateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id LAEnvironment)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id LAEnvironment)
initSelector = mkSelector "init"

-- | @Selector@ for @addObserver:@
addObserverSelector :: Selector '[RawId] ()
addObserverSelector = mkSelector "addObserver:"

-- | @Selector@ for @removeObserver:@
removeObserverSelector :: Selector '[RawId] ()
removeObserverSelector = mkSelector "removeObserver:"

-- | @Selector@ for @currentUser@
currentUserSelector :: Selector '[] (Id LAEnvironment)
currentUserSelector = mkSelector "currentUser"

-- | @Selector@ for @state@
stateSelector :: Selector '[] (Id LAEnvironmentState)
stateSelector = mkSelector "state"

