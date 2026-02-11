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
  , newSelector
  , initSelector
  , addObserverSelector
  , removeObserverSelector
  , currentUserSelector
  , stateSelector


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

import ObjC.LocalAuthentication.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The clients should use @currentUser@ class property.
--
-- ObjC selector: @+ new@
new :: IO (Id LAEnvironment)
new  =
  do
    cls' <- getRequiredClass "LAEnvironment"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The clients should use  @currentUser@ class property.
--
-- ObjC selector: @- init@
init_ :: IsLAEnvironment laEnvironment => laEnvironment -> IO (Id LAEnvironment)
init_ laEnvironment  =
  sendMsg laEnvironment (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Adds observer to monitor changes of the environment.
--
-- The observer will be held weakly so its instance should be kept alive by the caller.
--
-- ObjC selector: @- addObserver:@
addObserver :: IsLAEnvironment laEnvironment => laEnvironment -> RawId -> IO ()
addObserver laEnvironment  observer =
  sendMsg laEnvironment (mkSelector "addObserver:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ())]

-- | Removes the previously registered observer.
--
-- If the observer is deallocated, it will be removed automatically.
--
-- ObjC selector: @- removeObserver:@
removeObserver :: IsLAEnvironment laEnvironment => laEnvironment -> RawId -> IO ()
removeObserver laEnvironment  observer =
  sendMsg laEnvironment (mkSelector "removeObserver:") retVoid [argPtr (castPtr (unRawId observer) :: Ptr ())]

-- | Environment of the current user.
--
-- ObjC selector: @+ currentUser@
currentUser :: IO (Id LAEnvironment)
currentUser  =
  do
    cls' <- getRequiredClass "LAEnvironment"
    sendClassMsg cls' (mkSelector "currentUser") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The environment state information.
--
-- ObjC selector: @- state@
state :: IsLAEnvironment laEnvironment => laEnvironment -> IO (Id LAEnvironmentState)
state laEnvironment  =
  sendMsg laEnvironment (mkSelector "state") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @addObserver:@
addObserverSelector :: Selector
addObserverSelector = mkSelector "addObserver:"

-- | @Selector@ for @removeObserver:@
removeObserverSelector :: Selector
removeObserverSelector = mkSelector "removeObserver:"

-- | @Selector@ for @currentUser@
currentUserSelector :: Selector
currentUserSelector = mkSelector "currentUser"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

