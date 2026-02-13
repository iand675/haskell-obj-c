{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A type that represents a container's status.
--
-- This type contains two properties:
--
-- * The ``state`` value that indicates the state of the container, such as ``FSContainerState/ready`` or ``FSContainerState/blocked``. * The ``status`` is an error (optional in Swift, nullable in Objective-C) that provides further information about the state, such as why the container is blocked.
--
-- Examples of statuses that require intervention include errors that indicate the container isn't ready (POSIX @EAGAIN@ or @ENOTCONN@), the container needs authentication (@ENEEDAUTH@), or that authentication failed (@EAUTH@). The status can also be an informative error, such as the FSKit error ``FSError/Code/statusOperationInProgress``.
--
-- Generated bindings for @FSContainerStatus@.
module ObjC.FSKit.FSContainerStatus
  ( FSContainerStatus
  , IsFSContainerStatus(..)
  , init_
  , activeWithStatus
  , blockedWithStatus
  , notReadyWithStatus
  , readyWithStatus
  , state
  , status
  , active
  , ready
  , activeSelector
  , activeWithStatusSelector
  , blockedWithStatusSelector
  , initSelector
  , notReadyWithStatusSelector
  , readySelector
  , readyWithStatusSelector
  , stateSelector
  , statusSelector

  -- * Enum types
  , FSContainerState(FSContainerState)
  , pattern FSContainerStateNotReady
  , pattern FSContainerStateBlocked
  , pattern FSContainerStateReady
  , pattern FSContainerStateActive

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FSKit.Internal.Classes
import ObjC.FSKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsFSContainerStatus fsContainerStatus => fsContainerStatus -> IO (Id FSContainerStatus)
init_ fsContainerStatus =
  sendOwnedMessage fsContainerStatus initSelector

-- | Returns a active container status instance with the provided error status.
--
-- - Parameter errorStatus: The error status, if any, for the new instance.
--
-- ObjC selector: @+ activeWithStatus:@
activeWithStatus :: IsNSError errorStatus => errorStatus -> IO (Id FSContainerStatus)
activeWithStatus errorStatus =
  do
    cls' <- getRequiredClass "FSContainerStatus"
    sendClassMessage cls' activeWithStatusSelector (toNSError errorStatus)

-- | Returns a blocked container status instance with the provided error status.
--
-- - Parameter errorStatus: The error status, if any, for the new instance.
--
-- ObjC selector: @+ blockedWithStatus:@
blockedWithStatus :: IsNSError errorStatus => errorStatus -> IO (Id FSContainerStatus)
blockedWithStatus errorStatus =
  do
    cls' <- getRequiredClass "FSContainerStatus"
    sendClassMessage cls' blockedWithStatusSelector (toNSError errorStatus)

-- | Returns a not-ready container status instance with the provided error status.
--
-- - Parameter errorStatus: The error status, if any, for the new instance.
--
-- ObjC selector: @+ notReadyWithStatus:@
notReadyWithStatus :: IsNSError errorStatus => errorStatus -> IO (Id FSContainerStatus)
notReadyWithStatus errorStatus =
  do
    cls' <- getRequiredClass "FSContainerStatus"
    sendClassMessage cls' notReadyWithStatusSelector (toNSError errorStatus)

-- | Returns a ready container status instance with the provided error status.
--
-- - Parameter errorStatus: The error status, if any, for the new instance.
--
-- ObjC selector: @+ readyWithStatus:@
readyWithStatus :: IsNSError errorStatus => errorStatus -> IO (Id FSContainerStatus)
readyWithStatus errorStatus =
  do
    cls' <- getRequiredClass "FSContainerStatus"
    sendClassMessage cls' readyWithStatusSelector (toNSError errorStatus)

-- | A value that represents the container state, such as ready, active, or blocked.
--
-- ObjC selector: @- state@
state :: IsFSContainerStatus fsContainerStatus => fsContainerStatus -> IO FSContainerState
state fsContainerStatus =
  sendMessage fsContainerStatus stateSelector

-- | An optional error that provides further information about the state.
--
-- ObjC selector: @- status@
status :: IsFSContainerStatus fsContainerStatus => fsContainerStatus -> IO (Id NSError)
status fsContainerStatus =
  sendMessage fsContainerStatus statusSelector

-- | A status that represents an active container with no error.
--
-- This value is a ``FSContainerStatus`` with a ``state`` that is ``active``, and has a ``status`` that is @nil@.
--
-- ObjC selector: @+ active@
active :: IO (Id FSContainerStatus)
active  =
  do
    cls' <- getRequiredClass "FSContainerStatus"
    sendClassMessage cls' activeSelector

-- | A status that represents a ready container with no error.
--
-- This value is a ``FSContainerStatus`` with a ``state`` that is ``ready``, and a ``status`` that is @nil@.
--
-- ObjC selector: @+ ready@
ready :: IO (Id FSContainerStatus)
ready  =
  do
    cls' <- getRequiredClass "FSContainerStatus"
    sendClassMessage cls' readySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id FSContainerStatus)
initSelector = mkSelector "init"

-- | @Selector@ for @activeWithStatus:@
activeWithStatusSelector :: Selector '[Id NSError] (Id FSContainerStatus)
activeWithStatusSelector = mkSelector "activeWithStatus:"

-- | @Selector@ for @blockedWithStatus:@
blockedWithStatusSelector :: Selector '[Id NSError] (Id FSContainerStatus)
blockedWithStatusSelector = mkSelector "blockedWithStatus:"

-- | @Selector@ for @notReadyWithStatus:@
notReadyWithStatusSelector :: Selector '[Id NSError] (Id FSContainerStatus)
notReadyWithStatusSelector = mkSelector "notReadyWithStatus:"

-- | @Selector@ for @readyWithStatus:@
readyWithStatusSelector :: Selector '[Id NSError] (Id FSContainerStatus)
readyWithStatusSelector = mkSelector "readyWithStatus:"

-- | @Selector@ for @state@
stateSelector :: Selector '[] FSContainerState
stateSelector = mkSelector "state"

-- | @Selector@ for @status@
statusSelector :: Selector '[] (Id NSError)
statusSelector = mkSelector "status"

-- | @Selector@ for @active@
activeSelector :: Selector '[] (Id FSContainerStatus)
activeSelector = mkSelector "active"

-- | @Selector@ for @ready@
readySelector :: Selector '[] (Id FSContainerStatus)
readySelector = mkSelector "ready"

