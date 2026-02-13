{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Class that abstracts state transitions and basic task controls.
--
-- Generated bindings for @MLTask@.
module ObjC.CoreML.MLTask
  ( MLTask
  , IsMLTask(..)
  , resume
  , cancel
  , init_
  , new
  , taskIdentifier
  , state
  , error_
  , cancelSelector
  , errorSelector
  , initSelector
  , newSelector
  , resumeSelector
  , stateSelector
  , taskIdentifierSelector

  -- * Enum types
  , MLTaskState(MLTaskState)
  , pattern MLTaskStateSuspended
  , pattern MLTaskStateRunning
  , pattern MLTaskStateCancelling
  , pattern MLTaskStateCompleted
  , pattern MLTaskStateFailed

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreML.Internal.Classes
import ObjC.CoreML.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- resume@
resume :: IsMLTask mlTask => mlTask -> IO ()
resume mlTask =
  sendMessage mlTask resumeSelector

-- | @- cancel@
cancel :: IsMLTask mlTask => mlTask -> IO ()
cancel mlTask =
  sendMessage mlTask cancelSelector

-- | @- init@
init_ :: IsMLTask mlTask => mlTask -> IO (Id MLTask)
init_ mlTask =
  sendOwnedMessage mlTask initSelector

-- | @+ new@
new :: IO RawId
new  =
  do
    cls' <- getRequiredClass "MLTask"
    sendOwnedClassMessage cls' newSelector

-- | @- taskIdentifier@
taskIdentifier :: IsMLTask mlTask => mlTask -> IO (Id NSString)
taskIdentifier mlTask =
  sendMessage mlTask taskIdentifierSelector

-- | @- state@
state :: IsMLTask mlTask => mlTask -> IO MLTaskState
state mlTask =
  sendMessage mlTask stateSelector

-- | @- error@
error_ :: IsMLTask mlTask => mlTask -> IO (Id NSError)
error_ mlTask =
  sendMessage mlTask errorSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resume@
resumeSelector :: Selector '[] ()
resumeSelector = mkSelector "resume"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLTask)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] RawId
newSelector = mkSelector "new"

-- | @Selector@ for @taskIdentifier@
taskIdentifierSelector :: Selector '[] (Id NSString)
taskIdentifierSelector = mkSelector "taskIdentifier"

-- | @Selector@ for @state@
stateSelector :: Selector '[] MLTaskState
stateSelector = mkSelector "state"

-- | @Selector@ for @error@
errorSelector :: Selector '[] (Id NSError)
errorSelector = mkSelector "error"

