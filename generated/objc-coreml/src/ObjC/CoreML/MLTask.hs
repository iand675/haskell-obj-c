{-# LANGUAGE PatternSynonyms #-}
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
  , resumeSelector
  , cancelSelector
  , initSelector
  , newSelector
  , taskIdentifierSelector
  , stateSelector
  , errorSelector

  -- * Enum types
  , MLTaskState(MLTaskState)
  , pattern MLTaskStateSuspended
  , pattern MLTaskStateRunning
  , pattern MLTaskStateCancelling
  , pattern MLTaskStateCompleted
  , pattern MLTaskStateFailed

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

import ObjC.CoreML.Internal.Classes
import ObjC.CoreML.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- resume@
resume :: IsMLTask mlTask => mlTask -> IO ()
resume mlTask  =
  sendMsg mlTask (mkSelector "resume") retVoid []

-- | @- cancel@
cancel :: IsMLTask mlTask => mlTask -> IO ()
cancel mlTask  =
  sendMsg mlTask (mkSelector "cancel") retVoid []

-- | @- init@
init_ :: IsMLTask mlTask => mlTask -> IO (Id MLTask)
init_ mlTask  =
  sendMsg mlTask (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO RawId
new  =
  do
    cls' <- getRequiredClass "MLTask"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "new") (retPtr retVoid) []

-- | @- taskIdentifier@
taskIdentifier :: IsMLTask mlTask => mlTask -> IO (Id NSString)
taskIdentifier mlTask  =
  sendMsg mlTask (mkSelector "taskIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- state@
state :: IsMLTask mlTask => mlTask -> IO MLTaskState
state mlTask  =
  fmap (coerce :: CLong -> MLTaskState) $ sendMsg mlTask (mkSelector "state") retCLong []

-- | @- error@
error_ :: IsMLTask mlTask => mlTask -> IO (Id NSError)
error_ mlTask  =
  sendMsg mlTask (mkSelector "error") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resume@
resumeSelector :: Selector
resumeSelector = mkSelector "resume"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @taskIdentifier@
taskIdentifierSelector :: Selector
taskIdentifierSelector = mkSelector "taskIdentifier"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @error@
errorSelector :: Selector
errorSelector = mkSelector "error"

