{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Models a finite state machine that has a single current state.
--
-- Generated bindings for @GKStateMachine@.
module ObjC.GameplayKit.GKStateMachine
  ( GKStateMachine
  , IsGKStateMachine(..)
  , stateMachineWithStates
  , initWithStates
  , updateWithDeltaTime
  , stateForClass
  , canEnterState
  , enterState
  , currentState
  , stateMachineWithStatesSelector
  , initWithStatesSelector
  , updateWithDeltaTimeSelector
  , stateForClassSelector
  , canEnterStateSelector
  , enterStateSelector
  , currentStateSelector


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

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a state machine with an array of possible states and an initial state.
--
-- @states@ — a list of possible states for this state machine.
--
-- ObjC selector: @+ stateMachineWithStates:@
stateMachineWithStates :: IsNSArray states => states -> IO (Id GKStateMachine)
stateMachineWithStates states =
  do
    cls' <- getRequiredClass "GKStateMachine"
    withObjCPtr states $ \raw_states ->
      sendClassMsg cls' (mkSelector "stateMachineWithStates:") (retPtr retVoid) [argPtr (castPtr raw_states :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithStates:@
initWithStates :: (IsGKStateMachine gkStateMachine, IsNSArray states) => gkStateMachine -> states -> IO (Id GKStateMachine)
initWithStates gkStateMachine  states =
withObjCPtr states $ \raw_states ->
    sendMsg gkStateMachine (mkSelector "initWithStates:") (retPtr retVoid) [argPtr (castPtr raw_states :: Ptr ())] >>= ownedObject . castPtr

-- | Updates the current state machine.
--
-- @sec@ — the time, in seconds, since the last frame
--
-- ObjC selector: @- updateWithDeltaTime:@
updateWithDeltaTime :: IsGKStateMachine gkStateMachine => gkStateMachine -> CDouble -> IO ()
updateWithDeltaTime gkStateMachine  sec =
  sendMsg gkStateMachine (mkSelector "updateWithDeltaTime:") retVoid [argCDouble (fromIntegral sec)]

-- | Gets the instance of the indicated state class from this state machine. Returns nil if the state does not exist
--
-- @stateClass@ — the class of the state to be retrieved
--
-- ObjC selector: @- stateForClass:@
stateForClass :: IsGKStateMachine gkStateMachine => gkStateMachine -> Class -> IO (Id GKState)
stateForClass gkStateMachine  stateClass =
  sendMsg gkStateMachine (mkSelector "stateForClass:") (retPtr retVoid) [argPtr (unClass stateClass)] >>= retainedObject . castPtr

-- | Returns YES if the indicated class is a a valid next state or if currentState is nil
--
-- @stateClass@ — the class of the state to be tested
--
-- ObjC selector: @- canEnterState:@
canEnterState :: IsGKStateMachine gkStateMachine => gkStateMachine -> Class -> IO Bool
canEnterState gkStateMachine  stateClass =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkStateMachine (mkSelector "canEnterState:") retCULong [argPtr (unClass stateClass)]

-- | Calls canEnterState to check if we can enter the given state and then enters that state if so. [GKState willExitWithNextState:] is called on the old current state. [GKState didEnterWithPreviousState:] is called on the new state.
--
-- @stateClass@ — the class of the state to switch to
--
-- Returns: YES if state was entered.  NO otherwise.
--
-- ObjC selector: @- enterState:@
enterState :: IsGKStateMachine gkStateMachine => gkStateMachine -> Class -> IO Bool
enterState gkStateMachine  stateClass =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkStateMachine (mkSelector "enterState:") retCULong [argPtr (unClass stateClass)]

-- | The current state that the state machine is in. Prior to the first called to enterState this is equal to nil.
--
-- ObjC selector: @- currentState@
currentState :: IsGKStateMachine gkStateMachine => gkStateMachine -> IO (Id GKState)
currentState gkStateMachine  =
  sendMsg gkStateMachine (mkSelector "currentState") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stateMachineWithStates:@
stateMachineWithStatesSelector :: Selector
stateMachineWithStatesSelector = mkSelector "stateMachineWithStates:"

-- | @Selector@ for @initWithStates:@
initWithStatesSelector :: Selector
initWithStatesSelector = mkSelector "initWithStates:"

-- | @Selector@ for @updateWithDeltaTime:@
updateWithDeltaTimeSelector :: Selector
updateWithDeltaTimeSelector = mkSelector "updateWithDeltaTime:"

-- | @Selector@ for @stateForClass:@
stateForClassSelector :: Selector
stateForClassSelector = mkSelector "stateForClass:"

-- | @Selector@ for @canEnterState:@
canEnterStateSelector :: Selector
canEnterStateSelector = mkSelector "canEnterState:"

-- | @Selector@ for @enterState:@
enterStateSelector :: Selector
enterStateSelector = mkSelector "enterState:"

-- | @Selector@ for @currentState@
currentStateSelector :: Selector
currentStateSelector = mkSelector "currentState"

