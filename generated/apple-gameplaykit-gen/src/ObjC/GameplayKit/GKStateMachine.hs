{-# LANGUAGE DataKinds #-}
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
  , canEnterStateSelector
  , currentStateSelector
  , enterStateSelector
  , initWithStatesSelector
  , stateForClassSelector
  , stateMachineWithStatesSelector
  , updateWithDeltaTimeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' stateMachineWithStatesSelector (toNSArray states)

-- | @- initWithStates:@
initWithStates :: (IsGKStateMachine gkStateMachine, IsNSArray states) => gkStateMachine -> states -> IO (Id GKStateMachine)
initWithStates gkStateMachine states =
  sendOwnedMessage gkStateMachine initWithStatesSelector (toNSArray states)

-- | Updates the current state machine.
--
-- @sec@ — the time, in seconds, since the last frame
--
-- ObjC selector: @- updateWithDeltaTime:@
updateWithDeltaTime :: IsGKStateMachine gkStateMachine => gkStateMachine -> CDouble -> IO ()
updateWithDeltaTime gkStateMachine sec =
  sendMessage gkStateMachine updateWithDeltaTimeSelector sec

-- | Gets the instance of the indicated state class from this state machine. Returns nil if the state does not exist
--
-- @stateClass@ — the class of the state to be retrieved
--
-- ObjC selector: @- stateForClass:@
stateForClass :: IsGKStateMachine gkStateMachine => gkStateMachine -> Class -> IO (Id GKState)
stateForClass gkStateMachine stateClass =
  sendMessage gkStateMachine stateForClassSelector stateClass

-- | Returns YES if the indicated class is a a valid next state or if currentState is nil
--
-- @stateClass@ — the class of the state to be tested
--
-- ObjC selector: @- canEnterState:@
canEnterState :: IsGKStateMachine gkStateMachine => gkStateMachine -> Class -> IO Bool
canEnterState gkStateMachine stateClass =
  sendMessage gkStateMachine canEnterStateSelector stateClass

-- | Calls canEnterState to check if we can enter the given state and then enters that state if so. [GKState willExitWithNextState:] is called on the old current state. [GKState didEnterWithPreviousState:] is called on the new state.
--
-- @stateClass@ — the class of the state to switch to
--
-- Returns: YES if state was entered.  NO otherwise.
--
-- ObjC selector: @- enterState:@
enterState :: IsGKStateMachine gkStateMachine => gkStateMachine -> Class -> IO Bool
enterState gkStateMachine stateClass =
  sendMessage gkStateMachine enterStateSelector stateClass

-- | The current state that the state machine is in. Prior to the first called to enterState this is equal to nil.
--
-- ObjC selector: @- currentState@
currentState :: IsGKStateMachine gkStateMachine => gkStateMachine -> IO (Id GKState)
currentState gkStateMachine =
  sendMessage gkStateMachine currentStateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stateMachineWithStates:@
stateMachineWithStatesSelector :: Selector '[Id NSArray] (Id GKStateMachine)
stateMachineWithStatesSelector = mkSelector "stateMachineWithStates:"

-- | @Selector@ for @initWithStates:@
initWithStatesSelector :: Selector '[Id NSArray] (Id GKStateMachine)
initWithStatesSelector = mkSelector "initWithStates:"

-- | @Selector@ for @updateWithDeltaTime:@
updateWithDeltaTimeSelector :: Selector '[CDouble] ()
updateWithDeltaTimeSelector = mkSelector "updateWithDeltaTime:"

-- | @Selector@ for @stateForClass:@
stateForClassSelector :: Selector '[Class] (Id GKState)
stateForClassSelector = mkSelector "stateForClass:"

-- | @Selector@ for @canEnterState:@
canEnterStateSelector :: Selector '[Class] Bool
canEnterStateSelector = mkSelector "canEnterState:"

-- | @Selector@ for @enterState:@
enterStateSelector :: Selector '[Class] Bool
enterStateSelector = mkSelector "enterState:"

-- | @Selector@ for @currentState@
currentStateSelector :: Selector '[] (Id GKState)
currentStateSelector = mkSelector "currentState"

