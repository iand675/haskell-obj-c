{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a single state in a state machine. By default, states allow transitions freely to and from the states in the machine.
--
-- If a more restricted set of valid transitions are needed in the state machine, you may override isValidNextState: where applicable.
--
-- See: GKStateMachine
--
-- See: isValidNextState:
--
-- Generated bindings for @GKState@.
module ObjC.GameplayKit.GKState
  ( GKState
  , IsGKState(..)
  , state
  , init_
  , isValidNextState
  , didEnterWithPreviousState
  , updateWithDeltaTime
  , willExitWithNextState
  , stateMachine
  , didEnterWithPreviousStateSelector
  , initSelector
  , isValidNextStateSelector
  , stateMachineSelector
  , stateSelector
  , updateWithDeltaTimeSelector
  , willExitWithNextStateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a new state to be used in a state machine.
--
-- See: GKStateMachine
--
-- ObjC selector: @+ state@
state :: IO (Id GKState)
state  =
  do
    cls' <- getRequiredClass "GKState"
    sendClassMessage cls' stateSelector

-- | @- init@
init_ :: IsGKState gkState => gkState -> IO (Id GKState)
init_ gkState =
  sendOwnedMessage gkState initSelector

-- | Returns YES if the given class is a valid next state to enter.
--
-- By default GKState will return YES for any class that is subclass of GKState. Override this in a subclass to enforce limited edge traversals in the state machine.
--
-- See: GKStateMachine.canEnterState:
--
-- See: GKStateMachine.enterState:
--
-- @stateClass@ — the class to be checked
--
-- Returns: YES if the class is kind of GKState and the state transition is valid, else NO.
--
-- ObjC selector: @- isValidNextState:@
isValidNextState :: IsGKState gkState => gkState -> Class -> IO Bool
isValidNextState gkState stateClass =
  sendMessage gkState isValidNextStateSelector stateClass

-- | Called by GKStateMachine when this state is entered.
--
-- @previousState@ — the state that was exited to enter this state.  This is nil if this is the state machine's first entered state.
--
-- See: stateMachineWithStates:initialStateClass:
--
-- ObjC selector: @- didEnterWithPreviousState:@
didEnterWithPreviousState :: (IsGKState gkState, IsGKState previousState) => gkState -> previousState -> IO ()
didEnterWithPreviousState gkState previousState =
  sendMessage gkState didEnterWithPreviousStateSelector (toGKState previousState)

-- | Called by GKStateMachine when it is updated
--
-- @seconds@ — the time in seconds since the last update
--
-- ObjC selector: @- updateWithDeltaTime:@
updateWithDeltaTime :: IsGKState gkState => gkState -> CDouble -> IO ()
updateWithDeltaTime gkState seconds =
  sendMessage gkState updateWithDeltaTimeSelector seconds

-- | Called by GKStateMachine when this state is exited
--
-- @nextState@ — the state that is being entered next
--
-- ObjC selector: @- willExitWithNextState:@
willExitWithNextState :: (IsGKState gkState, IsGKState nextState) => gkState -> nextState -> IO ()
willExitWithNextState gkState nextState =
  sendMessage gkState willExitWithNextStateSelector (toGKState nextState)

-- | The state machine that this state is associated with. This is nil if this state hasn't been added to a state machine yet.
--
-- ObjC selector: @- stateMachine@
stateMachine :: IsGKState gkState => gkState -> IO (Id GKStateMachine)
stateMachine gkState =
  sendMessage gkState stateMachineSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @state@
stateSelector :: Selector '[] (Id GKState)
stateSelector = mkSelector "state"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id GKState)
initSelector = mkSelector "init"

-- | @Selector@ for @isValidNextState:@
isValidNextStateSelector :: Selector '[Class] Bool
isValidNextStateSelector = mkSelector "isValidNextState:"

-- | @Selector@ for @didEnterWithPreviousState:@
didEnterWithPreviousStateSelector :: Selector '[Id GKState] ()
didEnterWithPreviousStateSelector = mkSelector "didEnterWithPreviousState:"

-- | @Selector@ for @updateWithDeltaTime:@
updateWithDeltaTimeSelector :: Selector '[CDouble] ()
updateWithDeltaTimeSelector = mkSelector "updateWithDeltaTime:"

-- | @Selector@ for @willExitWithNextState:@
willExitWithNextStateSelector :: Selector '[Id GKState] ()
willExitWithNextStateSelector = mkSelector "willExitWithNextState:"

-- | @Selector@ for @stateMachine@
stateMachineSelector :: Selector '[] (Id GKStateMachine)
stateMachineSelector = mkSelector "stateMachine"

