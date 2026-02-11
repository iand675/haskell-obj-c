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
  , stateSelector
  , initSelector
  , isValidNextStateSelector
  , didEnterWithPreviousStateSelector
  , updateWithDeltaTimeSelector
  , willExitWithNextStateSelector
  , stateMachineSelector


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

-- | Creates a new state to be used in a state machine.
--
-- See: GKStateMachine
--
-- ObjC selector: @+ state@
state :: IO (Id GKState)
state  =
  do
    cls' <- getRequiredClass "GKState"
    sendClassMsg cls' (mkSelector "state") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsGKState gkState => gkState -> IO (Id GKState)
init_ gkState  =
  sendMsg gkState (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
isValidNextState gkState  stateClass =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg gkState (mkSelector "isValidNextState:") retCULong [argPtr (unClass stateClass)]

-- | Called by GKStateMachine when this state is entered.
--
-- @previousState@ — the state that was exited to enter this state.  This is nil if this is the state machine's first entered state.
--
-- See: stateMachineWithStates:initialStateClass:
--
-- ObjC selector: @- didEnterWithPreviousState:@
didEnterWithPreviousState :: (IsGKState gkState, IsGKState previousState) => gkState -> previousState -> IO ()
didEnterWithPreviousState gkState  previousState =
withObjCPtr previousState $ \raw_previousState ->
    sendMsg gkState (mkSelector "didEnterWithPreviousState:") retVoid [argPtr (castPtr raw_previousState :: Ptr ())]

-- | Called by GKStateMachine when it is updated
--
-- @seconds@ — the time in seconds since the last update
--
-- ObjC selector: @- updateWithDeltaTime:@
updateWithDeltaTime :: IsGKState gkState => gkState -> CDouble -> IO ()
updateWithDeltaTime gkState  seconds =
  sendMsg gkState (mkSelector "updateWithDeltaTime:") retVoid [argCDouble (fromIntegral seconds)]

-- | Called by GKStateMachine when this state is exited
--
-- @nextState@ — the state that is being entered next
--
-- ObjC selector: @- willExitWithNextState:@
willExitWithNextState :: (IsGKState gkState, IsGKState nextState) => gkState -> nextState -> IO ()
willExitWithNextState gkState  nextState =
withObjCPtr nextState $ \raw_nextState ->
    sendMsg gkState (mkSelector "willExitWithNextState:") retVoid [argPtr (castPtr raw_nextState :: Ptr ())]

-- | The state machine that this state is associated with. This is nil if this state hasn't been added to a state machine yet.
--
-- ObjC selector: @- stateMachine@
stateMachine :: IsGKState gkState => gkState -> IO (Id GKStateMachine)
stateMachine gkState  =
  sendMsg gkState (mkSelector "stateMachine") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @isValidNextState:@
isValidNextStateSelector :: Selector
isValidNextStateSelector = mkSelector "isValidNextState:"

-- | @Selector@ for @didEnterWithPreviousState:@
didEnterWithPreviousStateSelector :: Selector
didEnterWithPreviousStateSelector = mkSelector "didEnterWithPreviousState:"

-- | @Selector@ for @updateWithDeltaTime:@
updateWithDeltaTimeSelector :: Selector
updateWithDeltaTimeSelector = mkSelector "updateWithDeltaTime:"

-- | @Selector@ for @willExitWithNextState:@
willExitWithNextStateSelector :: Selector
willExitWithNextStateSelector = mkSelector "willExitWithNextState:"

-- | @Selector@ for @stateMachine@
stateMachineSelector :: Selector
stateMachineSelector = mkSelector "stateMachine"

