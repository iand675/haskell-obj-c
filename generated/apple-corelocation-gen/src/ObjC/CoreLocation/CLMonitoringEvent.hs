{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLMonitoringEvent@.
module ObjC.CoreLocation.CLMonitoringEvent
  ( CLMonitoringEvent
  , IsCLMonitoringEvent(..)
  , init_
  , new
  , identifier
  , refinement
  , state
  , date
  , authorizationDenied
  , authorizationDeniedGlobally
  , authorizationRestricted
  , insufficientlyInUse
  , accuracyLimited
  , conditionUnsupported
  , conditionLimitExceeded
  , persistenceUnavailable
  , serviceSessionRequired
  , authorizationRequestInProgress
  , accuracyLimitedSelector
  , authorizationDeniedGloballySelector
  , authorizationDeniedSelector
  , authorizationRequestInProgressSelector
  , authorizationRestrictedSelector
  , conditionLimitExceededSelector
  , conditionUnsupportedSelector
  , dateSelector
  , identifierSelector
  , initSelector
  , insufficientlyInUseSelector
  , newSelector
  , persistenceUnavailableSelector
  , refinementSelector
  , serviceSessionRequiredSelector
  , stateSelector

  -- * Enum types
  , CLMonitoringState(CLMonitoringState)
  , pattern CLMonitoringStateUnknown
  , pattern CLMonitoringStateSatisfied
  , pattern CLMonitoringStateUnsatisfied
  , pattern CLMonitoringStateUnmonitored

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreLocation.Internal.Classes
import ObjC.CoreLocation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO (Id CLMonitoringEvent)
init_ clMonitoringEvent =
  sendOwnedMessage clMonitoringEvent initSelector

-- | @+ new@
new :: IO (Id CLMonitoringEvent)
new  =
  do
    cls' <- getRequiredClass "CLMonitoringEvent"
    sendOwnedClassMessage cls' newSelector

-- | @- identifier@
identifier :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO (Id NSString)
identifier clMonitoringEvent =
  sendMessage clMonitoringEvent identifierSelector

-- | @- refinement@
refinement :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO (Id CLCondition)
refinement clMonitoringEvent =
  sendMessage clMonitoringEvent refinementSelector

-- | @- state@
state :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO CLMonitoringState
state clMonitoringEvent =
  sendMessage clMonitoringEvent stateSelector

-- | @- date@
date :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO (Id NSDate)
date clMonitoringEvent =
  sendMessage clMonitoringEvent dateSelector

-- | @- authorizationDenied@
authorizationDenied :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO Bool
authorizationDenied clMonitoringEvent =
  sendMessage clMonitoringEvent authorizationDeniedSelector

-- | @- authorizationDeniedGlobally@
authorizationDeniedGlobally :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO Bool
authorizationDeniedGlobally clMonitoringEvent =
  sendMessage clMonitoringEvent authorizationDeniedGloballySelector

-- | @- authorizationRestricted@
authorizationRestricted :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO Bool
authorizationRestricted clMonitoringEvent =
  sendMessage clMonitoringEvent authorizationRestrictedSelector

-- | @- insufficientlyInUse@
insufficientlyInUse :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO Bool
insufficientlyInUse clMonitoringEvent =
  sendMessage clMonitoringEvent insufficientlyInUseSelector

-- | @- accuracyLimited@
accuracyLimited :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO Bool
accuracyLimited clMonitoringEvent =
  sendMessage clMonitoringEvent accuracyLimitedSelector

-- | @- conditionUnsupported@
conditionUnsupported :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO Bool
conditionUnsupported clMonitoringEvent =
  sendMessage clMonitoringEvent conditionUnsupportedSelector

-- | @- conditionLimitExceeded@
conditionLimitExceeded :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO Bool
conditionLimitExceeded clMonitoringEvent =
  sendMessage clMonitoringEvent conditionLimitExceededSelector

-- | @- persistenceUnavailable@
persistenceUnavailable :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO Bool
persistenceUnavailable clMonitoringEvent =
  sendMessage clMonitoringEvent persistenceUnavailableSelector

-- | @- serviceSessionRequired@
serviceSessionRequired :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO Bool
serviceSessionRequired clMonitoringEvent =
  sendMessage clMonitoringEvent serviceSessionRequiredSelector

-- | @- authorizationRequestInProgress@
authorizationRequestInProgress :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO Bool
authorizationRequestInProgress clMonitoringEvent =
  sendMessage clMonitoringEvent authorizationRequestInProgressSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CLMonitoringEvent)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CLMonitoringEvent)
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @refinement@
refinementSelector :: Selector '[] (Id CLCondition)
refinementSelector = mkSelector "refinement"

-- | @Selector@ for @state@
stateSelector :: Selector '[] CLMonitoringState
stateSelector = mkSelector "state"

-- | @Selector@ for @date@
dateSelector :: Selector '[] (Id NSDate)
dateSelector = mkSelector "date"

-- | @Selector@ for @authorizationDenied@
authorizationDeniedSelector :: Selector '[] Bool
authorizationDeniedSelector = mkSelector "authorizationDenied"

-- | @Selector@ for @authorizationDeniedGlobally@
authorizationDeniedGloballySelector :: Selector '[] Bool
authorizationDeniedGloballySelector = mkSelector "authorizationDeniedGlobally"

-- | @Selector@ for @authorizationRestricted@
authorizationRestrictedSelector :: Selector '[] Bool
authorizationRestrictedSelector = mkSelector "authorizationRestricted"

-- | @Selector@ for @insufficientlyInUse@
insufficientlyInUseSelector :: Selector '[] Bool
insufficientlyInUseSelector = mkSelector "insufficientlyInUse"

-- | @Selector@ for @accuracyLimited@
accuracyLimitedSelector :: Selector '[] Bool
accuracyLimitedSelector = mkSelector "accuracyLimited"

-- | @Selector@ for @conditionUnsupported@
conditionUnsupportedSelector :: Selector '[] Bool
conditionUnsupportedSelector = mkSelector "conditionUnsupported"

-- | @Selector@ for @conditionLimitExceeded@
conditionLimitExceededSelector :: Selector '[] Bool
conditionLimitExceededSelector = mkSelector "conditionLimitExceeded"

-- | @Selector@ for @persistenceUnavailable@
persistenceUnavailableSelector :: Selector '[] Bool
persistenceUnavailableSelector = mkSelector "persistenceUnavailable"

-- | @Selector@ for @serviceSessionRequired@
serviceSessionRequiredSelector :: Selector '[] Bool
serviceSessionRequiredSelector = mkSelector "serviceSessionRequired"

-- | @Selector@ for @authorizationRequestInProgress@
authorizationRequestInProgressSelector :: Selector '[] Bool
authorizationRequestInProgressSelector = mkSelector "authorizationRequestInProgress"

