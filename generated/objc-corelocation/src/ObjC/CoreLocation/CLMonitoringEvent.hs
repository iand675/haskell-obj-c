{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , newSelector
  , identifierSelector
  , refinementSelector
  , stateSelector
  , dateSelector
  , authorizationDeniedSelector
  , authorizationDeniedGloballySelector
  , authorizationRestrictedSelector
  , insufficientlyInUseSelector
  , accuracyLimitedSelector
  , conditionUnsupportedSelector
  , conditionLimitExceededSelector
  , persistenceUnavailableSelector
  , serviceSessionRequiredSelector
  , authorizationRequestInProgressSelector

  -- * Enum types
  , CLMonitoringState(CLMonitoringState)
  , pattern CLMonitoringStateUnknown
  , pattern CLMonitoringStateSatisfied
  , pattern CLMonitoringStateUnsatisfied
  , pattern CLMonitoringStateUnmonitored

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

import ObjC.CoreLocation.Internal.Classes
import ObjC.CoreLocation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO (Id CLMonitoringEvent)
init_ clMonitoringEvent  =
  sendMsg clMonitoringEvent (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CLMonitoringEvent)
new  =
  do
    cls' <- getRequiredClass "CLMonitoringEvent"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- identifier@
identifier :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO (Id NSString)
identifier clMonitoringEvent  =
  sendMsg clMonitoringEvent (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- refinement@
refinement :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO (Id CLCondition)
refinement clMonitoringEvent  =
  sendMsg clMonitoringEvent (mkSelector "refinement") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- state@
state :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO CLMonitoringState
state clMonitoringEvent  =
  fmap (coerce :: CULong -> CLMonitoringState) $ sendMsg clMonitoringEvent (mkSelector "state") retCULong []

-- | @- date@
date :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO (Id NSDate)
date clMonitoringEvent  =
  sendMsg clMonitoringEvent (mkSelector "date") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- authorizationDenied@
authorizationDenied :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO Bool
authorizationDenied clMonitoringEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clMonitoringEvent (mkSelector "authorizationDenied") retCULong []

-- | @- authorizationDeniedGlobally@
authorizationDeniedGlobally :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO Bool
authorizationDeniedGlobally clMonitoringEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clMonitoringEvent (mkSelector "authorizationDeniedGlobally") retCULong []

-- | @- authorizationRestricted@
authorizationRestricted :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO Bool
authorizationRestricted clMonitoringEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clMonitoringEvent (mkSelector "authorizationRestricted") retCULong []

-- | @- insufficientlyInUse@
insufficientlyInUse :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO Bool
insufficientlyInUse clMonitoringEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clMonitoringEvent (mkSelector "insufficientlyInUse") retCULong []

-- | @- accuracyLimited@
accuracyLimited :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO Bool
accuracyLimited clMonitoringEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clMonitoringEvent (mkSelector "accuracyLimited") retCULong []

-- | @- conditionUnsupported@
conditionUnsupported :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO Bool
conditionUnsupported clMonitoringEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clMonitoringEvent (mkSelector "conditionUnsupported") retCULong []

-- | @- conditionLimitExceeded@
conditionLimitExceeded :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO Bool
conditionLimitExceeded clMonitoringEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clMonitoringEvent (mkSelector "conditionLimitExceeded") retCULong []

-- | @- persistenceUnavailable@
persistenceUnavailable :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO Bool
persistenceUnavailable clMonitoringEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clMonitoringEvent (mkSelector "persistenceUnavailable") retCULong []

-- | @- serviceSessionRequired@
serviceSessionRequired :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO Bool
serviceSessionRequired clMonitoringEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clMonitoringEvent (mkSelector "serviceSessionRequired") retCULong []

-- | @- authorizationRequestInProgress@
authorizationRequestInProgress :: IsCLMonitoringEvent clMonitoringEvent => clMonitoringEvent -> IO Bool
authorizationRequestInProgress clMonitoringEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg clMonitoringEvent (mkSelector "authorizationRequestInProgress") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @refinement@
refinementSelector :: Selector
refinementSelector = mkSelector "refinement"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @date@
dateSelector :: Selector
dateSelector = mkSelector "date"

-- | @Selector@ for @authorizationDenied@
authorizationDeniedSelector :: Selector
authorizationDeniedSelector = mkSelector "authorizationDenied"

-- | @Selector@ for @authorizationDeniedGlobally@
authorizationDeniedGloballySelector :: Selector
authorizationDeniedGloballySelector = mkSelector "authorizationDeniedGlobally"

-- | @Selector@ for @authorizationRestricted@
authorizationRestrictedSelector :: Selector
authorizationRestrictedSelector = mkSelector "authorizationRestricted"

-- | @Selector@ for @insufficientlyInUse@
insufficientlyInUseSelector :: Selector
insufficientlyInUseSelector = mkSelector "insufficientlyInUse"

-- | @Selector@ for @accuracyLimited@
accuracyLimitedSelector :: Selector
accuracyLimitedSelector = mkSelector "accuracyLimited"

-- | @Selector@ for @conditionUnsupported@
conditionUnsupportedSelector :: Selector
conditionUnsupportedSelector = mkSelector "conditionUnsupported"

-- | @Selector@ for @conditionLimitExceeded@
conditionLimitExceededSelector :: Selector
conditionLimitExceededSelector = mkSelector "conditionLimitExceeded"

-- | @Selector@ for @persistenceUnavailable@
persistenceUnavailableSelector :: Selector
persistenceUnavailableSelector = mkSelector "persistenceUnavailable"

-- | @Selector@ for @serviceSessionRequired@
serviceSessionRequiredSelector :: Selector
serviceSessionRequiredSelector = mkSelector "serviceSessionRequired"

-- | @Selector@ for @authorizationRequestInProgress@
authorizationRequestInProgressSelector :: Selector
authorizationRequestInProgressSelector = mkSelector "authorizationRequestInProgress"

