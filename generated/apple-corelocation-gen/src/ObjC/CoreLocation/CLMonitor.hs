{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLMonitor@.
module ObjC.CoreLocation.CLMonitor
  ( CLMonitor
  , IsCLMonitor(..)
  , requestMonitorWithConfiguration_completion
  , addConditionForMonitoring_identifier
  , addConditionForMonitoring_identifier_assumedState
  , removeConditionFromMonitoringWithIdentifier
  , monitoringRecordForIdentifier
  , init_
  , new
  , name
  , monitoredIdentifiers
  , addConditionForMonitoring_identifierSelector
  , addConditionForMonitoring_identifier_assumedStateSelector
  , initSelector
  , monitoredIdentifiersSelector
  , monitoringRecordForIdentifierSelector
  , nameSelector
  , newSelector
  , removeConditionFromMonitoringWithIdentifierSelector
  , requestMonitorWithConfiguration_completionSelector

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

-- | @+ requestMonitorWithConfiguration:completion:@
requestMonitorWithConfiguration_completion :: IsCLMonitorConfiguration config => config -> Ptr () -> IO ()
requestMonitorWithConfiguration_completion config completionHandler =
  do
    cls' <- getRequiredClass "CLMonitor"
    sendClassMessage cls' requestMonitorWithConfiguration_completionSelector (toCLMonitorConfiguration config) completionHandler

-- | @- addConditionForMonitoring:identifier:@
addConditionForMonitoring_identifier :: (IsCLMonitor clMonitor, IsCLCondition condition, IsNSString identifier) => clMonitor -> condition -> identifier -> IO ()
addConditionForMonitoring_identifier clMonitor condition identifier =
  sendMessage clMonitor addConditionForMonitoring_identifierSelector (toCLCondition condition) (toNSString identifier)

-- | @- addConditionForMonitoring:identifier:assumedState:@
addConditionForMonitoring_identifier_assumedState :: (IsCLMonitor clMonitor, IsCLCondition condition, IsNSString identifier) => clMonitor -> condition -> identifier -> CLMonitoringState -> IO ()
addConditionForMonitoring_identifier_assumedState clMonitor condition identifier state =
  sendMessage clMonitor addConditionForMonitoring_identifier_assumedStateSelector (toCLCondition condition) (toNSString identifier) state

-- | @- removeConditionFromMonitoringWithIdentifier:@
removeConditionFromMonitoringWithIdentifier :: (IsCLMonitor clMonitor, IsNSString identifier) => clMonitor -> identifier -> IO ()
removeConditionFromMonitoringWithIdentifier clMonitor identifier =
  sendMessage clMonitor removeConditionFromMonitoringWithIdentifierSelector (toNSString identifier)

-- | @- monitoringRecordForIdentifier:@
monitoringRecordForIdentifier :: (IsCLMonitor clMonitor, IsNSString identifier) => clMonitor -> identifier -> IO (Id CLMonitoringRecord)
monitoringRecordForIdentifier clMonitor identifier =
  sendMessage clMonitor monitoringRecordForIdentifierSelector (toNSString identifier)

-- | @- init@
init_ :: IsCLMonitor clMonitor => clMonitor -> IO (Id CLMonitor)
init_ clMonitor =
  sendOwnedMessage clMonitor initSelector

-- | @+ new@
new :: IO (Id CLMonitor)
new  =
  do
    cls' <- getRequiredClass "CLMonitor"
    sendOwnedClassMessage cls' newSelector

-- | @- name@
name :: IsCLMonitor clMonitor => clMonitor -> IO (Id NSString)
name clMonitor =
  sendMessage clMonitor nameSelector

-- | @- monitoredIdentifiers@
monitoredIdentifiers :: IsCLMonitor clMonitor => clMonitor -> IO (Id NSArray)
monitoredIdentifiers clMonitor =
  sendMessage clMonitor monitoredIdentifiersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestMonitorWithConfiguration:completion:@
requestMonitorWithConfiguration_completionSelector :: Selector '[Id CLMonitorConfiguration, Ptr ()] ()
requestMonitorWithConfiguration_completionSelector = mkSelector "requestMonitorWithConfiguration:completion:"

-- | @Selector@ for @addConditionForMonitoring:identifier:@
addConditionForMonitoring_identifierSelector :: Selector '[Id CLCondition, Id NSString] ()
addConditionForMonitoring_identifierSelector = mkSelector "addConditionForMonitoring:identifier:"

-- | @Selector@ for @addConditionForMonitoring:identifier:assumedState:@
addConditionForMonitoring_identifier_assumedStateSelector :: Selector '[Id CLCondition, Id NSString, CLMonitoringState] ()
addConditionForMonitoring_identifier_assumedStateSelector = mkSelector "addConditionForMonitoring:identifier:assumedState:"

-- | @Selector@ for @removeConditionFromMonitoringWithIdentifier:@
removeConditionFromMonitoringWithIdentifierSelector :: Selector '[Id NSString] ()
removeConditionFromMonitoringWithIdentifierSelector = mkSelector "removeConditionFromMonitoringWithIdentifier:"

-- | @Selector@ for @monitoringRecordForIdentifier:@
monitoringRecordForIdentifierSelector :: Selector '[Id NSString] (Id CLMonitoringRecord)
monitoringRecordForIdentifierSelector = mkSelector "monitoringRecordForIdentifier:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CLMonitor)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CLMonitor)
newSelector = mkSelector "new"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @monitoredIdentifiers@
monitoredIdentifiersSelector :: Selector '[] (Id NSArray)
monitoredIdentifiersSelector = mkSelector "monitoredIdentifiers"

