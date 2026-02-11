{-# LANGUAGE PatternSynonyms #-}
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
  , requestMonitorWithConfiguration_completionSelector
  , addConditionForMonitoring_identifierSelector
  , addConditionForMonitoring_identifier_assumedStateSelector
  , removeConditionFromMonitoringWithIdentifierSelector
  , monitoringRecordForIdentifierSelector
  , initSelector
  , newSelector
  , nameSelector
  , monitoredIdentifiersSelector

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

-- | @+ requestMonitorWithConfiguration:completion:@
requestMonitorWithConfiguration_completion :: IsCLMonitorConfiguration config => config -> Ptr () -> IO ()
requestMonitorWithConfiguration_completion config completionHandler =
  do
    cls' <- getRequiredClass "CLMonitor"
    withObjCPtr config $ \raw_config ->
      sendClassMsg cls' (mkSelector "requestMonitorWithConfiguration:completion:") retVoid [argPtr (castPtr raw_config :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- addConditionForMonitoring:identifier:@
addConditionForMonitoring_identifier :: (IsCLMonitor clMonitor, IsCLCondition condition, IsNSString identifier) => clMonitor -> condition -> identifier -> IO ()
addConditionForMonitoring_identifier clMonitor  condition identifier =
withObjCPtr condition $ \raw_condition ->
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg clMonitor (mkSelector "addConditionForMonitoring:identifier:") retVoid [argPtr (castPtr raw_condition :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())]

-- | @- addConditionForMonitoring:identifier:assumedState:@
addConditionForMonitoring_identifier_assumedState :: (IsCLMonitor clMonitor, IsCLCondition condition, IsNSString identifier) => clMonitor -> condition -> identifier -> CLMonitoringState -> IO ()
addConditionForMonitoring_identifier_assumedState clMonitor  condition identifier state =
withObjCPtr condition $ \raw_condition ->
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg clMonitor (mkSelector "addConditionForMonitoring:identifier:assumedState:") retVoid [argPtr (castPtr raw_condition :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ()), argCULong (coerce state)]

-- | @- removeConditionFromMonitoringWithIdentifier:@
removeConditionFromMonitoringWithIdentifier :: (IsCLMonitor clMonitor, IsNSString identifier) => clMonitor -> identifier -> IO ()
removeConditionFromMonitoringWithIdentifier clMonitor  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg clMonitor (mkSelector "removeConditionFromMonitoringWithIdentifier:") retVoid [argPtr (castPtr raw_identifier :: Ptr ())]

-- | @- monitoringRecordForIdentifier:@
monitoringRecordForIdentifier :: (IsCLMonitor clMonitor, IsNSString identifier) => clMonitor -> identifier -> IO (Id CLMonitoringRecord)
monitoringRecordForIdentifier clMonitor  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg clMonitor (mkSelector "monitoringRecordForIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsCLMonitor clMonitor => clMonitor -> IO (Id CLMonitor)
init_ clMonitor  =
  sendMsg clMonitor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id CLMonitor)
new  =
  do
    cls' <- getRequiredClass "CLMonitor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- name@
name :: IsCLMonitor clMonitor => clMonitor -> IO (Id NSString)
name clMonitor  =
  sendMsg clMonitor (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- monitoredIdentifiers@
monitoredIdentifiers :: IsCLMonitor clMonitor => clMonitor -> IO (Id NSArray)
monitoredIdentifiers clMonitor  =
  sendMsg clMonitor (mkSelector "monitoredIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestMonitorWithConfiguration:completion:@
requestMonitorWithConfiguration_completionSelector :: Selector
requestMonitorWithConfiguration_completionSelector = mkSelector "requestMonitorWithConfiguration:completion:"

-- | @Selector@ for @addConditionForMonitoring:identifier:@
addConditionForMonitoring_identifierSelector :: Selector
addConditionForMonitoring_identifierSelector = mkSelector "addConditionForMonitoring:identifier:"

-- | @Selector@ for @addConditionForMonitoring:identifier:assumedState:@
addConditionForMonitoring_identifier_assumedStateSelector :: Selector
addConditionForMonitoring_identifier_assumedStateSelector = mkSelector "addConditionForMonitoring:identifier:assumedState:"

-- | @Selector@ for @removeConditionFromMonitoringWithIdentifier:@
removeConditionFromMonitoringWithIdentifierSelector :: Selector
removeConditionFromMonitoringWithIdentifierSelector = mkSelector "removeConditionFromMonitoringWithIdentifier:"

-- | @Selector@ for @monitoringRecordForIdentifier:@
monitoringRecordForIdentifierSelector :: Selector
monitoringRecordForIdentifierSelector = mkSelector "monitoringRecordForIdentifier:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @monitoredIdentifiers@
monitoredIdentifiersSelector :: Selector
monitoredIdentifiersSelector = mkSelector "monitoredIdentifiers"

