{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CKOperationConfiguration
--
-- An operation configuration is a set of properties that describes how your operation should behave.  All properties have a default value.  When determining what properties to apply to an operation, we consult the operation's configuration property, as well as the operation->group->defaultConfiguration property.  We combine them following these rules:
--
-- Group Default Configuration Value | Operation Configuration Value |        Value Applied To Operation
-- -----------------------------------+-------------------------------+-----------------------------------------
-- default value           |         default value         |                  default value
-- default value           |         explicit value        |       operation.configuration explicit value
-- explicit value          |         default value         | operation.group.defaultConfiguration explicit value
-- explicit value          |         explicit value        |       operation.configuration explicit value
--
-- For example:  CKOperationGroup -> defaultConfiguration -> allowsCellularAccess explicitly set to NO  + CKOperation -> configuration -> allowsCellularAccess has default value of YES  = disallow cellular access
--
-- CKOperationGroup -> defaultConfiguration -> allowsCellularAccess explicitly set to NO  + CKOperation -> configuration -> allowsCellularAccess explicitly set to YES  = allow cellular access
--
-- Generated bindings for @CKOperationConfiguration@.
module ObjC.CloudKit.CKOperationConfiguration
  ( CKOperationConfiguration
  , IsCKOperationConfiguration(..)
  , container
  , setContainer
  , qualityOfService
  , setQualityOfService
  , allowsCellularAccess
  , setAllowsCellularAccess
  , longLived
  , setLongLived
  , timeoutIntervalForRequest
  , setTimeoutIntervalForRequest
  , timeoutIntervalForResource
  , setTimeoutIntervalForResource
  , allowsCellularAccessSelector
  , containerSelector
  , longLivedSelector
  , qualityOfServiceSelector
  , setAllowsCellularAccessSelector
  , setContainerSelector
  , setLongLivedSelector
  , setQualityOfServiceSelector
  , setTimeoutIntervalForRequestSelector
  , setTimeoutIntervalForResourceSelector
  , timeoutIntervalForRequestSelector
  , timeoutIntervalForResourceSelector

  -- * Enum types
  , NSQualityOfService(NSQualityOfService)
  , pattern NSQualityOfServiceUserInteractive
  , pattern NSQualityOfServiceUserInitiated
  , pattern NSQualityOfServiceUtility
  , pattern NSQualityOfServiceBackground
  , pattern NSQualityOfServiceDefault

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | If no container is set, [CKContainer defaultContainer] is used
--
-- ObjC selector: @- container@
container :: IsCKOperationConfiguration ckOperationConfiguration => ckOperationConfiguration -> IO (Id CKContainer)
container ckOperationConfiguration =
  sendMessage ckOperationConfiguration containerSelector

-- | If no container is set, [CKContainer defaultContainer] is used
--
-- ObjC selector: @- setContainer:@
setContainer :: (IsCKOperationConfiguration ckOperationConfiguration, IsCKContainer value) => ckOperationConfiguration -> value -> IO ()
setContainer ckOperationConfiguration value =
  sendMessage ckOperationConfiguration setContainerSelector (toCKContainer value)

-- | CKOperations behave differently depending on how you set qualityOfService.
--
-- Quality of Service | timeoutIntervalForResource | Network Error Behavior | Discretionary Behavior
-- -------------------+----------------------------+------------------------+-----------------------
-- UserInteractive    | -1 (no enforcement)        | fail                   | nonDiscretionary
-- UserInitiated      | -1 (no enforcement)        | fail                   | nonDiscretionary
-- Default            | 1 week                     | fail                   | discretionary when app backgrounded
-- Utility            | 1 week                     | internally retried     | discretionary when app backgrounded
-- Background         | 1 week                     | internally retried     | discretionary
--
-- timeoutIntervalForResource - the timeout interval for any network resources retrieved by this operation - this can be overridden via CKOperationConfiguration's timeoutIntervalForResource property
--
-- Network Error Behavior - when a network request in service of a CKOperation fails due to a networking error, the operation may fail with that error, or internally retry the network request.  Only a subset of networking errors are retried, and limiting factors such as timeoutIntervalForResource are still applicable.
--
-- Discretionary Behavior - network requests in service of a CKOperation may be marked as discretionary - discretionary network requests are scheduled at the description of the system for optimal performance
--
-- CKOperations have a default qualityOfService of Default.
--
-- ObjC selector: @- qualityOfService@
qualityOfService :: IsCKOperationConfiguration ckOperationConfiguration => ckOperationConfiguration -> IO NSQualityOfService
qualityOfService ckOperationConfiguration =
  sendMessage ckOperationConfiguration qualityOfServiceSelector

-- | CKOperations behave differently depending on how you set qualityOfService.
--
-- Quality of Service | timeoutIntervalForResource | Network Error Behavior | Discretionary Behavior
-- -------------------+----------------------------+------------------------+-----------------------
-- UserInteractive    | -1 (no enforcement)        | fail                   | nonDiscretionary
-- UserInitiated      | -1 (no enforcement)        | fail                   | nonDiscretionary
-- Default            | 1 week                     | fail                   | discretionary when app backgrounded
-- Utility            | 1 week                     | internally retried     | discretionary when app backgrounded
-- Background         | 1 week                     | internally retried     | discretionary
--
-- timeoutIntervalForResource - the timeout interval for any network resources retrieved by this operation - this can be overridden via CKOperationConfiguration's timeoutIntervalForResource property
--
-- Network Error Behavior - when a network request in service of a CKOperation fails due to a networking error, the operation may fail with that error, or internally retry the network request.  Only a subset of networking errors are retried, and limiting factors such as timeoutIntervalForResource are still applicable.
--
-- Discretionary Behavior - network requests in service of a CKOperation may be marked as discretionary - discretionary network requests are scheduled at the description of the system for optimal performance
--
-- CKOperations have a default qualityOfService of Default.
--
-- ObjC selector: @- setQualityOfService:@
setQualityOfService :: IsCKOperationConfiguration ckOperationConfiguration => ckOperationConfiguration -> NSQualityOfService -> IO ()
setQualityOfService ckOperationConfiguration value =
  sendMessage ckOperationConfiguration setQualityOfServiceSelector value

-- | Defaults to @YES@
--
-- ObjC selector: @- allowsCellularAccess@
allowsCellularAccess :: IsCKOperationConfiguration ckOperationConfiguration => ckOperationConfiguration -> IO Bool
allowsCellularAccess ckOperationConfiguration =
  sendMessage ckOperationConfiguration allowsCellularAccessSelector

-- | Defaults to @YES@
--
-- ObjC selector: @- setAllowsCellularAccess:@
setAllowsCellularAccess :: IsCKOperationConfiguration ckOperationConfiguration => ckOperationConfiguration -> Bool -> IO ()
setAllowsCellularAccess ckOperationConfiguration value =
  sendMessage ckOperationConfiguration setAllowsCellularAccessSelector value

-- | Long lived operations will continue running even if your process exits. If your process remains alive for the lifetime of the long lived operation its behavior is the same as a regular operation.
--
-- Long lived operations can be fetched and replayed from the container via the @fetchAllLongLivedOperations:@ and @fetchLongLivedOperationsWithIDs:@ APIs. Your code should only fetch and re-enqueue long lived operations on app launch.
--
-- Long lived operations persist until their -[NSOperation completionBlock] returns or until the operation is cancelled.  Long lived operations may be garbage collected 24 hours after they finish running if no client has replayed them.
--
-- The default value for longLived is NO. Changing the value of longLived on an already started operation or on an outstanding long lived operation fetched from CKContainer has no effect.
--
-- ObjC selector: @- longLived@
longLived :: IsCKOperationConfiguration ckOperationConfiguration => ckOperationConfiguration -> IO Bool
longLived ckOperationConfiguration =
  sendMessage ckOperationConfiguration longLivedSelector

-- | Long lived operations will continue running even if your process exits. If your process remains alive for the lifetime of the long lived operation its behavior is the same as a regular operation.
--
-- Long lived operations can be fetched and replayed from the container via the @fetchAllLongLivedOperations:@ and @fetchLongLivedOperationsWithIDs:@ APIs. Your code should only fetch and re-enqueue long lived operations on app launch.
--
-- Long lived operations persist until their -[NSOperation completionBlock] returns or until the operation is cancelled.  Long lived operations may be garbage collected 24 hours after they finish running if no client has replayed them.
--
-- The default value for longLived is NO. Changing the value of longLived on an already started operation or on an outstanding long lived operation fetched from CKContainer has no effect.
--
-- ObjC selector: @- setLongLived:@
setLongLived :: IsCKOperationConfiguration ckOperationConfiguration => ckOperationConfiguration -> Bool -> IO ()
setLongLived ckOperationConfiguration value =
  sendMessage ckOperationConfiguration setLongLivedSelector value

-- | If non-zero, overrides the timeout interval for any network requests issued by this operation.  The default value is 60.
--
-- See: NSURLSessionConfiguration.timeoutIntervalForRequest
--
-- ObjC selector: @- timeoutIntervalForRequest@
timeoutIntervalForRequest :: IsCKOperationConfiguration ckOperationConfiguration => ckOperationConfiguration -> IO CDouble
timeoutIntervalForRequest ckOperationConfiguration =
  sendMessage ckOperationConfiguration timeoutIntervalForRequestSelector

-- | If non-zero, overrides the timeout interval for any network requests issued by this operation.  The default value is 60.
--
-- See: NSURLSessionConfiguration.timeoutIntervalForRequest
--
-- ObjC selector: @- setTimeoutIntervalForRequest:@
setTimeoutIntervalForRequest :: IsCKOperationConfiguration ckOperationConfiguration => ckOperationConfiguration -> CDouble -> IO ()
setTimeoutIntervalForRequest ckOperationConfiguration value =
  sendMessage ckOperationConfiguration setTimeoutIntervalForRequestSelector value

-- | If set, overrides the timeout interval for any network resources retrieved by this operation.  If not explicitly set, defaults to a value based on the operation's @qualityOfService@
--
-- See: NSURLSessionConfiguration.timeoutIntervalForResource
--
-- ObjC selector: @- timeoutIntervalForResource@
timeoutIntervalForResource :: IsCKOperationConfiguration ckOperationConfiguration => ckOperationConfiguration -> IO CDouble
timeoutIntervalForResource ckOperationConfiguration =
  sendMessage ckOperationConfiguration timeoutIntervalForResourceSelector

-- | If set, overrides the timeout interval for any network resources retrieved by this operation.  If not explicitly set, defaults to a value based on the operation's @qualityOfService@
--
-- See: NSURLSessionConfiguration.timeoutIntervalForResource
--
-- ObjC selector: @- setTimeoutIntervalForResource:@
setTimeoutIntervalForResource :: IsCKOperationConfiguration ckOperationConfiguration => ckOperationConfiguration -> CDouble -> IO ()
setTimeoutIntervalForResource ckOperationConfiguration value =
  sendMessage ckOperationConfiguration setTimeoutIntervalForResourceSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @container@
containerSelector :: Selector '[] (Id CKContainer)
containerSelector = mkSelector "container"

-- | @Selector@ for @setContainer:@
setContainerSelector :: Selector '[Id CKContainer] ()
setContainerSelector = mkSelector "setContainer:"

-- | @Selector@ for @qualityOfService@
qualityOfServiceSelector :: Selector '[] NSQualityOfService
qualityOfServiceSelector = mkSelector "qualityOfService"

-- | @Selector@ for @setQualityOfService:@
setQualityOfServiceSelector :: Selector '[NSQualityOfService] ()
setQualityOfServiceSelector = mkSelector "setQualityOfService:"

-- | @Selector@ for @allowsCellularAccess@
allowsCellularAccessSelector :: Selector '[] Bool
allowsCellularAccessSelector = mkSelector "allowsCellularAccess"

-- | @Selector@ for @setAllowsCellularAccess:@
setAllowsCellularAccessSelector :: Selector '[Bool] ()
setAllowsCellularAccessSelector = mkSelector "setAllowsCellularAccess:"

-- | @Selector@ for @longLived@
longLivedSelector :: Selector '[] Bool
longLivedSelector = mkSelector "longLived"

-- | @Selector@ for @setLongLived:@
setLongLivedSelector :: Selector '[Bool] ()
setLongLivedSelector = mkSelector "setLongLived:"

-- | @Selector@ for @timeoutIntervalForRequest@
timeoutIntervalForRequestSelector :: Selector '[] CDouble
timeoutIntervalForRequestSelector = mkSelector "timeoutIntervalForRequest"

-- | @Selector@ for @setTimeoutIntervalForRequest:@
setTimeoutIntervalForRequestSelector :: Selector '[CDouble] ()
setTimeoutIntervalForRequestSelector = mkSelector "setTimeoutIntervalForRequest:"

-- | @Selector@ for @timeoutIntervalForResource@
timeoutIntervalForResourceSelector :: Selector '[] CDouble
timeoutIntervalForResourceSelector = mkSelector "timeoutIntervalForResource"

-- | @Selector@ for @setTimeoutIntervalForResource:@
setTimeoutIntervalForResourceSelector :: Selector '[CDouble] ()
setTimeoutIntervalForResourceSelector = mkSelector "setTimeoutIntervalForResource:"

