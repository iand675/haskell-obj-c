{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CKOperationGroup
--
-- A mechanism for your app to group several operations at the granularity of a user action.
--
-- For example, when building a Calendar application, these things might warrant being their own operation groups:  - an initial fetch of data from the server, consisting of many queries, fetchChanges, and fetch operations  - doing an incremental fetch of data in response to a push notification  - saving several records due to a user saving a calendar event
--
-- You associate @CKOperationGroup@ s with@CKOperation@ s by setting the @CKOperation.group@ property.  Create a new @CKOperationGroup@ instance for each distinct user action.
--
-- Generated bindings for @CKOperationGroup@.
module ObjC.CloudKit.CKOperationGroup
  ( CKOperationGroup
  , IsCKOperationGroup(..)
  , init_
  , initWithCoder
  , operationGroupID
  , defaultConfiguration
  , setDefaultConfiguration
  , name
  , setName
  , quantity
  , setQuantity
  , expectedSendSize
  , setExpectedSendSize
  , expectedReceiveSize
  , setExpectedReceiveSize
  , defaultConfigurationSelector
  , expectedReceiveSizeSelector
  , expectedSendSizeSelector
  , initSelector
  , initWithCoderSelector
  , nameSelector
  , operationGroupIDSelector
  , quantitySelector
  , setDefaultConfigurationSelector
  , setExpectedReceiveSizeSelector
  , setExpectedSendSizeSelector
  , setNameSelector
  , setQuantitySelector

  -- * Enum types
  , CKOperationGroupTransferSize(CKOperationGroupTransferSize)
  , pattern CKOperationGroupTransferSizeUnknown
  , pattern CKOperationGroupTransferSizeKilobytes
  , pattern CKOperationGroupTransferSizeMegabytes
  , pattern CKOperationGroupTransferSizeTensOfMegabytes
  , pattern CKOperationGroupTransferSizeHundredsOfMegabytes
  , pattern CKOperationGroupTransferSizeGigabytes
  , pattern CKOperationGroupTransferSizeTensOfGigabytes
  , pattern CKOperationGroupTransferSizeHundredsOfGigabytes

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.CloudKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKOperationGroup ckOperationGroup => ckOperationGroup -> IO (Id CKOperationGroup)
init_ ckOperationGroup =
  sendOwnedMessage ckOperationGroup initSelector

-- | @- initWithCoder:@
initWithCoder :: (IsCKOperationGroup ckOperationGroup, IsNSCoder aDecoder) => ckOperationGroup -> aDecoder -> IO (Id CKOperationGroup)
initWithCoder ckOperationGroup aDecoder =
  sendOwnedMessage ckOperationGroup initWithCoderSelector (toNSCoder aDecoder)

-- | This is an identifier unique to this @CKOperationGroup@
--
-- This value is chosen by the system, and will be unique to this instance of a @CKOperationGroup.@  This identifier will be sent to Apple's servers, and can be used to identify any server-side logging associated with this operation group.
--
-- ObjC selector: @- operationGroupID@
operationGroupID :: IsCKOperationGroup ckOperationGroup => ckOperationGroup -> IO (Id NSString)
operationGroupID ckOperationGroup =
  sendMessage ckOperationGroup operationGroupIDSelector

-- | This is the default configuration applied to operations in this operation group.
--
-- If an operation associated with this operation group has its own configuration, then any explicitly-set properties in that operation's configuration will override these default configuration values.  See the example in CKOperation.h
--
-- ObjC selector: @- defaultConfiguration@
defaultConfiguration :: IsCKOperationGroup ckOperationGroup => ckOperationGroup -> IO (Id CKOperationConfiguration)
defaultConfiguration ckOperationGroup =
  sendMessage ckOperationGroup defaultConfigurationSelector

-- | This is the default configuration applied to operations in this operation group.
--
-- If an operation associated with this operation group has its own configuration, then any explicitly-set properties in that operation's configuration will override these default configuration values.  See the example in CKOperation.h
--
-- ObjC selector: @- setDefaultConfiguration:@
setDefaultConfiguration :: (IsCKOperationGroup ckOperationGroup, IsCKOperationConfiguration value) => ckOperationGroup -> value -> IO ()
setDefaultConfiguration ckOperationGroup value =
  sendMessage ckOperationGroup setDefaultConfigurationSelector (toCKOperationConfiguration value)

-- | Describes the user action attributed to the operation group.
--
-- @name@ should describe the type of work being done.  Some examples:  "Initial Fetch"  "Incremental Fetch"  "Saving User-Entered Record"  This string will be sent to Apple servers to provide aggregate reporting for @CKOperationGroup@ s and therefore must not include personally identifying data.
--
-- ObjC selector: @- name@
name :: IsCKOperationGroup ckOperationGroup => ckOperationGroup -> IO (Id NSString)
name ckOperationGroup =
  sendMessage ckOperationGroup nameSelector

-- | Describes the user action attributed to the operation group.
--
-- @name@ should describe the type of work being done.  Some examples:  "Initial Fetch"  "Incremental Fetch"  "Saving User-Entered Record"  This string will be sent to Apple servers to provide aggregate reporting for @CKOperationGroup@ s and therefore must not include personally identifying data.
--
-- ObjC selector: @- setName:@
setName :: (IsCKOperationGroup ckOperationGroup, IsNSString value) => ckOperationGroup -> value -> IO ()
setName ckOperationGroup value =
  sendMessage ckOperationGroup setNameSelector (toNSString value)

-- | Describes an application-specific "number of elements" associated with the operation group.
--
-- @quantity@ is intended to show the app-specific count of items contained within the operation group.  It is your job to assign meaning to this value.  For example, if an app created an operation group to save 3 calendar events the user had created, the app might want to set this to "3".  This value is not shown to your users, it's meant to aid your development and debugging.  This value will be reported in the CloudKit Dashboard's log entries for all operations associated with this operation group.
--
-- ObjC selector: @- quantity@
quantity :: IsCKOperationGroup ckOperationGroup => ckOperationGroup -> IO CULong
quantity ckOperationGroup =
  sendMessage ckOperationGroup quantitySelector

-- | Describes an application-specific "number of elements" associated with the operation group.
--
-- @quantity@ is intended to show the app-specific count of items contained within the operation group.  It is your job to assign meaning to this value.  For example, if an app created an operation group to save 3 calendar events the user had created, the app might want to set this to "3".  This value is not shown to your users, it's meant to aid your development and debugging.  This value will be reported in the CloudKit Dashboard's log entries for all operations associated with this operation group.
--
-- ObjC selector: @- setQuantity:@
setQuantity :: IsCKOperationGroup ckOperationGroup => ckOperationGroup -> CULong -> IO ()
setQuantity ckOperationGroup value =
  sendMessage ckOperationGroup setQuantitySelector value

-- | Estimated size of traffic being uploaded to the CloudKit Server
--
-- Inform the system how much data you plan on transferring.  Obviously, these won't be exact.  Be as accurate as possible, but even an order-of-magnitude estimate is better than no value.  The system will consult these values when scheduling discretionary network requests (see the description of @CKOperationConfiguration.qualityOfService).@  Overestimating your workload means that an operation group issuing discretionary network requests may be delayed until network conditions are good.  Underestimating your workload may cause the system to oversaturate a constrained connection, leading to network failures.  You may update after the @CKOperationGroup@ is created.  If it is increased, then subsequent @CKOperation@ s associated with this operation group may be delayed until network conditions are good.  Defaults to @CKOperationGroupTransferSizeUnknown@
--
-- ObjC selector: @- expectedSendSize@
expectedSendSize :: IsCKOperationGroup ckOperationGroup => ckOperationGroup -> IO CKOperationGroupTransferSize
expectedSendSize ckOperationGroup =
  sendMessage ckOperationGroup expectedSendSizeSelector

-- | Estimated size of traffic being uploaded to the CloudKit Server
--
-- Inform the system how much data you plan on transferring.  Obviously, these won't be exact.  Be as accurate as possible, but even an order-of-magnitude estimate is better than no value.  The system will consult these values when scheduling discretionary network requests (see the description of @CKOperationConfiguration.qualityOfService).@  Overestimating your workload means that an operation group issuing discretionary network requests may be delayed until network conditions are good.  Underestimating your workload may cause the system to oversaturate a constrained connection, leading to network failures.  You may update after the @CKOperationGroup@ is created.  If it is increased, then subsequent @CKOperation@ s associated with this operation group may be delayed until network conditions are good.  Defaults to @CKOperationGroupTransferSizeUnknown@
--
-- ObjC selector: @- setExpectedSendSize:@
setExpectedSendSize :: IsCKOperationGroup ckOperationGroup => ckOperationGroup -> CKOperationGroupTransferSize -> IO ()
setExpectedSendSize ckOperationGroup value =
  sendMessage ckOperationGroup setExpectedSendSizeSelector value

-- | Estimated size of traffic being downloaded from the CloudKit Server
--
-- Inform the system how much data you plan on transferring.  Obviously, these won't be exact.  Be as accurate as possible, but even an order-of-magnitude estimate is better than no value.  The system will consult these values when scheduling discretionary network requests (see the description of @CKOperationConfiguration.qualityOfService).@  Overestimating your workload means that an operation group issuing discretionary network requests may be delayed until network conditions are good.  Underestimating your workload may cause the system to oversaturate a constrained connection, leading to network failures.  You may update after the @CKOperationGroup@ is created.  If it is increased, then subsequent @CKOperation@ s associated with this operation group may be delayed until network conditions are good.  Defaults to @CKOperationGroupTransferSizeUnknown@
--
-- ObjC selector: @- expectedReceiveSize@
expectedReceiveSize :: IsCKOperationGroup ckOperationGroup => ckOperationGroup -> IO CKOperationGroupTransferSize
expectedReceiveSize ckOperationGroup =
  sendMessage ckOperationGroup expectedReceiveSizeSelector

-- | Estimated size of traffic being downloaded from the CloudKit Server
--
-- Inform the system how much data you plan on transferring.  Obviously, these won't be exact.  Be as accurate as possible, but even an order-of-magnitude estimate is better than no value.  The system will consult these values when scheduling discretionary network requests (see the description of @CKOperationConfiguration.qualityOfService).@  Overestimating your workload means that an operation group issuing discretionary network requests may be delayed until network conditions are good.  Underestimating your workload may cause the system to oversaturate a constrained connection, leading to network failures.  You may update after the @CKOperationGroup@ is created.  If it is increased, then subsequent @CKOperation@ s associated with this operation group may be delayed until network conditions are good.  Defaults to @CKOperationGroupTransferSizeUnknown@
--
-- ObjC selector: @- setExpectedReceiveSize:@
setExpectedReceiveSize :: IsCKOperationGroup ckOperationGroup => ckOperationGroup -> CKOperationGroupTransferSize -> IO ()
setExpectedReceiveSize ckOperationGroup value =
  sendMessage ckOperationGroup setExpectedReceiveSizeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKOperationGroup)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id CKOperationGroup)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @operationGroupID@
operationGroupIDSelector :: Selector '[] (Id NSString)
operationGroupIDSelector = mkSelector "operationGroupID"

-- | @Selector@ for @defaultConfiguration@
defaultConfigurationSelector :: Selector '[] (Id CKOperationConfiguration)
defaultConfigurationSelector = mkSelector "defaultConfiguration"

-- | @Selector@ for @setDefaultConfiguration:@
setDefaultConfigurationSelector :: Selector '[Id CKOperationConfiguration] ()
setDefaultConfigurationSelector = mkSelector "setDefaultConfiguration:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @quantity@
quantitySelector :: Selector '[] CULong
quantitySelector = mkSelector "quantity"

-- | @Selector@ for @setQuantity:@
setQuantitySelector :: Selector '[CULong] ()
setQuantitySelector = mkSelector "setQuantity:"

-- | @Selector@ for @expectedSendSize@
expectedSendSizeSelector :: Selector '[] CKOperationGroupTransferSize
expectedSendSizeSelector = mkSelector "expectedSendSize"

-- | @Selector@ for @setExpectedSendSize:@
setExpectedSendSizeSelector :: Selector '[CKOperationGroupTransferSize] ()
setExpectedSendSizeSelector = mkSelector "setExpectedSendSize:"

-- | @Selector@ for @expectedReceiveSize@
expectedReceiveSizeSelector :: Selector '[] CKOperationGroupTransferSize
expectedReceiveSizeSelector = mkSelector "expectedReceiveSize"

-- | @Selector@ for @setExpectedReceiveSize:@
setExpectedReceiveSizeSelector :: Selector '[CKOperationGroupTransferSize] ()
setExpectedReceiveSizeSelector = mkSelector "setExpectedReceiveSize:"

