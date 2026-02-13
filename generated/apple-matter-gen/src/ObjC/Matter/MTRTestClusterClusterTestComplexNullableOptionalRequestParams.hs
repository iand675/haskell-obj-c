{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestComplexNullableOptionalRequestParams@.
module ObjC.Matter.MTRTestClusterClusterTestComplexNullableOptionalRequestParams
  ( MTRTestClusterClusterTestComplexNullableOptionalRequestParams
  , IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams(..)
  , nullableInt
  , setNullableInt
  , optionalInt
  , setOptionalInt
  , nullableOptionalInt
  , setNullableOptionalInt
  , nullableString
  , setNullableString
  , optionalString
  , setOptionalString
  , nullableOptionalString
  , setNullableOptionalString
  , nullableStruct
  , setNullableStruct
  , optionalStruct
  , setOptionalStruct
  , nullableOptionalStruct
  , setNullableOptionalStruct
  , nullableList
  , setNullableList
  , optionalList
  , setOptionalList
  , nullableOptionalList
  , setNullableOptionalList
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , nullableIntSelector
  , nullableListSelector
  , nullableOptionalIntSelector
  , nullableOptionalListSelector
  , nullableOptionalStringSelector
  , nullableOptionalStructSelector
  , nullableStringSelector
  , nullableStructSelector
  , optionalIntSelector
  , optionalListSelector
  , optionalStringSelector
  , optionalStructSelector
  , serverSideProcessingTimeoutSelector
  , setNullableIntSelector
  , setNullableListSelector
  , setNullableOptionalIntSelector
  , setNullableOptionalListSelector
  , setNullableOptionalStringSelector
  , setNullableOptionalStructSelector
  , setNullableStringSelector
  , setNullableStructSelector
  , setOptionalIntSelector
  , setOptionalListSelector
  , setOptionalStringSelector
  , setOptionalStructSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , timedInvokeTimeoutMsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- nullableInt@
nullableInt :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id NSNumber)
nullableInt mtrTestClusterClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams nullableIntSelector

-- | @- setNullableInt:@
setNullableInt :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableInt mtrTestClusterClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams setNullableIntSelector (toNSNumber value)

-- | @- optionalInt@
optionalInt :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id NSNumber)
optionalInt mtrTestClusterClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams optionalIntSelector

-- | @- setOptionalInt:@
setOptionalInt :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setOptionalInt mtrTestClusterClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams setOptionalIntSelector (toNSNumber value)

-- | @- nullableOptionalInt@
nullableOptionalInt :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id NSNumber)
nullableOptionalInt mtrTestClusterClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams nullableOptionalIntSelector

-- | @- setNullableOptionalInt:@
setNullableOptionalInt :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableOptionalInt mtrTestClusterClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams setNullableOptionalIntSelector (toNSNumber value)

-- | @- nullableString@
nullableString :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id NSString)
nullableString mtrTestClusterClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams nullableStringSelector

-- | @- setNullableString:@
setNullableString :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsNSString value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableString mtrTestClusterClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams setNullableStringSelector (toNSString value)

-- | @- optionalString@
optionalString :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id NSString)
optionalString mtrTestClusterClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams optionalStringSelector

-- | @- setOptionalString:@
setOptionalString :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsNSString value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setOptionalString mtrTestClusterClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams setOptionalStringSelector (toNSString value)

-- | @- nullableOptionalString@
nullableOptionalString :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id NSString)
nullableOptionalString mtrTestClusterClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams nullableOptionalStringSelector

-- | @- setNullableOptionalString:@
setNullableOptionalString :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsNSString value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableOptionalString mtrTestClusterClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams setNullableOptionalStringSelector (toNSString value)

-- | @- nullableStruct@
nullableStruct :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
nullableStruct mtrTestClusterClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams nullableStructSelector

-- | @- setNullableStruct:@
setNullableStruct :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableStruct mtrTestClusterClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams setNullableStructSelector (toMTRUnitTestingClusterSimpleStruct value)

-- | @- optionalStruct@
optionalStruct :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
optionalStruct mtrTestClusterClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams optionalStructSelector

-- | @- setOptionalStruct:@
setOptionalStruct :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setOptionalStruct mtrTestClusterClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams setOptionalStructSelector (toMTRUnitTestingClusterSimpleStruct value)

-- | @- nullableOptionalStruct@
nullableOptionalStruct :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
nullableOptionalStruct mtrTestClusterClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams nullableOptionalStructSelector

-- | @- setNullableOptionalStruct:@
setNullableOptionalStruct :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableOptionalStruct mtrTestClusterClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams setNullableOptionalStructSelector (toMTRUnitTestingClusterSimpleStruct value)

-- | @- nullableList@
nullableList :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id NSArray)
nullableList mtrTestClusterClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams nullableListSelector

-- | @- setNullableList:@
setNullableList :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsNSArray value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableList mtrTestClusterClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams setNullableListSelector (toNSArray value)

-- | @- optionalList@
optionalList :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id NSArray)
optionalList mtrTestClusterClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams optionalListSelector

-- | @- setOptionalList:@
setOptionalList :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsNSArray value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setOptionalList mtrTestClusterClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams setOptionalListSelector (toNSArray value)

-- | @- nullableOptionalList@
nullableOptionalList :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id NSArray)
nullableOptionalList mtrTestClusterClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams nullableOptionalListSelector

-- | @- setNullableOptionalList:@
setNullableOptionalList :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsNSArray value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableOptionalList mtrTestClusterClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams setNullableOptionalListSelector (toNSArray value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTestClusterClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTestClusterClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrTestClusterClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTestClusterClusterTestComplexNullableOptionalRequestParams mtrTestClusterClusterTestComplexNullableOptionalRequestParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrTestClusterClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nullableInt@
nullableIntSelector :: Selector '[] (Id NSNumber)
nullableIntSelector = mkSelector "nullableInt"

-- | @Selector@ for @setNullableInt:@
setNullableIntSelector :: Selector '[Id NSNumber] ()
setNullableIntSelector = mkSelector "setNullableInt:"

-- | @Selector@ for @optionalInt@
optionalIntSelector :: Selector '[] (Id NSNumber)
optionalIntSelector = mkSelector "optionalInt"

-- | @Selector@ for @setOptionalInt:@
setOptionalIntSelector :: Selector '[Id NSNumber] ()
setOptionalIntSelector = mkSelector "setOptionalInt:"

-- | @Selector@ for @nullableOptionalInt@
nullableOptionalIntSelector :: Selector '[] (Id NSNumber)
nullableOptionalIntSelector = mkSelector "nullableOptionalInt"

-- | @Selector@ for @setNullableOptionalInt:@
setNullableOptionalIntSelector :: Selector '[Id NSNumber] ()
setNullableOptionalIntSelector = mkSelector "setNullableOptionalInt:"

-- | @Selector@ for @nullableString@
nullableStringSelector :: Selector '[] (Id NSString)
nullableStringSelector = mkSelector "nullableString"

-- | @Selector@ for @setNullableString:@
setNullableStringSelector :: Selector '[Id NSString] ()
setNullableStringSelector = mkSelector "setNullableString:"

-- | @Selector@ for @optionalString@
optionalStringSelector :: Selector '[] (Id NSString)
optionalStringSelector = mkSelector "optionalString"

-- | @Selector@ for @setOptionalString:@
setOptionalStringSelector :: Selector '[Id NSString] ()
setOptionalStringSelector = mkSelector "setOptionalString:"

-- | @Selector@ for @nullableOptionalString@
nullableOptionalStringSelector :: Selector '[] (Id NSString)
nullableOptionalStringSelector = mkSelector "nullableOptionalString"

-- | @Selector@ for @setNullableOptionalString:@
setNullableOptionalStringSelector :: Selector '[Id NSString] ()
setNullableOptionalStringSelector = mkSelector "setNullableOptionalString:"

-- | @Selector@ for @nullableStruct@
nullableStructSelector :: Selector '[] (Id MTRUnitTestingClusterSimpleStruct)
nullableStructSelector = mkSelector "nullableStruct"

-- | @Selector@ for @setNullableStruct:@
setNullableStructSelector :: Selector '[Id MTRUnitTestingClusterSimpleStruct] ()
setNullableStructSelector = mkSelector "setNullableStruct:"

-- | @Selector@ for @optionalStruct@
optionalStructSelector :: Selector '[] (Id MTRUnitTestingClusterSimpleStruct)
optionalStructSelector = mkSelector "optionalStruct"

-- | @Selector@ for @setOptionalStruct:@
setOptionalStructSelector :: Selector '[Id MTRUnitTestingClusterSimpleStruct] ()
setOptionalStructSelector = mkSelector "setOptionalStruct:"

-- | @Selector@ for @nullableOptionalStruct@
nullableOptionalStructSelector :: Selector '[] (Id MTRUnitTestingClusterSimpleStruct)
nullableOptionalStructSelector = mkSelector "nullableOptionalStruct"

-- | @Selector@ for @setNullableOptionalStruct:@
setNullableOptionalStructSelector :: Selector '[Id MTRUnitTestingClusterSimpleStruct] ()
setNullableOptionalStructSelector = mkSelector "setNullableOptionalStruct:"

-- | @Selector@ for @nullableList@
nullableListSelector :: Selector '[] (Id NSArray)
nullableListSelector = mkSelector "nullableList"

-- | @Selector@ for @setNullableList:@
setNullableListSelector :: Selector '[Id NSArray] ()
setNullableListSelector = mkSelector "setNullableList:"

-- | @Selector@ for @optionalList@
optionalListSelector :: Selector '[] (Id NSArray)
optionalListSelector = mkSelector "optionalList"

-- | @Selector@ for @setOptionalList:@
setOptionalListSelector :: Selector '[Id NSArray] ()
setOptionalListSelector = mkSelector "setOptionalList:"

-- | @Selector@ for @nullableOptionalList@
nullableOptionalListSelector :: Selector '[] (Id NSArray)
nullableOptionalListSelector = mkSelector "nullableOptionalList"

-- | @Selector@ for @setNullableOptionalList:@
setNullableOptionalListSelector :: Selector '[Id NSArray] ()
setNullableOptionalListSelector = mkSelector "setNullableOptionalList:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector '[] (Id NSNumber)
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector '[Id NSNumber] ()
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"

