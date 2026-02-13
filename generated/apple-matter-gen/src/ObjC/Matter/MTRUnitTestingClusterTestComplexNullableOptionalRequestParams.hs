{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestComplexNullableOptionalRequestParams@.
module ObjC.Matter.MTRUnitTestingClusterTestComplexNullableOptionalRequestParams
  ( MTRUnitTestingClusterTestComplexNullableOptionalRequestParams
  , IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams(..)
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
nullableInt :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id NSNumber)
nullableInt mtrUnitTestingClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams nullableIntSelector

-- | @- setNullableInt:@
setNullableInt :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableInt mtrUnitTestingClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams setNullableIntSelector (toNSNumber value)

-- | @- optionalInt@
optionalInt :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id NSNumber)
optionalInt mtrUnitTestingClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams optionalIntSelector

-- | @- setOptionalInt:@
setOptionalInt :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setOptionalInt mtrUnitTestingClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams setOptionalIntSelector (toNSNumber value)

-- | @- nullableOptionalInt@
nullableOptionalInt :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id NSNumber)
nullableOptionalInt mtrUnitTestingClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams nullableOptionalIntSelector

-- | @- setNullableOptionalInt:@
setNullableOptionalInt :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableOptionalInt mtrUnitTestingClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams setNullableOptionalIntSelector (toNSNumber value)

-- | @- nullableString@
nullableString :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id NSString)
nullableString mtrUnitTestingClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams nullableStringSelector

-- | @- setNullableString:@
setNullableString :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsNSString value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableString mtrUnitTestingClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams setNullableStringSelector (toNSString value)

-- | @- optionalString@
optionalString :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id NSString)
optionalString mtrUnitTestingClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams optionalStringSelector

-- | @- setOptionalString:@
setOptionalString :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsNSString value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setOptionalString mtrUnitTestingClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams setOptionalStringSelector (toNSString value)

-- | @- nullableOptionalString@
nullableOptionalString :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id NSString)
nullableOptionalString mtrUnitTestingClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams nullableOptionalStringSelector

-- | @- setNullableOptionalString:@
setNullableOptionalString :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsNSString value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableOptionalString mtrUnitTestingClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams setNullableOptionalStringSelector (toNSString value)

-- | @- nullableStruct@
nullableStruct :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
nullableStruct mtrUnitTestingClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams nullableStructSelector

-- | @- setNullableStruct:@
setNullableStruct :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableStruct mtrUnitTestingClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams setNullableStructSelector (toMTRUnitTestingClusterSimpleStruct value)

-- | @- optionalStruct@
optionalStruct :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
optionalStruct mtrUnitTestingClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams optionalStructSelector

-- | @- setOptionalStruct:@
setOptionalStruct :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setOptionalStruct mtrUnitTestingClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams setOptionalStructSelector (toMTRUnitTestingClusterSimpleStruct value)

-- | @- nullableOptionalStruct@
nullableOptionalStruct :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
nullableOptionalStruct mtrUnitTestingClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams nullableOptionalStructSelector

-- | @- setNullableOptionalStruct:@
setNullableOptionalStruct :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableOptionalStruct mtrUnitTestingClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams setNullableOptionalStructSelector (toMTRUnitTestingClusterSimpleStruct value)

-- | @- nullableList@
nullableList :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id NSArray)
nullableList mtrUnitTestingClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams nullableListSelector

-- | @- setNullableList:@
setNullableList :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsNSArray value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableList mtrUnitTestingClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams setNullableListSelector (toNSArray value)

-- | @- optionalList@
optionalList :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id NSArray)
optionalList mtrUnitTestingClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams optionalListSelector

-- | @- setOptionalList:@
setOptionalList :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsNSArray value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setOptionalList mtrUnitTestingClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams setOptionalListSelector (toNSArray value)

-- | @- nullableOptionalList@
nullableOptionalList :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id NSArray)
nullableOptionalList mtrUnitTestingClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams nullableOptionalListSelector

-- | @- setNullableOptionalList:@
setNullableOptionalList :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsNSArray value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setNullableOptionalList mtrUnitTestingClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams setNullableOptionalListSelector (toNSArray value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrUnitTestingClusterTestComplexNullableOptionalRequestParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRUnitTestingClusterTestComplexNullableOptionalRequestParams mtrUnitTestingClusterTestComplexNullableOptionalRequestParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrUnitTestingClusterTestComplexNullableOptionalRequestParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

