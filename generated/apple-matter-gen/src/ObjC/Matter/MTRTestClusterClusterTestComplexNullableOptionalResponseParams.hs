{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestComplexNullableOptionalResponseParams@.
module ObjC.Matter.MTRTestClusterClusterTestComplexNullableOptionalResponseParams
  ( MTRTestClusterClusterTestComplexNullableOptionalResponseParams
  , IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams(..)
  , nullableIntWasNull
  , setNullableIntWasNull
  , nullableIntValue
  , setNullableIntValue
  , optionalIntWasPresent
  , setOptionalIntWasPresent
  , optionalIntValue
  , setOptionalIntValue
  , nullableOptionalIntWasPresent
  , setNullableOptionalIntWasPresent
  , nullableOptionalIntWasNull
  , setNullableOptionalIntWasNull
  , nullableOptionalIntValue
  , setNullableOptionalIntValue
  , nullableStringWasNull
  , setNullableStringWasNull
  , nullableStringValue
  , setNullableStringValue
  , optionalStringWasPresent
  , setOptionalStringWasPresent
  , optionalStringValue
  , setOptionalStringValue
  , nullableOptionalStringWasPresent
  , setNullableOptionalStringWasPresent
  , nullableOptionalStringWasNull
  , setNullableOptionalStringWasNull
  , nullableOptionalStringValue
  , setNullableOptionalStringValue
  , nullableStructWasNull
  , setNullableStructWasNull
  , nullableStructValue
  , setNullableStructValue
  , optionalStructWasPresent
  , setOptionalStructWasPresent
  , optionalStructValue
  , setOptionalStructValue
  , nullableOptionalStructWasPresent
  , setNullableOptionalStructWasPresent
  , nullableOptionalStructWasNull
  , setNullableOptionalStructWasNull
  , nullableOptionalStructValue
  , setNullableOptionalStructValue
  , nullableListWasNull
  , setNullableListWasNull
  , nullableListValue
  , setNullableListValue
  , optionalListWasPresent
  , setOptionalListWasPresent
  , optionalListValue
  , setOptionalListValue
  , nullableOptionalListWasPresent
  , setNullableOptionalListWasPresent
  , nullableOptionalListWasNull
  , setNullableOptionalListWasNull
  , nullableOptionalListValue
  , setNullableOptionalListValue
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , nullableIntValueSelector
  , nullableIntWasNullSelector
  , nullableListValueSelector
  , nullableListWasNullSelector
  , nullableOptionalIntValueSelector
  , nullableOptionalIntWasNullSelector
  , nullableOptionalIntWasPresentSelector
  , nullableOptionalListValueSelector
  , nullableOptionalListWasNullSelector
  , nullableOptionalListWasPresentSelector
  , nullableOptionalStringValueSelector
  , nullableOptionalStringWasNullSelector
  , nullableOptionalStringWasPresentSelector
  , nullableOptionalStructValueSelector
  , nullableOptionalStructWasNullSelector
  , nullableOptionalStructWasPresentSelector
  , nullableStringValueSelector
  , nullableStringWasNullSelector
  , nullableStructValueSelector
  , nullableStructWasNullSelector
  , optionalIntValueSelector
  , optionalIntWasPresentSelector
  , optionalListValueSelector
  , optionalListWasPresentSelector
  , optionalStringValueSelector
  , optionalStringWasPresentSelector
  , optionalStructValueSelector
  , optionalStructWasPresentSelector
  , setNullableIntValueSelector
  , setNullableIntWasNullSelector
  , setNullableListValueSelector
  , setNullableListWasNullSelector
  , setNullableOptionalIntValueSelector
  , setNullableOptionalIntWasNullSelector
  , setNullableOptionalIntWasPresentSelector
  , setNullableOptionalListValueSelector
  , setNullableOptionalListWasNullSelector
  , setNullableOptionalListWasPresentSelector
  , setNullableOptionalStringValueSelector
  , setNullableOptionalStringWasNullSelector
  , setNullableOptionalStringWasPresentSelector
  , setNullableOptionalStructValueSelector
  , setNullableOptionalStructWasNullSelector
  , setNullableOptionalStructWasPresentSelector
  , setNullableStringValueSelector
  , setNullableStringWasNullSelector
  , setNullableStructValueSelector
  , setNullableStructWasNullSelector
  , setOptionalIntValueSelector
  , setOptionalIntWasPresentSelector
  , setOptionalListValueSelector
  , setOptionalListWasPresentSelector
  , setOptionalStringValueSelector
  , setOptionalStringWasPresentSelector
  , setOptionalStructValueSelector
  , setOptionalStructWasPresentSelector
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

-- | @- nullableIntWasNull@
nullableIntWasNull :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableIntWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams nullableIntWasNullSelector

-- | @- setNullableIntWasNull:@
setNullableIntWasNull :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableIntWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setNullableIntWasNullSelector (toNSNumber value)

-- | @- nullableIntValue@
nullableIntValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableIntValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams nullableIntValueSelector

-- | @- setNullableIntValue:@
setNullableIntValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableIntValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setNullableIntValueSelector (toNSNumber value)

-- | @- optionalIntWasPresent@
optionalIntWasPresent :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
optionalIntWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams optionalIntWasPresentSelector

-- | @- setOptionalIntWasPresent:@
setOptionalIntWasPresent :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalIntWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setOptionalIntWasPresentSelector (toNSNumber value)

-- | @- optionalIntValue@
optionalIntValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
optionalIntValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams optionalIntValueSelector

-- | @- setOptionalIntValue:@
setOptionalIntValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalIntValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setOptionalIntValueSelector (toNSNumber value)

-- | @- nullableOptionalIntWasPresent@
nullableOptionalIntWasPresent :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalIntWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams nullableOptionalIntWasPresentSelector

-- | @- setNullableOptionalIntWasPresent:@
setNullableOptionalIntWasPresent :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalIntWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setNullableOptionalIntWasPresentSelector (toNSNumber value)

-- | @- nullableOptionalIntWasNull@
nullableOptionalIntWasNull :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalIntWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams nullableOptionalIntWasNullSelector

-- | @- setNullableOptionalIntWasNull:@
setNullableOptionalIntWasNull :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalIntWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setNullableOptionalIntWasNullSelector (toNSNumber value)

-- | @- nullableOptionalIntValue@
nullableOptionalIntValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalIntValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams nullableOptionalIntValueSelector

-- | @- setNullableOptionalIntValue:@
setNullableOptionalIntValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalIntValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setNullableOptionalIntValueSelector (toNSNumber value)

-- | @- nullableStringWasNull@
nullableStringWasNull :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableStringWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams nullableStringWasNullSelector

-- | @- setNullableStringWasNull:@
setNullableStringWasNull :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableStringWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setNullableStringWasNullSelector (toNSNumber value)

-- | @- nullableStringValue@
nullableStringValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSString)
nullableStringValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams nullableStringValueSelector

-- | @- setNullableStringValue:@
setNullableStringValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSString value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableStringValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setNullableStringValueSelector (toNSString value)

-- | @- optionalStringWasPresent@
optionalStringWasPresent :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
optionalStringWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams optionalStringWasPresentSelector

-- | @- setOptionalStringWasPresent:@
setOptionalStringWasPresent :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalStringWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setOptionalStringWasPresentSelector (toNSNumber value)

-- | @- optionalStringValue@
optionalStringValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSString)
optionalStringValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams optionalStringValueSelector

-- | @- setOptionalStringValue:@
setOptionalStringValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSString value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalStringValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setOptionalStringValueSelector (toNSString value)

-- | @- nullableOptionalStringWasPresent@
nullableOptionalStringWasPresent :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalStringWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams nullableOptionalStringWasPresentSelector

-- | @- setNullableOptionalStringWasPresent:@
setNullableOptionalStringWasPresent :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStringWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setNullableOptionalStringWasPresentSelector (toNSNumber value)

-- | @- nullableOptionalStringWasNull@
nullableOptionalStringWasNull :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalStringWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams nullableOptionalStringWasNullSelector

-- | @- setNullableOptionalStringWasNull:@
setNullableOptionalStringWasNull :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStringWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setNullableOptionalStringWasNullSelector (toNSNumber value)

-- | @- nullableOptionalStringValue@
nullableOptionalStringValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSString)
nullableOptionalStringValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams nullableOptionalStringValueSelector

-- | @- setNullableOptionalStringValue:@
setNullableOptionalStringValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSString value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStringValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setNullableOptionalStringValueSelector (toNSString value)

-- | @- nullableStructWasNull@
nullableStructWasNull :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableStructWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams nullableStructWasNullSelector

-- | @- setNullableStructWasNull:@
setNullableStructWasNull :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableStructWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setNullableStructWasNullSelector (toNSNumber value)

-- | @- nullableStructValue@
nullableStructValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
nullableStructValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams nullableStructValueSelector

-- | @- setNullableStructValue:@
setNullableStructValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableStructValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setNullableStructValueSelector (toMTRUnitTestingClusterSimpleStruct value)

-- | @- optionalStructWasPresent@
optionalStructWasPresent :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
optionalStructWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams optionalStructWasPresentSelector

-- | @- setOptionalStructWasPresent:@
setOptionalStructWasPresent :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalStructWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setOptionalStructWasPresentSelector (toNSNumber value)

-- | @- optionalStructValue@
optionalStructValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
optionalStructValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams optionalStructValueSelector

-- | @- setOptionalStructValue:@
setOptionalStructValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalStructValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setOptionalStructValueSelector (toMTRUnitTestingClusterSimpleStruct value)

-- | @- nullableOptionalStructWasPresent@
nullableOptionalStructWasPresent :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalStructWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams nullableOptionalStructWasPresentSelector

-- | @- setNullableOptionalStructWasPresent:@
setNullableOptionalStructWasPresent :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStructWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setNullableOptionalStructWasPresentSelector (toNSNumber value)

-- | @- nullableOptionalStructWasNull@
nullableOptionalStructWasNull :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalStructWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams nullableOptionalStructWasNullSelector

-- | @- setNullableOptionalStructWasNull:@
setNullableOptionalStructWasNull :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStructWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setNullableOptionalStructWasNullSelector (toNSNumber value)

-- | @- nullableOptionalStructValue@
nullableOptionalStructValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
nullableOptionalStructValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams nullableOptionalStructValueSelector

-- | @- setNullableOptionalStructValue:@
setNullableOptionalStructValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStructValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setNullableOptionalStructValueSelector (toMTRUnitTestingClusterSimpleStruct value)

-- | @- nullableListWasNull@
nullableListWasNull :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableListWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams nullableListWasNullSelector

-- | @- setNullableListWasNull:@
setNullableListWasNull :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableListWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setNullableListWasNullSelector (toNSNumber value)

-- | @- nullableListValue@
nullableListValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSArray)
nullableListValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams nullableListValueSelector

-- | @- setNullableListValue:@
setNullableListValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSArray value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableListValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setNullableListValueSelector (toNSArray value)

-- | @- optionalListWasPresent@
optionalListWasPresent :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
optionalListWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams optionalListWasPresentSelector

-- | @- setOptionalListWasPresent:@
setOptionalListWasPresent :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalListWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setOptionalListWasPresentSelector (toNSNumber value)

-- | @- optionalListValue@
optionalListValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSArray)
optionalListValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams optionalListValueSelector

-- | @- setOptionalListValue:@
setOptionalListValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSArray value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalListValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setOptionalListValueSelector (toNSArray value)

-- | @- nullableOptionalListWasPresent@
nullableOptionalListWasPresent :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalListWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams nullableOptionalListWasPresentSelector

-- | @- setNullableOptionalListWasPresent:@
setNullableOptionalListWasPresent :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalListWasPresent mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setNullableOptionalListWasPresentSelector (toNSNumber value)

-- | @- nullableOptionalListWasNull@
nullableOptionalListWasNull :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalListWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams nullableOptionalListWasNullSelector

-- | @- setNullableOptionalListWasNull:@
setNullableOptionalListWasNull :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalListWasNull mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setNullableOptionalListWasNullSelector (toNSNumber value)

-- | @- nullableOptionalListValue@
nullableOptionalListValue :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSArray)
nullableOptionalListValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams nullableOptionalListValueSelector

-- | @- setNullableOptionalListValue:@
setNullableOptionalListValue :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSArray value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalListValue mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setNullableOptionalListValueSelector (toNSArray value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrTestClusterClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTestClusterClusterTestComplexNullableOptionalResponseParams mtrTestClusterClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrTestClusterClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrTestClusterClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrTestClusterClusterTestComplexNullableOptionalResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nullableIntWasNull@
nullableIntWasNullSelector :: Selector '[] (Id NSNumber)
nullableIntWasNullSelector = mkSelector "nullableIntWasNull"

-- | @Selector@ for @setNullableIntWasNull:@
setNullableIntWasNullSelector :: Selector '[Id NSNumber] ()
setNullableIntWasNullSelector = mkSelector "setNullableIntWasNull:"

-- | @Selector@ for @nullableIntValue@
nullableIntValueSelector :: Selector '[] (Id NSNumber)
nullableIntValueSelector = mkSelector "nullableIntValue"

-- | @Selector@ for @setNullableIntValue:@
setNullableIntValueSelector :: Selector '[Id NSNumber] ()
setNullableIntValueSelector = mkSelector "setNullableIntValue:"

-- | @Selector@ for @optionalIntWasPresent@
optionalIntWasPresentSelector :: Selector '[] (Id NSNumber)
optionalIntWasPresentSelector = mkSelector "optionalIntWasPresent"

-- | @Selector@ for @setOptionalIntWasPresent:@
setOptionalIntWasPresentSelector :: Selector '[Id NSNumber] ()
setOptionalIntWasPresentSelector = mkSelector "setOptionalIntWasPresent:"

-- | @Selector@ for @optionalIntValue@
optionalIntValueSelector :: Selector '[] (Id NSNumber)
optionalIntValueSelector = mkSelector "optionalIntValue"

-- | @Selector@ for @setOptionalIntValue:@
setOptionalIntValueSelector :: Selector '[Id NSNumber] ()
setOptionalIntValueSelector = mkSelector "setOptionalIntValue:"

-- | @Selector@ for @nullableOptionalIntWasPresent@
nullableOptionalIntWasPresentSelector :: Selector '[] (Id NSNumber)
nullableOptionalIntWasPresentSelector = mkSelector "nullableOptionalIntWasPresent"

-- | @Selector@ for @setNullableOptionalIntWasPresent:@
setNullableOptionalIntWasPresentSelector :: Selector '[Id NSNumber] ()
setNullableOptionalIntWasPresentSelector = mkSelector "setNullableOptionalIntWasPresent:"

-- | @Selector@ for @nullableOptionalIntWasNull@
nullableOptionalIntWasNullSelector :: Selector '[] (Id NSNumber)
nullableOptionalIntWasNullSelector = mkSelector "nullableOptionalIntWasNull"

-- | @Selector@ for @setNullableOptionalIntWasNull:@
setNullableOptionalIntWasNullSelector :: Selector '[Id NSNumber] ()
setNullableOptionalIntWasNullSelector = mkSelector "setNullableOptionalIntWasNull:"

-- | @Selector@ for @nullableOptionalIntValue@
nullableOptionalIntValueSelector :: Selector '[] (Id NSNumber)
nullableOptionalIntValueSelector = mkSelector "nullableOptionalIntValue"

-- | @Selector@ for @setNullableOptionalIntValue:@
setNullableOptionalIntValueSelector :: Selector '[Id NSNumber] ()
setNullableOptionalIntValueSelector = mkSelector "setNullableOptionalIntValue:"

-- | @Selector@ for @nullableStringWasNull@
nullableStringWasNullSelector :: Selector '[] (Id NSNumber)
nullableStringWasNullSelector = mkSelector "nullableStringWasNull"

-- | @Selector@ for @setNullableStringWasNull:@
setNullableStringWasNullSelector :: Selector '[Id NSNumber] ()
setNullableStringWasNullSelector = mkSelector "setNullableStringWasNull:"

-- | @Selector@ for @nullableStringValue@
nullableStringValueSelector :: Selector '[] (Id NSString)
nullableStringValueSelector = mkSelector "nullableStringValue"

-- | @Selector@ for @setNullableStringValue:@
setNullableStringValueSelector :: Selector '[Id NSString] ()
setNullableStringValueSelector = mkSelector "setNullableStringValue:"

-- | @Selector@ for @optionalStringWasPresent@
optionalStringWasPresentSelector :: Selector '[] (Id NSNumber)
optionalStringWasPresentSelector = mkSelector "optionalStringWasPresent"

-- | @Selector@ for @setOptionalStringWasPresent:@
setOptionalStringWasPresentSelector :: Selector '[Id NSNumber] ()
setOptionalStringWasPresentSelector = mkSelector "setOptionalStringWasPresent:"

-- | @Selector@ for @optionalStringValue@
optionalStringValueSelector :: Selector '[] (Id NSString)
optionalStringValueSelector = mkSelector "optionalStringValue"

-- | @Selector@ for @setOptionalStringValue:@
setOptionalStringValueSelector :: Selector '[Id NSString] ()
setOptionalStringValueSelector = mkSelector "setOptionalStringValue:"

-- | @Selector@ for @nullableOptionalStringWasPresent@
nullableOptionalStringWasPresentSelector :: Selector '[] (Id NSNumber)
nullableOptionalStringWasPresentSelector = mkSelector "nullableOptionalStringWasPresent"

-- | @Selector@ for @setNullableOptionalStringWasPresent:@
setNullableOptionalStringWasPresentSelector :: Selector '[Id NSNumber] ()
setNullableOptionalStringWasPresentSelector = mkSelector "setNullableOptionalStringWasPresent:"

-- | @Selector@ for @nullableOptionalStringWasNull@
nullableOptionalStringWasNullSelector :: Selector '[] (Id NSNumber)
nullableOptionalStringWasNullSelector = mkSelector "nullableOptionalStringWasNull"

-- | @Selector@ for @setNullableOptionalStringWasNull:@
setNullableOptionalStringWasNullSelector :: Selector '[Id NSNumber] ()
setNullableOptionalStringWasNullSelector = mkSelector "setNullableOptionalStringWasNull:"

-- | @Selector@ for @nullableOptionalStringValue@
nullableOptionalStringValueSelector :: Selector '[] (Id NSString)
nullableOptionalStringValueSelector = mkSelector "nullableOptionalStringValue"

-- | @Selector@ for @setNullableOptionalStringValue:@
setNullableOptionalStringValueSelector :: Selector '[Id NSString] ()
setNullableOptionalStringValueSelector = mkSelector "setNullableOptionalStringValue:"

-- | @Selector@ for @nullableStructWasNull@
nullableStructWasNullSelector :: Selector '[] (Id NSNumber)
nullableStructWasNullSelector = mkSelector "nullableStructWasNull"

-- | @Selector@ for @setNullableStructWasNull:@
setNullableStructWasNullSelector :: Selector '[Id NSNumber] ()
setNullableStructWasNullSelector = mkSelector "setNullableStructWasNull:"

-- | @Selector@ for @nullableStructValue@
nullableStructValueSelector :: Selector '[] (Id MTRUnitTestingClusterSimpleStruct)
nullableStructValueSelector = mkSelector "nullableStructValue"

-- | @Selector@ for @setNullableStructValue:@
setNullableStructValueSelector :: Selector '[Id MTRUnitTestingClusterSimpleStruct] ()
setNullableStructValueSelector = mkSelector "setNullableStructValue:"

-- | @Selector@ for @optionalStructWasPresent@
optionalStructWasPresentSelector :: Selector '[] (Id NSNumber)
optionalStructWasPresentSelector = mkSelector "optionalStructWasPresent"

-- | @Selector@ for @setOptionalStructWasPresent:@
setOptionalStructWasPresentSelector :: Selector '[Id NSNumber] ()
setOptionalStructWasPresentSelector = mkSelector "setOptionalStructWasPresent:"

-- | @Selector@ for @optionalStructValue@
optionalStructValueSelector :: Selector '[] (Id MTRUnitTestingClusterSimpleStruct)
optionalStructValueSelector = mkSelector "optionalStructValue"

-- | @Selector@ for @setOptionalStructValue:@
setOptionalStructValueSelector :: Selector '[Id MTRUnitTestingClusterSimpleStruct] ()
setOptionalStructValueSelector = mkSelector "setOptionalStructValue:"

-- | @Selector@ for @nullableOptionalStructWasPresent@
nullableOptionalStructWasPresentSelector :: Selector '[] (Id NSNumber)
nullableOptionalStructWasPresentSelector = mkSelector "nullableOptionalStructWasPresent"

-- | @Selector@ for @setNullableOptionalStructWasPresent:@
setNullableOptionalStructWasPresentSelector :: Selector '[Id NSNumber] ()
setNullableOptionalStructWasPresentSelector = mkSelector "setNullableOptionalStructWasPresent:"

-- | @Selector@ for @nullableOptionalStructWasNull@
nullableOptionalStructWasNullSelector :: Selector '[] (Id NSNumber)
nullableOptionalStructWasNullSelector = mkSelector "nullableOptionalStructWasNull"

-- | @Selector@ for @setNullableOptionalStructWasNull:@
setNullableOptionalStructWasNullSelector :: Selector '[Id NSNumber] ()
setNullableOptionalStructWasNullSelector = mkSelector "setNullableOptionalStructWasNull:"

-- | @Selector@ for @nullableOptionalStructValue@
nullableOptionalStructValueSelector :: Selector '[] (Id MTRUnitTestingClusterSimpleStruct)
nullableOptionalStructValueSelector = mkSelector "nullableOptionalStructValue"

-- | @Selector@ for @setNullableOptionalStructValue:@
setNullableOptionalStructValueSelector :: Selector '[Id MTRUnitTestingClusterSimpleStruct] ()
setNullableOptionalStructValueSelector = mkSelector "setNullableOptionalStructValue:"

-- | @Selector@ for @nullableListWasNull@
nullableListWasNullSelector :: Selector '[] (Id NSNumber)
nullableListWasNullSelector = mkSelector "nullableListWasNull"

-- | @Selector@ for @setNullableListWasNull:@
setNullableListWasNullSelector :: Selector '[Id NSNumber] ()
setNullableListWasNullSelector = mkSelector "setNullableListWasNull:"

-- | @Selector@ for @nullableListValue@
nullableListValueSelector :: Selector '[] (Id NSArray)
nullableListValueSelector = mkSelector "nullableListValue"

-- | @Selector@ for @setNullableListValue:@
setNullableListValueSelector :: Selector '[Id NSArray] ()
setNullableListValueSelector = mkSelector "setNullableListValue:"

-- | @Selector@ for @optionalListWasPresent@
optionalListWasPresentSelector :: Selector '[] (Id NSNumber)
optionalListWasPresentSelector = mkSelector "optionalListWasPresent"

-- | @Selector@ for @setOptionalListWasPresent:@
setOptionalListWasPresentSelector :: Selector '[Id NSNumber] ()
setOptionalListWasPresentSelector = mkSelector "setOptionalListWasPresent:"

-- | @Selector@ for @optionalListValue@
optionalListValueSelector :: Selector '[] (Id NSArray)
optionalListValueSelector = mkSelector "optionalListValue"

-- | @Selector@ for @setOptionalListValue:@
setOptionalListValueSelector :: Selector '[Id NSArray] ()
setOptionalListValueSelector = mkSelector "setOptionalListValue:"

-- | @Selector@ for @nullableOptionalListWasPresent@
nullableOptionalListWasPresentSelector :: Selector '[] (Id NSNumber)
nullableOptionalListWasPresentSelector = mkSelector "nullableOptionalListWasPresent"

-- | @Selector@ for @setNullableOptionalListWasPresent:@
setNullableOptionalListWasPresentSelector :: Selector '[Id NSNumber] ()
setNullableOptionalListWasPresentSelector = mkSelector "setNullableOptionalListWasPresent:"

-- | @Selector@ for @nullableOptionalListWasNull@
nullableOptionalListWasNullSelector :: Selector '[] (Id NSNumber)
nullableOptionalListWasNullSelector = mkSelector "nullableOptionalListWasNull"

-- | @Selector@ for @setNullableOptionalListWasNull:@
setNullableOptionalListWasNullSelector :: Selector '[Id NSNumber] ()
setNullableOptionalListWasNullSelector = mkSelector "setNullableOptionalListWasNull:"

-- | @Selector@ for @nullableOptionalListValue@
nullableOptionalListValueSelector :: Selector '[] (Id NSArray)
nullableOptionalListValueSelector = mkSelector "nullableOptionalListValue"

-- | @Selector@ for @setNullableOptionalListValue:@
setNullableOptionalListValueSelector :: Selector '[Id NSArray] ()
setNullableOptionalListValueSelector = mkSelector "setNullableOptionalListValue:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

