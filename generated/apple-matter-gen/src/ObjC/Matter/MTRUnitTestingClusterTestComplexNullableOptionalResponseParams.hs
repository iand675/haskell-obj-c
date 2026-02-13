{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestComplexNullableOptionalResponseParams@.
module ObjC.Matter.MTRUnitTestingClusterTestComplexNullableOptionalResponseParams
  ( MTRUnitTestingClusterTestComplexNullableOptionalResponseParams
  , IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams(..)
  , initWithResponseValue_error
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
  , initWithResponseValue_errorSelector
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

-- | Initialize an MTRUnitTestingClusterTestComplexNullableOptionalResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> responseValue -> error_ -> IO (Id MTRUnitTestingClusterTestComplexNullableOptionalResponseParams)
initWithResponseValue_error mtrUnitTestingClusterTestComplexNullableOptionalResponseParams responseValue error_ =
  sendOwnedMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- nullableIntWasNull@
nullableIntWasNull :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableIntWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams nullableIntWasNullSelector

-- | @- setNullableIntWasNull:@
setNullableIntWasNull :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableIntWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setNullableIntWasNullSelector (toNSNumber value)

-- | @- nullableIntValue@
nullableIntValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableIntValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams nullableIntValueSelector

-- | @- setNullableIntValue:@
setNullableIntValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableIntValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setNullableIntValueSelector (toNSNumber value)

-- | @- optionalIntWasPresent@
optionalIntWasPresent :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
optionalIntWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams optionalIntWasPresentSelector

-- | @- setOptionalIntWasPresent:@
setOptionalIntWasPresent :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalIntWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setOptionalIntWasPresentSelector (toNSNumber value)

-- | @- optionalIntValue@
optionalIntValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
optionalIntValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams optionalIntValueSelector

-- | @- setOptionalIntValue:@
setOptionalIntValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalIntValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setOptionalIntValueSelector (toNSNumber value)

-- | @- nullableOptionalIntWasPresent@
nullableOptionalIntWasPresent :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalIntWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams nullableOptionalIntWasPresentSelector

-- | @- setNullableOptionalIntWasPresent:@
setNullableOptionalIntWasPresent :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalIntWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setNullableOptionalIntWasPresentSelector (toNSNumber value)

-- | @- nullableOptionalIntWasNull@
nullableOptionalIntWasNull :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalIntWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams nullableOptionalIntWasNullSelector

-- | @- setNullableOptionalIntWasNull:@
setNullableOptionalIntWasNull :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalIntWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setNullableOptionalIntWasNullSelector (toNSNumber value)

-- | @- nullableOptionalIntValue@
nullableOptionalIntValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalIntValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams nullableOptionalIntValueSelector

-- | @- setNullableOptionalIntValue:@
setNullableOptionalIntValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalIntValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setNullableOptionalIntValueSelector (toNSNumber value)

-- | @- nullableStringWasNull@
nullableStringWasNull :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableStringWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams nullableStringWasNullSelector

-- | @- setNullableStringWasNull:@
setNullableStringWasNull :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableStringWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setNullableStringWasNullSelector (toNSNumber value)

-- | @- nullableStringValue@
nullableStringValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSString)
nullableStringValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams nullableStringValueSelector

-- | @- setNullableStringValue:@
setNullableStringValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSString value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableStringValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setNullableStringValueSelector (toNSString value)

-- | @- optionalStringWasPresent@
optionalStringWasPresent :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
optionalStringWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams optionalStringWasPresentSelector

-- | @- setOptionalStringWasPresent:@
setOptionalStringWasPresent :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalStringWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setOptionalStringWasPresentSelector (toNSNumber value)

-- | @- optionalStringValue@
optionalStringValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSString)
optionalStringValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams optionalStringValueSelector

-- | @- setOptionalStringValue:@
setOptionalStringValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSString value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalStringValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setOptionalStringValueSelector (toNSString value)

-- | @- nullableOptionalStringWasPresent@
nullableOptionalStringWasPresent :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalStringWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams nullableOptionalStringWasPresentSelector

-- | @- setNullableOptionalStringWasPresent:@
setNullableOptionalStringWasPresent :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStringWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setNullableOptionalStringWasPresentSelector (toNSNumber value)

-- | @- nullableOptionalStringWasNull@
nullableOptionalStringWasNull :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalStringWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams nullableOptionalStringWasNullSelector

-- | @- setNullableOptionalStringWasNull:@
setNullableOptionalStringWasNull :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStringWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setNullableOptionalStringWasNullSelector (toNSNumber value)

-- | @- nullableOptionalStringValue@
nullableOptionalStringValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSString)
nullableOptionalStringValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams nullableOptionalStringValueSelector

-- | @- setNullableOptionalStringValue:@
setNullableOptionalStringValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSString value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStringValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setNullableOptionalStringValueSelector (toNSString value)

-- | @- nullableStructWasNull@
nullableStructWasNull :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableStructWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams nullableStructWasNullSelector

-- | @- setNullableStructWasNull:@
setNullableStructWasNull :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableStructWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setNullableStructWasNullSelector (toNSNumber value)

-- | @- nullableStructValue@
nullableStructValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
nullableStructValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams nullableStructValueSelector

-- | @- setNullableStructValue:@
setNullableStructValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableStructValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setNullableStructValueSelector (toMTRUnitTestingClusterSimpleStruct value)

-- | @- optionalStructWasPresent@
optionalStructWasPresent :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
optionalStructWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams optionalStructWasPresentSelector

-- | @- setOptionalStructWasPresent:@
setOptionalStructWasPresent :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalStructWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setOptionalStructWasPresentSelector (toNSNumber value)

-- | @- optionalStructValue@
optionalStructValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
optionalStructValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams optionalStructValueSelector

-- | @- setOptionalStructValue:@
setOptionalStructValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalStructValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setOptionalStructValueSelector (toMTRUnitTestingClusterSimpleStruct value)

-- | @- nullableOptionalStructWasPresent@
nullableOptionalStructWasPresent :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalStructWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams nullableOptionalStructWasPresentSelector

-- | @- setNullableOptionalStructWasPresent:@
setNullableOptionalStructWasPresent :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStructWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setNullableOptionalStructWasPresentSelector (toNSNumber value)

-- | @- nullableOptionalStructWasNull@
nullableOptionalStructWasNull :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalStructWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams nullableOptionalStructWasNullSelector

-- | @- setNullableOptionalStructWasNull:@
setNullableOptionalStructWasNull :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStructWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setNullableOptionalStructWasNullSelector (toNSNumber value)

-- | @- nullableOptionalStructValue@
nullableOptionalStructValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id MTRUnitTestingClusterSimpleStruct)
nullableOptionalStructValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams nullableOptionalStructValueSelector

-- | @- setNullableOptionalStructValue:@
setNullableOptionalStructValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalStructValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setNullableOptionalStructValueSelector (toMTRUnitTestingClusterSimpleStruct value)

-- | @- nullableListWasNull@
nullableListWasNull :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableListWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams nullableListWasNullSelector

-- | @- setNullableListWasNull:@
setNullableListWasNull :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableListWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setNullableListWasNullSelector (toNSNumber value)

-- | @- nullableListValue@
nullableListValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSArray)
nullableListValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams nullableListValueSelector

-- | @- setNullableListValue:@
setNullableListValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSArray value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableListValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setNullableListValueSelector (toNSArray value)

-- | @- optionalListWasPresent@
optionalListWasPresent :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
optionalListWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams optionalListWasPresentSelector

-- | @- setOptionalListWasPresent:@
setOptionalListWasPresent :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalListWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setOptionalListWasPresentSelector (toNSNumber value)

-- | @- optionalListValue@
optionalListValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSArray)
optionalListValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams optionalListValueSelector

-- | @- setOptionalListValue:@
setOptionalListValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSArray value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setOptionalListValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setOptionalListValueSelector (toNSArray value)

-- | @- nullableOptionalListWasPresent@
nullableOptionalListWasPresent :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalListWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams nullableOptionalListWasPresentSelector

-- | @- setNullableOptionalListWasPresent:@
setNullableOptionalListWasPresent :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalListWasPresent mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setNullableOptionalListWasPresentSelector (toNSNumber value)

-- | @- nullableOptionalListWasNull@
nullableOptionalListWasNull :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
nullableOptionalListWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams nullableOptionalListWasNullSelector

-- | @- setNullableOptionalListWasNull:@
setNullableOptionalListWasNull :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalListWasNull mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setNullableOptionalListWasNullSelector (toNSNumber value)

-- | @- nullableOptionalListValue@
nullableOptionalListValue :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSArray)
nullableOptionalListValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams nullableOptionalListValueSelector

-- | @- setNullableOptionalListValue:@
setNullableOptionalListValue :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSArray value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setNullableOptionalListValue mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setNullableOptionalListValueSelector (toNSArray value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterTestComplexNullableOptionalResponseParams =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterTestComplexNullableOptionalResponseParams mtrUnitTestingClusterTestComplexNullableOptionalResponseParams, IsNSNumber value) => mtrUnitTestingClusterTestComplexNullableOptionalResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterTestComplexNullableOptionalResponseParams value =
  sendMessage mtrUnitTestingClusterTestComplexNullableOptionalResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRUnitTestingClusterTestComplexNullableOptionalResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

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

