{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterNullablesAndOptionalsStruct@.
module ObjC.Matter.MTRUnitTestingClusterNullablesAndOptionalsStruct
  ( MTRUnitTestingClusterNullablesAndOptionalsStruct
  , IsMTRUnitTestingClusterNullablesAndOptionalsStruct(..)
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
nullableInt :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id NSNumber)
nullableInt mtrUnitTestingClusterNullablesAndOptionalsStruct =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct nullableIntSelector

-- | @- setNullableInt:@
setNullableInt :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsNSNumber value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableInt mtrUnitTestingClusterNullablesAndOptionalsStruct value =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct setNullableIntSelector (toNSNumber value)

-- | @- optionalInt@
optionalInt :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id NSNumber)
optionalInt mtrUnitTestingClusterNullablesAndOptionalsStruct =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct optionalIntSelector

-- | @- setOptionalInt:@
setOptionalInt :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsNSNumber value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setOptionalInt mtrUnitTestingClusterNullablesAndOptionalsStruct value =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct setOptionalIntSelector (toNSNumber value)

-- | @- nullableOptionalInt@
nullableOptionalInt :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id NSNumber)
nullableOptionalInt mtrUnitTestingClusterNullablesAndOptionalsStruct =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct nullableOptionalIntSelector

-- | @- setNullableOptionalInt:@
setNullableOptionalInt :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsNSNumber value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableOptionalInt mtrUnitTestingClusterNullablesAndOptionalsStruct value =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct setNullableOptionalIntSelector (toNSNumber value)

-- | @- nullableString@
nullableString :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id NSString)
nullableString mtrUnitTestingClusterNullablesAndOptionalsStruct =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct nullableStringSelector

-- | @- setNullableString:@
setNullableString :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsNSString value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableString mtrUnitTestingClusterNullablesAndOptionalsStruct value =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct setNullableStringSelector (toNSString value)

-- | @- optionalString@
optionalString :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id NSString)
optionalString mtrUnitTestingClusterNullablesAndOptionalsStruct =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct optionalStringSelector

-- | @- setOptionalString:@
setOptionalString :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsNSString value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setOptionalString mtrUnitTestingClusterNullablesAndOptionalsStruct value =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct setOptionalStringSelector (toNSString value)

-- | @- nullableOptionalString@
nullableOptionalString :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id NSString)
nullableOptionalString mtrUnitTestingClusterNullablesAndOptionalsStruct =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct nullableOptionalStringSelector

-- | @- setNullableOptionalString:@
setNullableOptionalString :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsNSString value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableOptionalString mtrUnitTestingClusterNullablesAndOptionalsStruct value =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct setNullableOptionalStringSelector (toNSString value)

-- | @- nullableStruct@
nullableStruct :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id MTRUnitTestingClusterSimpleStruct)
nullableStruct mtrUnitTestingClusterNullablesAndOptionalsStruct =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct nullableStructSelector

-- | @- setNullableStruct:@
setNullableStruct :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableStruct mtrUnitTestingClusterNullablesAndOptionalsStruct value =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct setNullableStructSelector (toMTRUnitTestingClusterSimpleStruct value)

-- | @- optionalStruct@
optionalStruct :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id MTRUnitTestingClusterSimpleStruct)
optionalStruct mtrUnitTestingClusterNullablesAndOptionalsStruct =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct optionalStructSelector

-- | @- setOptionalStruct:@
setOptionalStruct :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setOptionalStruct mtrUnitTestingClusterNullablesAndOptionalsStruct value =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct setOptionalStructSelector (toMTRUnitTestingClusterSimpleStruct value)

-- | @- nullableOptionalStruct@
nullableOptionalStruct :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id MTRUnitTestingClusterSimpleStruct)
nullableOptionalStruct mtrUnitTestingClusterNullablesAndOptionalsStruct =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct nullableOptionalStructSelector

-- | @- setNullableOptionalStruct:@
setNullableOptionalStruct :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableOptionalStruct mtrUnitTestingClusterNullablesAndOptionalsStruct value =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct setNullableOptionalStructSelector (toMTRUnitTestingClusterSimpleStruct value)

-- | @- nullableList@
nullableList :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id NSArray)
nullableList mtrUnitTestingClusterNullablesAndOptionalsStruct =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct nullableListSelector

-- | @- setNullableList:@
setNullableList :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsNSArray value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableList mtrUnitTestingClusterNullablesAndOptionalsStruct value =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct setNullableListSelector (toNSArray value)

-- | @- optionalList@
optionalList :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id NSArray)
optionalList mtrUnitTestingClusterNullablesAndOptionalsStruct =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct optionalListSelector

-- | @- setOptionalList:@
setOptionalList :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsNSArray value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setOptionalList mtrUnitTestingClusterNullablesAndOptionalsStruct value =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct setOptionalListSelector (toNSArray value)

-- | @- nullableOptionalList@
nullableOptionalList :: IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct => mtrUnitTestingClusterNullablesAndOptionalsStruct -> IO (Id NSArray)
nullableOptionalList mtrUnitTestingClusterNullablesAndOptionalsStruct =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct nullableOptionalListSelector

-- | @- setNullableOptionalList:@
setNullableOptionalList :: (IsMTRUnitTestingClusterNullablesAndOptionalsStruct mtrUnitTestingClusterNullablesAndOptionalsStruct, IsNSArray value) => mtrUnitTestingClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableOptionalList mtrUnitTestingClusterNullablesAndOptionalsStruct value =
  sendMessage mtrUnitTestingClusterNullablesAndOptionalsStruct setNullableOptionalListSelector (toNSArray value)

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

