{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterNullablesAndOptionalsStruct@.
module ObjC.Matter.MTRTestClusterClusterNullablesAndOptionalsStruct
  ( MTRTestClusterClusterNullablesAndOptionalsStruct
  , IsMTRTestClusterClusterNullablesAndOptionalsStruct(..)
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
nullableInt :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id NSNumber)
nullableInt mtrTestClusterClusterNullablesAndOptionalsStruct =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct nullableIntSelector

-- | @- setNullableInt:@
setNullableInt :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsNSNumber value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableInt mtrTestClusterClusterNullablesAndOptionalsStruct value =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct setNullableIntSelector (toNSNumber value)

-- | @- optionalInt@
optionalInt :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id NSNumber)
optionalInt mtrTestClusterClusterNullablesAndOptionalsStruct =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct optionalIntSelector

-- | @- setOptionalInt:@
setOptionalInt :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsNSNumber value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setOptionalInt mtrTestClusterClusterNullablesAndOptionalsStruct value =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct setOptionalIntSelector (toNSNumber value)

-- | @- nullableOptionalInt@
nullableOptionalInt :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id NSNumber)
nullableOptionalInt mtrTestClusterClusterNullablesAndOptionalsStruct =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct nullableOptionalIntSelector

-- | @- setNullableOptionalInt:@
setNullableOptionalInt :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsNSNumber value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableOptionalInt mtrTestClusterClusterNullablesAndOptionalsStruct value =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct setNullableOptionalIntSelector (toNSNumber value)

-- | @- nullableString@
nullableString :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id NSString)
nullableString mtrTestClusterClusterNullablesAndOptionalsStruct =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct nullableStringSelector

-- | @- setNullableString:@
setNullableString :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsNSString value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableString mtrTestClusterClusterNullablesAndOptionalsStruct value =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct setNullableStringSelector (toNSString value)

-- | @- optionalString@
optionalString :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id NSString)
optionalString mtrTestClusterClusterNullablesAndOptionalsStruct =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct optionalStringSelector

-- | @- setOptionalString:@
setOptionalString :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsNSString value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setOptionalString mtrTestClusterClusterNullablesAndOptionalsStruct value =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct setOptionalStringSelector (toNSString value)

-- | @- nullableOptionalString@
nullableOptionalString :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id NSString)
nullableOptionalString mtrTestClusterClusterNullablesAndOptionalsStruct =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct nullableOptionalStringSelector

-- | @- setNullableOptionalString:@
setNullableOptionalString :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsNSString value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableOptionalString mtrTestClusterClusterNullablesAndOptionalsStruct value =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct setNullableOptionalStringSelector (toNSString value)

-- | @- nullableStruct@
nullableStruct :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id MTRTestClusterClusterSimpleStruct)
nullableStruct mtrTestClusterClusterNullablesAndOptionalsStruct =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct nullableStructSelector

-- | @- setNullableStruct:@
setNullableStruct :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsMTRTestClusterClusterSimpleStruct value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableStruct mtrTestClusterClusterNullablesAndOptionalsStruct value =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct setNullableStructSelector (toMTRTestClusterClusterSimpleStruct value)

-- | @- optionalStruct@
optionalStruct :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id MTRTestClusterClusterSimpleStruct)
optionalStruct mtrTestClusterClusterNullablesAndOptionalsStruct =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct optionalStructSelector

-- | @- setOptionalStruct:@
setOptionalStruct :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsMTRTestClusterClusterSimpleStruct value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setOptionalStruct mtrTestClusterClusterNullablesAndOptionalsStruct value =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct setOptionalStructSelector (toMTRTestClusterClusterSimpleStruct value)

-- | @- nullableOptionalStruct@
nullableOptionalStruct :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id MTRTestClusterClusterSimpleStruct)
nullableOptionalStruct mtrTestClusterClusterNullablesAndOptionalsStruct =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct nullableOptionalStructSelector

-- | @- setNullableOptionalStruct:@
setNullableOptionalStruct :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsMTRTestClusterClusterSimpleStruct value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableOptionalStruct mtrTestClusterClusterNullablesAndOptionalsStruct value =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct setNullableOptionalStructSelector (toMTRTestClusterClusterSimpleStruct value)

-- | @- nullableList@
nullableList :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id NSArray)
nullableList mtrTestClusterClusterNullablesAndOptionalsStruct =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct nullableListSelector

-- | @- setNullableList:@
setNullableList :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsNSArray value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableList mtrTestClusterClusterNullablesAndOptionalsStruct value =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct setNullableListSelector (toNSArray value)

-- | @- optionalList@
optionalList :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id NSArray)
optionalList mtrTestClusterClusterNullablesAndOptionalsStruct =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct optionalListSelector

-- | @- setOptionalList:@
setOptionalList :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsNSArray value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setOptionalList mtrTestClusterClusterNullablesAndOptionalsStruct value =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct setOptionalListSelector (toNSArray value)

-- | @- nullableOptionalList@
nullableOptionalList :: IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct => mtrTestClusterClusterNullablesAndOptionalsStruct -> IO (Id NSArray)
nullableOptionalList mtrTestClusterClusterNullablesAndOptionalsStruct =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct nullableOptionalListSelector

-- | @- setNullableOptionalList:@
setNullableOptionalList :: (IsMTRTestClusterClusterNullablesAndOptionalsStruct mtrTestClusterClusterNullablesAndOptionalsStruct, IsNSArray value) => mtrTestClusterClusterNullablesAndOptionalsStruct -> value -> IO ()
setNullableOptionalList mtrTestClusterClusterNullablesAndOptionalsStruct value =
  sendMessage mtrTestClusterClusterNullablesAndOptionalsStruct setNullableOptionalListSelector (toNSArray value)

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
nullableStructSelector :: Selector '[] (Id MTRTestClusterClusterSimpleStruct)
nullableStructSelector = mkSelector "nullableStruct"

-- | @Selector@ for @setNullableStruct:@
setNullableStructSelector :: Selector '[Id MTRTestClusterClusterSimpleStruct] ()
setNullableStructSelector = mkSelector "setNullableStruct:"

-- | @Selector@ for @optionalStruct@
optionalStructSelector :: Selector '[] (Id MTRTestClusterClusterSimpleStruct)
optionalStructSelector = mkSelector "optionalStruct"

-- | @Selector@ for @setOptionalStruct:@
setOptionalStructSelector :: Selector '[Id MTRTestClusterClusterSimpleStruct] ()
setOptionalStructSelector = mkSelector "setOptionalStruct:"

-- | @Selector@ for @nullableOptionalStruct@
nullableOptionalStructSelector :: Selector '[] (Id MTRTestClusterClusterSimpleStruct)
nullableOptionalStructSelector = mkSelector "nullableOptionalStruct"

-- | @Selector@ for @setNullableOptionalStruct:@
setNullableOptionalStructSelector :: Selector '[Id MTRTestClusterClusterSimpleStruct] ()
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

