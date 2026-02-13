{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestFabricScoped@.
module ObjC.Matter.MTRUnitTestingClusterTestFabricScoped
  ( MTRUnitTestingClusterTestFabricScoped
  , IsMTRUnitTestingClusterTestFabricScoped(..)
  , fabricSensitiveInt8u
  , setFabricSensitiveInt8u
  , optionalFabricSensitiveInt8u
  , setOptionalFabricSensitiveInt8u
  , nullableFabricSensitiveInt8u
  , setNullableFabricSensitiveInt8u
  , nullableOptionalFabricSensitiveInt8u
  , setNullableOptionalFabricSensitiveInt8u
  , fabricSensitiveCharString
  , setFabricSensitiveCharString
  , fabricSensitiveStruct
  , setFabricSensitiveStruct
  , fabricSensitiveInt8uList
  , setFabricSensitiveInt8uList
  , fabricIndex
  , setFabricIndex
  , fabricIndexSelector
  , fabricSensitiveCharStringSelector
  , fabricSensitiveInt8uListSelector
  , fabricSensitiveInt8uSelector
  , fabricSensitiveStructSelector
  , nullableFabricSensitiveInt8uSelector
  , nullableOptionalFabricSensitiveInt8uSelector
  , optionalFabricSensitiveInt8uSelector
  , setFabricIndexSelector
  , setFabricSensitiveCharStringSelector
  , setFabricSensitiveInt8uListSelector
  , setFabricSensitiveInt8uSelector
  , setFabricSensitiveStructSelector
  , setNullableFabricSensitiveInt8uSelector
  , setNullableOptionalFabricSensitiveInt8uSelector
  , setOptionalFabricSensitiveInt8uSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- fabricSensitiveInt8u@
fabricSensitiveInt8u :: IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped => mtrUnitTestingClusterTestFabricScoped -> IO (Id NSNumber)
fabricSensitiveInt8u mtrUnitTestingClusterTestFabricScoped =
  sendMessage mtrUnitTestingClusterTestFabricScoped fabricSensitiveInt8uSelector

-- | @- setFabricSensitiveInt8u:@
setFabricSensitiveInt8u :: (IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped, IsNSNumber value) => mtrUnitTestingClusterTestFabricScoped -> value -> IO ()
setFabricSensitiveInt8u mtrUnitTestingClusterTestFabricScoped value =
  sendMessage mtrUnitTestingClusterTestFabricScoped setFabricSensitiveInt8uSelector (toNSNumber value)

-- | @- optionalFabricSensitiveInt8u@
optionalFabricSensitiveInt8u :: IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped => mtrUnitTestingClusterTestFabricScoped -> IO (Id NSNumber)
optionalFabricSensitiveInt8u mtrUnitTestingClusterTestFabricScoped =
  sendMessage mtrUnitTestingClusterTestFabricScoped optionalFabricSensitiveInt8uSelector

-- | @- setOptionalFabricSensitiveInt8u:@
setOptionalFabricSensitiveInt8u :: (IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped, IsNSNumber value) => mtrUnitTestingClusterTestFabricScoped -> value -> IO ()
setOptionalFabricSensitiveInt8u mtrUnitTestingClusterTestFabricScoped value =
  sendMessage mtrUnitTestingClusterTestFabricScoped setOptionalFabricSensitiveInt8uSelector (toNSNumber value)

-- | @- nullableFabricSensitiveInt8u@
nullableFabricSensitiveInt8u :: IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped => mtrUnitTestingClusterTestFabricScoped -> IO (Id NSNumber)
nullableFabricSensitiveInt8u mtrUnitTestingClusterTestFabricScoped =
  sendMessage mtrUnitTestingClusterTestFabricScoped nullableFabricSensitiveInt8uSelector

-- | @- setNullableFabricSensitiveInt8u:@
setNullableFabricSensitiveInt8u :: (IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped, IsNSNumber value) => mtrUnitTestingClusterTestFabricScoped -> value -> IO ()
setNullableFabricSensitiveInt8u mtrUnitTestingClusterTestFabricScoped value =
  sendMessage mtrUnitTestingClusterTestFabricScoped setNullableFabricSensitiveInt8uSelector (toNSNumber value)

-- | @- nullableOptionalFabricSensitiveInt8u@
nullableOptionalFabricSensitiveInt8u :: IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped => mtrUnitTestingClusterTestFabricScoped -> IO (Id NSNumber)
nullableOptionalFabricSensitiveInt8u mtrUnitTestingClusterTestFabricScoped =
  sendMessage mtrUnitTestingClusterTestFabricScoped nullableOptionalFabricSensitiveInt8uSelector

-- | @- setNullableOptionalFabricSensitiveInt8u:@
setNullableOptionalFabricSensitiveInt8u :: (IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped, IsNSNumber value) => mtrUnitTestingClusterTestFabricScoped -> value -> IO ()
setNullableOptionalFabricSensitiveInt8u mtrUnitTestingClusterTestFabricScoped value =
  sendMessage mtrUnitTestingClusterTestFabricScoped setNullableOptionalFabricSensitiveInt8uSelector (toNSNumber value)

-- | @- fabricSensitiveCharString@
fabricSensitiveCharString :: IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped => mtrUnitTestingClusterTestFabricScoped -> IO (Id NSString)
fabricSensitiveCharString mtrUnitTestingClusterTestFabricScoped =
  sendMessage mtrUnitTestingClusterTestFabricScoped fabricSensitiveCharStringSelector

-- | @- setFabricSensitiveCharString:@
setFabricSensitiveCharString :: (IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped, IsNSString value) => mtrUnitTestingClusterTestFabricScoped -> value -> IO ()
setFabricSensitiveCharString mtrUnitTestingClusterTestFabricScoped value =
  sendMessage mtrUnitTestingClusterTestFabricScoped setFabricSensitiveCharStringSelector (toNSString value)

-- | @- fabricSensitiveStruct@
fabricSensitiveStruct :: IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped => mtrUnitTestingClusterTestFabricScoped -> IO (Id MTRUnitTestingClusterSimpleStruct)
fabricSensitiveStruct mtrUnitTestingClusterTestFabricScoped =
  sendMessage mtrUnitTestingClusterTestFabricScoped fabricSensitiveStructSelector

-- | @- setFabricSensitiveStruct:@
setFabricSensitiveStruct :: (IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterTestFabricScoped -> value -> IO ()
setFabricSensitiveStruct mtrUnitTestingClusterTestFabricScoped value =
  sendMessage mtrUnitTestingClusterTestFabricScoped setFabricSensitiveStructSelector (toMTRUnitTestingClusterSimpleStruct value)

-- | @- fabricSensitiveInt8uList@
fabricSensitiveInt8uList :: IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped => mtrUnitTestingClusterTestFabricScoped -> IO (Id NSArray)
fabricSensitiveInt8uList mtrUnitTestingClusterTestFabricScoped =
  sendMessage mtrUnitTestingClusterTestFabricScoped fabricSensitiveInt8uListSelector

-- | @- setFabricSensitiveInt8uList:@
setFabricSensitiveInt8uList :: (IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped, IsNSArray value) => mtrUnitTestingClusterTestFabricScoped -> value -> IO ()
setFabricSensitiveInt8uList mtrUnitTestingClusterTestFabricScoped value =
  sendMessage mtrUnitTestingClusterTestFabricScoped setFabricSensitiveInt8uListSelector (toNSArray value)

-- | @- fabricIndex@
fabricIndex :: IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped => mtrUnitTestingClusterTestFabricScoped -> IO (Id NSNumber)
fabricIndex mtrUnitTestingClusterTestFabricScoped =
  sendMessage mtrUnitTestingClusterTestFabricScoped fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRUnitTestingClusterTestFabricScoped mtrUnitTestingClusterTestFabricScoped, IsNSNumber value) => mtrUnitTestingClusterTestFabricScoped -> value -> IO ()
setFabricIndex mtrUnitTestingClusterTestFabricScoped value =
  sendMessage mtrUnitTestingClusterTestFabricScoped setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fabricSensitiveInt8u@
fabricSensitiveInt8uSelector :: Selector '[] (Id NSNumber)
fabricSensitiveInt8uSelector = mkSelector "fabricSensitiveInt8u"

-- | @Selector@ for @setFabricSensitiveInt8u:@
setFabricSensitiveInt8uSelector :: Selector '[Id NSNumber] ()
setFabricSensitiveInt8uSelector = mkSelector "setFabricSensitiveInt8u:"

-- | @Selector@ for @optionalFabricSensitiveInt8u@
optionalFabricSensitiveInt8uSelector :: Selector '[] (Id NSNumber)
optionalFabricSensitiveInt8uSelector = mkSelector "optionalFabricSensitiveInt8u"

-- | @Selector@ for @setOptionalFabricSensitiveInt8u:@
setOptionalFabricSensitiveInt8uSelector :: Selector '[Id NSNumber] ()
setOptionalFabricSensitiveInt8uSelector = mkSelector "setOptionalFabricSensitiveInt8u:"

-- | @Selector@ for @nullableFabricSensitiveInt8u@
nullableFabricSensitiveInt8uSelector :: Selector '[] (Id NSNumber)
nullableFabricSensitiveInt8uSelector = mkSelector "nullableFabricSensitiveInt8u"

-- | @Selector@ for @setNullableFabricSensitiveInt8u:@
setNullableFabricSensitiveInt8uSelector :: Selector '[Id NSNumber] ()
setNullableFabricSensitiveInt8uSelector = mkSelector "setNullableFabricSensitiveInt8u:"

-- | @Selector@ for @nullableOptionalFabricSensitiveInt8u@
nullableOptionalFabricSensitiveInt8uSelector :: Selector '[] (Id NSNumber)
nullableOptionalFabricSensitiveInt8uSelector = mkSelector "nullableOptionalFabricSensitiveInt8u"

-- | @Selector@ for @setNullableOptionalFabricSensitiveInt8u:@
setNullableOptionalFabricSensitiveInt8uSelector :: Selector '[Id NSNumber] ()
setNullableOptionalFabricSensitiveInt8uSelector = mkSelector "setNullableOptionalFabricSensitiveInt8u:"

-- | @Selector@ for @fabricSensitiveCharString@
fabricSensitiveCharStringSelector :: Selector '[] (Id NSString)
fabricSensitiveCharStringSelector = mkSelector "fabricSensitiveCharString"

-- | @Selector@ for @setFabricSensitiveCharString:@
setFabricSensitiveCharStringSelector :: Selector '[Id NSString] ()
setFabricSensitiveCharStringSelector = mkSelector "setFabricSensitiveCharString:"

-- | @Selector@ for @fabricSensitiveStruct@
fabricSensitiveStructSelector :: Selector '[] (Id MTRUnitTestingClusterSimpleStruct)
fabricSensitiveStructSelector = mkSelector "fabricSensitiveStruct"

-- | @Selector@ for @setFabricSensitiveStruct:@
setFabricSensitiveStructSelector :: Selector '[Id MTRUnitTestingClusterSimpleStruct] ()
setFabricSensitiveStructSelector = mkSelector "setFabricSensitiveStruct:"

-- | @Selector@ for @fabricSensitiveInt8uList@
fabricSensitiveInt8uListSelector :: Selector '[] (Id NSArray)
fabricSensitiveInt8uListSelector = mkSelector "fabricSensitiveInt8uList"

-- | @Selector@ for @setFabricSensitiveInt8uList:@
setFabricSensitiveInt8uListSelector :: Selector '[Id NSArray] ()
setFabricSensitiveInt8uListSelector = mkSelector "setFabricSensitiveInt8uList:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

