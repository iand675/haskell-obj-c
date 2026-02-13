{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestFabricScoped@.
module ObjC.Matter.MTRTestClusterClusterTestFabricScoped
  ( MTRTestClusterClusterTestFabricScoped
  , IsMTRTestClusterClusterTestFabricScoped(..)
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
fabricSensitiveInt8u :: IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped => mtrTestClusterClusterTestFabricScoped -> IO (Id NSNumber)
fabricSensitiveInt8u mtrTestClusterClusterTestFabricScoped =
  sendMessage mtrTestClusterClusterTestFabricScoped fabricSensitiveInt8uSelector

-- | @- setFabricSensitiveInt8u:@
setFabricSensitiveInt8u :: (IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped, IsNSNumber value) => mtrTestClusterClusterTestFabricScoped -> value -> IO ()
setFabricSensitiveInt8u mtrTestClusterClusterTestFabricScoped value =
  sendMessage mtrTestClusterClusterTestFabricScoped setFabricSensitiveInt8uSelector (toNSNumber value)

-- | @- optionalFabricSensitiveInt8u@
optionalFabricSensitiveInt8u :: IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped => mtrTestClusterClusterTestFabricScoped -> IO (Id NSNumber)
optionalFabricSensitiveInt8u mtrTestClusterClusterTestFabricScoped =
  sendMessage mtrTestClusterClusterTestFabricScoped optionalFabricSensitiveInt8uSelector

-- | @- setOptionalFabricSensitiveInt8u:@
setOptionalFabricSensitiveInt8u :: (IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped, IsNSNumber value) => mtrTestClusterClusterTestFabricScoped -> value -> IO ()
setOptionalFabricSensitiveInt8u mtrTestClusterClusterTestFabricScoped value =
  sendMessage mtrTestClusterClusterTestFabricScoped setOptionalFabricSensitiveInt8uSelector (toNSNumber value)

-- | @- nullableFabricSensitiveInt8u@
nullableFabricSensitiveInt8u :: IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped => mtrTestClusterClusterTestFabricScoped -> IO (Id NSNumber)
nullableFabricSensitiveInt8u mtrTestClusterClusterTestFabricScoped =
  sendMessage mtrTestClusterClusterTestFabricScoped nullableFabricSensitiveInt8uSelector

-- | @- setNullableFabricSensitiveInt8u:@
setNullableFabricSensitiveInt8u :: (IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped, IsNSNumber value) => mtrTestClusterClusterTestFabricScoped -> value -> IO ()
setNullableFabricSensitiveInt8u mtrTestClusterClusterTestFabricScoped value =
  sendMessage mtrTestClusterClusterTestFabricScoped setNullableFabricSensitiveInt8uSelector (toNSNumber value)

-- | @- nullableOptionalFabricSensitiveInt8u@
nullableOptionalFabricSensitiveInt8u :: IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped => mtrTestClusterClusterTestFabricScoped -> IO (Id NSNumber)
nullableOptionalFabricSensitiveInt8u mtrTestClusterClusterTestFabricScoped =
  sendMessage mtrTestClusterClusterTestFabricScoped nullableOptionalFabricSensitiveInt8uSelector

-- | @- setNullableOptionalFabricSensitiveInt8u:@
setNullableOptionalFabricSensitiveInt8u :: (IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped, IsNSNumber value) => mtrTestClusterClusterTestFabricScoped -> value -> IO ()
setNullableOptionalFabricSensitiveInt8u mtrTestClusterClusterTestFabricScoped value =
  sendMessage mtrTestClusterClusterTestFabricScoped setNullableOptionalFabricSensitiveInt8uSelector (toNSNumber value)

-- | @- fabricSensitiveCharString@
fabricSensitiveCharString :: IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped => mtrTestClusterClusterTestFabricScoped -> IO (Id NSString)
fabricSensitiveCharString mtrTestClusterClusterTestFabricScoped =
  sendMessage mtrTestClusterClusterTestFabricScoped fabricSensitiveCharStringSelector

-- | @- setFabricSensitiveCharString:@
setFabricSensitiveCharString :: (IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped, IsNSString value) => mtrTestClusterClusterTestFabricScoped -> value -> IO ()
setFabricSensitiveCharString mtrTestClusterClusterTestFabricScoped value =
  sendMessage mtrTestClusterClusterTestFabricScoped setFabricSensitiveCharStringSelector (toNSString value)

-- | @- fabricSensitiveStruct@
fabricSensitiveStruct :: IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped => mtrTestClusterClusterTestFabricScoped -> IO (Id MTRTestClusterClusterSimpleStruct)
fabricSensitiveStruct mtrTestClusterClusterTestFabricScoped =
  sendMessage mtrTestClusterClusterTestFabricScoped fabricSensitiveStructSelector

-- | @- setFabricSensitiveStruct:@
setFabricSensitiveStruct :: (IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped, IsMTRTestClusterClusterSimpleStruct value) => mtrTestClusterClusterTestFabricScoped -> value -> IO ()
setFabricSensitiveStruct mtrTestClusterClusterTestFabricScoped value =
  sendMessage mtrTestClusterClusterTestFabricScoped setFabricSensitiveStructSelector (toMTRTestClusterClusterSimpleStruct value)

-- | @- fabricSensitiveInt8uList@
fabricSensitiveInt8uList :: IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped => mtrTestClusterClusterTestFabricScoped -> IO (Id NSArray)
fabricSensitiveInt8uList mtrTestClusterClusterTestFabricScoped =
  sendMessage mtrTestClusterClusterTestFabricScoped fabricSensitiveInt8uListSelector

-- | @- setFabricSensitiveInt8uList:@
setFabricSensitiveInt8uList :: (IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped, IsNSArray value) => mtrTestClusterClusterTestFabricScoped -> value -> IO ()
setFabricSensitiveInt8uList mtrTestClusterClusterTestFabricScoped value =
  sendMessage mtrTestClusterClusterTestFabricScoped setFabricSensitiveInt8uListSelector (toNSArray value)

-- | @- fabricIndex@
fabricIndex :: IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped => mtrTestClusterClusterTestFabricScoped -> IO (Id NSNumber)
fabricIndex mtrTestClusterClusterTestFabricScoped =
  sendMessage mtrTestClusterClusterTestFabricScoped fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRTestClusterClusterTestFabricScoped mtrTestClusterClusterTestFabricScoped, IsNSNumber value) => mtrTestClusterClusterTestFabricScoped -> value -> IO ()
setFabricIndex mtrTestClusterClusterTestFabricScoped value =
  sendMessage mtrTestClusterClusterTestFabricScoped setFabricIndexSelector (toNSNumber value)

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
fabricSensitiveStructSelector :: Selector '[] (Id MTRTestClusterClusterSimpleStruct)
fabricSensitiveStructSelector = mkSelector "fabricSensitiveStruct"

-- | @Selector@ for @setFabricSensitiveStruct:@
setFabricSensitiveStructSelector :: Selector '[Id MTRTestClusterClusterSimpleStruct] ()
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

