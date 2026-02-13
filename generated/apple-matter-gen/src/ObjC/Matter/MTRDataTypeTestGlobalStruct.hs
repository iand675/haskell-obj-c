{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypeTestGlobalStruct@.
module ObjC.Matter.MTRDataTypeTestGlobalStruct
  ( MTRDataTypeTestGlobalStruct
  , IsMTRDataTypeTestGlobalStruct(..)
  , name
  , setName
  , myBitmap
  , setMyBitmap
  , myEnum
  , setMyEnum
  , myBitmapSelector
  , myEnumSelector
  , nameSelector
  , setMyBitmapSelector
  , setMyEnumSelector
  , setNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsMTRDataTypeTestGlobalStruct mtrDataTypeTestGlobalStruct => mtrDataTypeTestGlobalStruct -> IO (Id NSString)
name mtrDataTypeTestGlobalStruct =
  sendMessage mtrDataTypeTestGlobalStruct nameSelector

-- | @- setName:@
setName :: (IsMTRDataTypeTestGlobalStruct mtrDataTypeTestGlobalStruct, IsNSString value) => mtrDataTypeTestGlobalStruct -> value -> IO ()
setName mtrDataTypeTestGlobalStruct value =
  sendMessage mtrDataTypeTestGlobalStruct setNameSelector (toNSString value)

-- | @- myBitmap@
myBitmap :: IsMTRDataTypeTestGlobalStruct mtrDataTypeTestGlobalStruct => mtrDataTypeTestGlobalStruct -> IO (Id NSNumber)
myBitmap mtrDataTypeTestGlobalStruct =
  sendMessage mtrDataTypeTestGlobalStruct myBitmapSelector

-- | @- setMyBitmap:@
setMyBitmap :: (IsMTRDataTypeTestGlobalStruct mtrDataTypeTestGlobalStruct, IsNSNumber value) => mtrDataTypeTestGlobalStruct -> value -> IO ()
setMyBitmap mtrDataTypeTestGlobalStruct value =
  sendMessage mtrDataTypeTestGlobalStruct setMyBitmapSelector (toNSNumber value)

-- | @- myEnum@
myEnum :: IsMTRDataTypeTestGlobalStruct mtrDataTypeTestGlobalStruct => mtrDataTypeTestGlobalStruct -> IO (Id NSNumber)
myEnum mtrDataTypeTestGlobalStruct =
  sendMessage mtrDataTypeTestGlobalStruct myEnumSelector

-- | @- setMyEnum:@
setMyEnum :: (IsMTRDataTypeTestGlobalStruct mtrDataTypeTestGlobalStruct, IsNSNumber value) => mtrDataTypeTestGlobalStruct -> value -> IO ()
setMyEnum mtrDataTypeTestGlobalStruct value =
  sendMessage mtrDataTypeTestGlobalStruct setMyEnumSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @myBitmap@
myBitmapSelector :: Selector '[] (Id NSNumber)
myBitmapSelector = mkSelector "myBitmap"

-- | @Selector@ for @setMyBitmap:@
setMyBitmapSelector :: Selector '[Id NSNumber] ()
setMyBitmapSelector = mkSelector "setMyBitmap:"

-- | @Selector@ for @myEnum@
myEnumSelector :: Selector '[] (Id NSNumber)
myEnumSelector = mkSelector "myEnum"

-- | @Selector@ for @setMyEnum:@
setMyEnumSelector :: Selector '[Id NSNumber] ()
setMyEnumSelector = mkSelector "setMyEnum:"

