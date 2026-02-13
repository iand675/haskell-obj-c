{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterNestedStruct@.
module ObjC.Matter.MTRUnitTestingClusterNestedStruct
  ( MTRUnitTestingClusterNestedStruct
  , IsMTRUnitTestingClusterNestedStruct(..)
  , a
  , setA
  , b
  , setB
  , c
  , setC
  , d
  , setD
  , aSelector
  , bSelector
  , cSelector
  , dSelector
  , setASelector
  , setBSelector
  , setCSelector
  , setDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- a@
a :: IsMTRUnitTestingClusterNestedStruct mtrUnitTestingClusterNestedStruct => mtrUnitTestingClusterNestedStruct -> IO (Id NSNumber)
a mtrUnitTestingClusterNestedStruct =
  sendMessage mtrUnitTestingClusterNestedStruct aSelector

-- | @- setA:@
setA :: (IsMTRUnitTestingClusterNestedStruct mtrUnitTestingClusterNestedStruct, IsNSNumber value) => mtrUnitTestingClusterNestedStruct -> value -> IO ()
setA mtrUnitTestingClusterNestedStruct value =
  sendMessage mtrUnitTestingClusterNestedStruct setASelector (toNSNumber value)

-- | @- b@
b :: IsMTRUnitTestingClusterNestedStruct mtrUnitTestingClusterNestedStruct => mtrUnitTestingClusterNestedStruct -> IO (Id NSNumber)
b mtrUnitTestingClusterNestedStruct =
  sendMessage mtrUnitTestingClusterNestedStruct bSelector

-- | @- setB:@
setB :: (IsMTRUnitTestingClusterNestedStruct mtrUnitTestingClusterNestedStruct, IsNSNumber value) => mtrUnitTestingClusterNestedStruct -> value -> IO ()
setB mtrUnitTestingClusterNestedStruct value =
  sendMessage mtrUnitTestingClusterNestedStruct setBSelector (toNSNumber value)

-- | @- c@
c :: IsMTRUnitTestingClusterNestedStruct mtrUnitTestingClusterNestedStruct => mtrUnitTestingClusterNestedStruct -> IO (Id MTRUnitTestingClusterSimpleStruct)
c mtrUnitTestingClusterNestedStruct =
  sendMessage mtrUnitTestingClusterNestedStruct cSelector

-- | @- setC:@
setC :: (IsMTRUnitTestingClusterNestedStruct mtrUnitTestingClusterNestedStruct, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterNestedStruct -> value -> IO ()
setC mtrUnitTestingClusterNestedStruct value =
  sendMessage mtrUnitTestingClusterNestedStruct setCSelector (toMTRUnitTestingClusterSimpleStruct value)

-- | @- d@
d :: IsMTRUnitTestingClusterNestedStruct mtrUnitTestingClusterNestedStruct => mtrUnitTestingClusterNestedStruct -> IO (Id MTRDataTypeTestGlobalStruct)
d mtrUnitTestingClusterNestedStruct =
  sendMessage mtrUnitTestingClusterNestedStruct dSelector

-- | @- setD:@
setD :: (IsMTRUnitTestingClusterNestedStruct mtrUnitTestingClusterNestedStruct, IsMTRDataTypeTestGlobalStruct value) => mtrUnitTestingClusterNestedStruct -> value -> IO ()
setD mtrUnitTestingClusterNestedStruct value =
  sendMessage mtrUnitTestingClusterNestedStruct setDSelector (toMTRDataTypeTestGlobalStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @a@
aSelector :: Selector '[] (Id NSNumber)
aSelector = mkSelector "a"

-- | @Selector@ for @setA:@
setASelector :: Selector '[Id NSNumber] ()
setASelector = mkSelector "setA:"

-- | @Selector@ for @b@
bSelector :: Selector '[] (Id NSNumber)
bSelector = mkSelector "b"

-- | @Selector@ for @setB:@
setBSelector :: Selector '[Id NSNumber] ()
setBSelector = mkSelector "setB:"

-- | @Selector@ for @c@
cSelector :: Selector '[] (Id MTRUnitTestingClusterSimpleStruct)
cSelector = mkSelector "c"

-- | @Selector@ for @setC:@
setCSelector :: Selector '[Id MTRUnitTestingClusterSimpleStruct] ()
setCSelector = mkSelector "setC:"

-- | @Selector@ for @d@
dSelector :: Selector '[] (Id MTRDataTypeTestGlobalStruct)
dSelector = mkSelector "d"

-- | @Selector@ for @setD:@
setDSelector :: Selector '[Id MTRDataTypeTestGlobalStruct] ()
setDSelector = mkSelector "setD:"

