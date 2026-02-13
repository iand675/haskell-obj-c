{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterNestedStructList@.
module ObjC.Matter.MTRUnitTestingClusterNestedStructList
  ( MTRUnitTestingClusterNestedStructList
  , IsMTRUnitTestingClusterNestedStructList(..)
  , a
  , setA
  , b
  , setB
  , c
  , setC
  , d
  , setD
  , e
  , setE
  , f
  , setF
  , g
  , setG
  , aSelector
  , bSelector
  , cSelector
  , dSelector
  , eSelector
  , fSelector
  , gSelector
  , setASelector
  , setBSelector
  , setCSelector
  , setDSelector
  , setESelector
  , setFSelector
  , setGSelector


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
a :: IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList => mtrUnitTestingClusterNestedStructList -> IO (Id NSNumber)
a mtrUnitTestingClusterNestedStructList =
  sendMessage mtrUnitTestingClusterNestedStructList aSelector

-- | @- setA:@
setA :: (IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList, IsNSNumber value) => mtrUnitTestingClusterNestedStructList -> value -> IO ()
setA mtrUnitTestingClusterNestedStructList value =
  sendMessage mtrUnitTestingClusterNestedStructList setASelector (toNSNumber value)

-- | @- b@
b :: IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList => mtrUnitTestingClusterNestedStructList -> IO (Id NSNumber)
b mtrUnitTestingClusterNestedStructList =
  sendMessage mtrUnitTestingClusterNestedStructList bSelector

-- | @- setB:@
setB :: (IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList, IsNSNumber value) => mtrUnitTestingClusterNestedStructList -> value -> IO ()
setB mtrUnitTestingClusterNestedStructList value =
  sendMessage mtrUnitTestingClusterNestedStructList setBSelector (toNSNumber value)

-- | @- c@
c :: IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList => mtrUnitTestingClusterNestedStructList -> IO (Id MTRUnitTestingClusterSimpleStruct)
c mtrUnitTestingClusterNestedStructList =
  sendMessage mtrUnitTestingClusterNestedStructList cSelector

-- | @- setC:@
setC :: (IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterNestedStructList -> value -> IO ()
setC mtrUnitTestingClusterNestedStructList value =
  sendMessage mtrUnitTestingClusterNestedStructList setCSelector (toMTRUnitTestingClusterSimpleStruct value)

-- | @- d@
d :: IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList => mtrUnitTestingClusterNestedStructList -> IO (Id NSArray)
d mtrUnitTestingClusterNestedStructList =
  sendMessage mtrUnitTestingClusterNestedStructList dSelector

-- | @- setD:@
setD :: (IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList, IsNSArray value) => mtrUnitTestingClusterNestedStructList -> value -> IO ()
setD mtrUnitTestingClusterNestedStructList value =
  sendMessage mtrUnitTestingClusterNestedStructList setDSelector (toNSArray value)

-- | @- e@
e :: IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList => mtrUnitTestingClusterNestedStructList -> IO (Id NSArray)
e mtrUnitTestingClusterNestedStructList =
  sendMessage mtrUnitTestingClusterNestedStructList eSelector

-- | @- setE:@
setE :: (IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList, IsNSArray value) => mtrUnitTestingClusterNestedStructList -> value -> IO ()
setE mtrUnitTestingClusterNestedStructList value =
  sendMessage mtrUnitTestingClusterNestedStructList setESelector (toNSArray value)

-- | @- f@
f :: IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList => mtrUnitTestingClusterNestedStructList -> IO (Id NSArray)
f mtrUnitTestingClusterNestedStructList =
  sendMessage mtrUnitTestingClusterNestedStructList fSelector

-- | @- setF:@
setF :: (IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList, IsNSArray value) => mtrUnitTestingClusterNestedStructList -> value -> IO ()
setF mtrUnitTestingClusterNestedStructList value =
  sendMessage mtrUnitTestingClusterNestedStructList setFSelector (toNSArray value)

-- | @- g@
g :: IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList => mtrUnitTestingClusterNestedStructList -> IO (Id NSArray)
g mtrUnitTestingClusterNestedStructList =
  sendMessage mtrUnitTestingClusterNestedStructList gSelector

-- | @- setG:@
setG :: (IsMTRUnitTestingClusterNestedStructList mtrUnitTestingClusterNestedStructList, IsNSArray value) => mtrUnitTestingClusterNestedStructList -> value -> IO ()
setG mtrUnitTestingClusterNestedStructList value =
  sendMessage mtrUnitTestingClusterNestedStructList setGSelector (toNSArray value)

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
dSelector :: Selector '[] (Id NSArray)
dSelector = mkSelector "d"

-- | @Selector@ for @setD:@
setDSelector :: Selector '[Id NSArray] ()
setDSelector = mkSelector "setD:"

-- | @Selector@ for @e@
eSelector :: Selector '[] (Id NSArray)
eSelector = mkSelector "e"

-- | @Selector@ for @setE:@
setESelector :: Selector '[Id NSArray] ()
setESelector = mkSelector "setE:"

-- | @Selector@ for @f@
fSelector :: Selector '[] (Id NSArray)
fSelector = mkSelector "f"

-- | @Selector@ for @setF:@
setFSelector :: Selector '[Id NSArray] ()
setFSelector = mkSelector "setF:"

-- | @Selector@ for @g@
gSelector :: Selector '[] (Id NSArray)
gSelector = mkSelector "g"

-- | @Selector@ for @setG:@
setGSelector :: Selector '[Id NSArray] ()
setGSelector = mkSelector "setG:"

