{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterNestedStructList@.
module ObjC.Matter.MTRTestClusterClusterNestedStructList
  ( MTRTestClusterClusterNestedStructList
  , IsMTRTestClusterClusterNestedStructList(..)
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
a :: IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList => mtrTestClusterClusterNestedStructList -> IO (Id NSNumber)
a mtrTestClusterClusterNestedStructList =
  sendMessage mtrTestClusterClusterNestedStructList aSelector

-- | @- setA:@
setA :: (IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList, IsNSNumber value) => mtrTestClusterClusterNestedStructList -> value -> IO ()
setA mtrTestClusterClusterNestedStructList value =
  sendMessage mtrTestClusterClusterNestedStructList setASelector (toNSNumber value)

-- | @- b@
b :: IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList => mtrTestClusterClusterNestedStructList -> IO (Id NSNumber)
b mtrTestClusterClusterNestedStructList =
  sendMessage mtrTestClusterClusterNestedStructList bSelector

-- | @- setB:@
setB :: (IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList, IsNSNumber value) => mtrTestClusterClusterNestedStructList -> value -> IO ()
setB mtrTestClusterClusterNestedStructList value =
  sendMessage mtrTestClusterClusterNestedStructList setBSelector (toNSNumber value)

-- | @- c@
c :: IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList => mtrTestClusterClusterNestedStructList -> IO (Id MTRTestClusterClusterSimpleStruct)
c mtrTestClusterClusterNestedStructList =
  sendMessage mtrTestClusterClusterNestedStructList cSelector

-- | @- setC:@
setC :: (IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList, IsMTRTestClusterClusterSimpleStruct value) => mtrTestClusterClusterNestedStructList -> value -> IO ()
setC mtrTestClusterClusterNestedStructList value =
  sendMessage mtrTestClusterClusterNestedStructList setCSelector (toMTRTestClusterClusterSimpleStruct value)

-- | @- d@
d :: IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList => mtrTestClusterClusterNestedStructList -> IO (Id NSArray)
d mtrTestClusterClusterNestedStructList =
  sendMessage mtrTestClusterClusterNestedStructList dSelector

-- | @- setD:@
setD :: (IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList, IsNSArray value) => mtrTestClusterClusterNestedStructList -> value -> IO ()
setD mtrTestClusterClusterNestedStructList value =
  sendMessage mtrTestClusterClusterNestedStructList setDSelector (toNSArray value)

-- | @- e@
e :: IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList => mtrTestClusterClusterNestedStructList -> IO (Id NSArray)
e mtrTestClusterClusterNestedStructList =
  sendMessage mtrTestClusterClusterNestedStructList eSelector

-- | @- setE:@
setE :: (IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList, IsNSArray value) => mtrTestClusterClusterNestedStructList -> value -> IO ()
setE mtrTestClusterClusterNestedStructList value =
  sendMessage mtrTestClusterClusterNestedStructList setESelector (toNSArray value)

-- | @- f@
f :: IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList => mtrTestClusterClusterNestedStructList -> IO (Id NSArray)
f mtrTestClusterClusterNestedStructList =
  sendMessage mtrTestClusterClusterNestedStructList fSelector

-- | @- setF:@
setF :: (IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList, IsNSArray value) => mtrTestClusterClusterNestedStructList -> value -> IO ()
setF mtrTestClusterClusterNestedStructList value =
  sendMessage mtrTestClusterClusterNestedStructList setFSelector (toNSArray value)

-- | @- g@
g :: IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList => mtrTestClusterClusterNestedStructList -> IO (Id NSArray)
g mtrTestClusterClusterNestedStructList =
  sendMessage mtrTestClusterClusterNestedStructList gSelector

-- | @- setG:@
setG :: (IsMTRTestClusterClusterNestedStructList mtrTestClusterClusterNestedStructList, IsNSArray value) => mtrTestClusterClusterNestedStructList -> value -> IO ()
setG mtrTestClusterClusterNestedStructList value =
  sendMessage mtrTestClusterClusterNestedStructList setGSelector (toNSArray value)

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
cSelector :: Selector '[] (Id MTRTestClusterClusterSimpleStruct)
cSelector = mkSelector "c"

-- | @Selector@ for @setC:@
setCSelector :: Selector '[Id MTRTestClusterClusterSimpleStruct] ()
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

