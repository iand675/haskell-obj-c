{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterSimpleStruct@.
module ObjC.Matter.MTRTestClusterClusterSimpleStruct
  ( MTRTestClusterClusterSimpleStruct
  , IsMTRTestClusterClusterSimpleStruct(..)
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
  , h
  , setH
  , aSelector
  , bSelector
  , cSelector
  , dSelector
  , eSelector
  , fSelector
  , gSelector
  , hSelector
  , setASelector
  , setBSelector
  , setCSelector
  , setDSelector
  , setESelector
  , setFSelector
  , setGSelector
  , setHSelector


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
a :: IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct => mtrTestClusterClusterSimpleStruct -> IO (Id NSNumber)
a mtrTestClusterClusterSimpleStruct =
  sendMessage mtrTestClusterClusterSimpleStruct aSelector

-- | @- setA:@
setA :: (IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct, IsNSNumber value) => mtrTestClusterClusterSimpleStruct -> value -> IO ()
setA mtrTestClusterClusterSimpleStruct value =
  sendMessage mtrTestClusterClusterSimpleStruct setASelector (toNSNumber value)

-- | @- b@
b :: IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct => mtrTestClusterClusterSimpleStruct -> IO (Id NSNumber)
b mtrTestClusterClusterSimpleStruct =
  sendMessage mtrTestClusterClusterSimpleStruct bSelector

-- | @- setB:@
setB :: (IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct, IsNSNumber value) => mtrTestClusterClusterSimpleStruct -> value -> IO ()
setB mtrTestClusterClusterSimpleStruct value =
  sendMessage mtrTestClusterClusterSimpleStruct setBSelector (toNSNumber value)

-- | @- c@
c :: IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct => mtrTestClusterClusterSimpleStruct -> IO (Id NSNumber)
c mtrTestClusterClusterSimpleStruct =
  sendMessage mtrTestClusterClusterSimpleStruct cSelector

-- | @- setC:@
setC :: (IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct, IsNSNumber value) => mtrTestClusterClusterSimpleStruct -> value -> IO ()
setC mtrTestClusterClusterSimpleStruct value =
  sendMessage mtrTestClusterClusterSimpleStruct setCSelector (toNSNumber value)

-- | @- d@
d :: IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct => mtrTestClusterClusterSimpleStruct -> IO (Id NSData)
d mtrTestClusterClusterSimpleStruct =
  sendMessage mtrTestClusterClusterSimpleStruct dSelector

-- | @- setD:@
setD :: (IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct, IsNSData value) => mtrTestClusterClusterSimpleStruct -> value -> IO ()
setD mtrTestClusterClusterSimpleStruct value =
  sendMessage mtrTestClusterClusterSimpleStruct setDSelector (toNSData value)

-- | @- e@
e :: IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct => mtrTestClusterClusterSimpleStruct -> IO (Id NSString)
e mtrTestClusterClusterSimpleStruct =
  sendMessage mtrTestClusterClusterSimpleStruct eSelector

-- | @- setE:@
setE :: (IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct, IsNSString value) => mtrTestClusterClusterSimpleStruct -> value -> IO ()
setE mtrTestClusterClusterSimpleStruct value =
  sendMessage mtrTestClusterClusterSimpleStruct setESelector (toNSString value)

-- | @- f@
f :: IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct => mtrTestClusterClusterSimpleStruct -> IO (Id NSNumber)
f mtrTestClusterClusterSimpleStruct =
  sendMessage mtrTestClusterClusterSimpleStruct fSelector

-- | @- setF:@
setF :: (IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct, IsNSNumber value) => mtrTestClusterClusterSimpleStruct -> value -> IO ()
setF mtrTestClusterClusterSimpleStruct value =
  sendMessage mtrTestClusterClusterSimpleStruct setFSelector (toNSNumber value)

-- | @- g@
g :: IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct => mtrTestClusterClusterSimpleStruct -> IO (Id NSNumber)
g mtrTestClusterClusterSimpleStruct =
  sendMessage mtrTestClusterClusterSimpleStruct gSelector

-- | @- setG:@
setG :: (IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct, IsNSNumber value) => mtrTestClusterClusterSimpleStruct -> value -> IO ()
setG mtrTestClusterClusterSimpleStruct value =
  sendMessage mtrTestClusterClusterSimpleStruct setGSelector (toNSNumber value)

-- | @- h@
h :: IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct => mtrTestClusterClusterSimpleStruct -> IO (Id NSNumber)
h mtrTestClusterClusterSimpleStruct =
  sendMessage mtrTestClusterClusterSimpleStruct hSelector

-- | @- setH:@
setH :: (IsMTRTestClusterClusterSimpleStruct mtrTestClusterClusterSimpleStruct, IsNSNumber value) => mtrTestClusterClusterSimpleStruct -> value -> IO ()
setH mtrTestClusterClusterSimpleStruct value =
  sendMessage mtrTestClusterClusterSimpleStruct setHSelector (toNSNumber value)

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
cSelector :: Selector '[] (Id NSNumber)
cSelector = mkSelector "c"

-- | @Selector@ for @setC:@
setCSelector :: Selector '[Id NSNumber] ()
setCSelector = mkSelector "setC:"

-- | @Selector@ for @d@
dSelector :: Selector '[] (Id NSData)
dSelector = mkSelector "d"

-- | @Selector@ for @setD:@
setDSelector :: Selector '[Id NSData] ()
setDSelector = mkSelector "setD:"

-- | @Selector@ for @e@
eSelector :: Selector '[] (Id NSString)
eSelector = mkSelector "e"

-- | @Selector@ for @setE:@
setESelector :: Selector '[Id NSString] ()
setESelector = mkSelector "setE:"

-- | @Selector@ for @f@
fSelector :: Selector '[] (Id NSNumber)
fSelector = mkSelector "f"

-- | @Selector@ for @setF:@
setFSelector :: Selector '[Id NSNumber] ()
setFSelector = mkSelector "setF:"

-- | @Selector@ for @g@
gSelector :: Selector '[] (Id NSNumber)
gSelector = mkSelector "g"

-- | @Selector@ for @setG:@
setGSelector :: Selector '[Id NSNumber] ()
setGSelector = mkSelector "setG:"

-- | @Selector@ for @h@
hSelector :: Selector '[] (Id NSNumber)
hSelector = mkSelector "h"

-- | @Selector@ for @setH:@
setHSelector :: Selector '[Id NSNumber] ()
setHSelector = mkSelector "setH:"

