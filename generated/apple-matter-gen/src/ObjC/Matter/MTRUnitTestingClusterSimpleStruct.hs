{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterSimpleStruct@.
module ObjC.Matter.MTRUnitTestingClusterSimpleStruct
  ( MTRUnitTestingClusterSimpleStruct
  , IsMTRUnitTestingClusterSimpleStruct(..)
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
  , i
  , setI
  , aSelector
  , bSelector
  , cSelector
  , dSelector
  , eSelector
  , fSelector
  , gSelector
  , hSelector
  , iSelector
  , setASelector
  , setBSelector
  , setCSelector
  , setDSelector
  , setESelector
  , setFSelector
  , setGSelector
  , setHSelector
  , setISelector


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
a :: IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct => mtrUnitTestingClusterSimpleStruct -> IO (Id NSNumber)
a mtrUnitTestingClusterSimpleStruct =
  sendMessage mtrUnitTestingClusterSimpleStruct aSelector

-- | @- setA:@
setA :: (IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct, IsNSNumber value) => mtrUnitTestingClusterSimpleStruct -> value -> IO ()
setA mtrUnitTestingClusterSimpleStruct value =
  sendMessage mtrUnitTestingClusterSimpleStruct setASelector (toNSNumber value)

-- | @- b@
b :: IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct => mtrUnitTestingClusterSimpleStruct -> IO (Id NSNumber)
b mtrUnitTestingClusterSimpleStruct =
  sendMessage mtrUnitTestingClusterSimpleStruct bSelector

-- | @- setB:@
setB :: (IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct, IsNSNumber value) => mtrUnitTestingClusterSimpleStruct -> value -> IO ()
setB mtrUnitTestingClusterSimpleStruct value =
  sendMessage mtrUnitTestingClusterSimpleStruct setBSelector (toNSNumber value)

-- | @- c@
c :: IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct => mtrUnitTestingClusterSimpleStruct -> IO (Id NSNumber)
c mtrUnitTestingClusterSimpleStruct =
  sendMessage mtrUnitTestingClusterSimpleStruct cSelector

-- | @- setC:@
setC :: (IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct, IsNSNumber value) => mtrUnitTestingClusterSimpleStruct -> value -> IO ()
setC mtrUnitTestingClusterSimpleStruct value =
  sendMessage mtrUnitTestingClusterSimpleStruct setCSelector (toNSNumber value)

-- | @- d@
d :: IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct => mtrUnitTestingClusterSimpleStruct -> IO (Id NSData)
d mtrUnitTestingClusterSimpleStruct =
  sendMessage mtrUnitTestingClusterSimpleStruct dSelector

-- | @- setD:@
setD :: (IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct, IsNSData value) => mtrUnitTestingClusterSimpleStruct -> value -> IO ()
setD mtrUnitTestingClusterSimpleStruct value =
  sendMessage mtrUnitTestingClusterSimpleStruct setDSelector (toNSData value)

-- | @- e@
e :: IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct => mtrUnitTestingClusterSimpleStruct -> IO (Id NSString)
e mtrUnitTestingClusterSimpleStruct =
  sendMessage mtrUnitTestingClusterSimpleStruct eSelector

-- | @- setE:@
setE :: (IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct, IsNSString value) => mtrUnitTestingClusterSimpleStruct -> value -> IO ()
setE mtrUnitTestingClusterSimpleStruct value =
  sendMessage mtrUnitTestingClusterSimpleStruct setESelector (toNSString value)

-- | @- f@
f :: IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct => mtrUnitTestingClusterSimpleStruct -> IO (Id NSNumber)
f mtrUnitTestingClusterSimpleStruct =
  sendMessage mtrUnitTestingClusterSimpleStruct fSelector

-- | @- setF:@
setF :: (IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct, IsNSNumber value) => mtrUnitTestingClusterSimpleStruct -> value -> IO ()
setF mtrUnitTestingClusterSimpleStruct value =
  sendMessage mtrUnitTestingClusterSimpleStruct setFSelector (toNSNumber value)

-- | @- g@
g :: IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct => mtrUnitTestingClusterSimpleStruct -> IO (Id NSNumber)
g mtrUnitTestingClusterSimpleStruct =
  sendMessage mtrUnitTestingClusterSimpleStruct gSelector

-- | @- setG:@
setG :: (IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct, IsNSNumber value) => mtrUnitTestingClusterSimpleStruct -> value -> IO ()
setG mtrUnitTestingClusterSimpleStruct value =
  sendMessage mtrUnitTestingClusterSimpleStruct setGSelector (toNSNumber value)

-- | @- h@
h :: IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct => mtrUnitTestingClusterSimpleStruct -> IO (Id NSNumber)
h mtrUnitTestingClusterSimpleStruct =
  sendMessage mtrUnitTestingClusterSimpleStruct hSelector

-- | @- setH:@
setH :: (IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct, IsNSNumber value) => mtrUnitTestingClusterSimpleStruct -> value -> IO ()
setH mtrUnitTestingClusterSimpleStruct value =
  sendMessage mtrUnitTestingClusterSimpleStruct setHSelector (toNSNumber value)

-- | @- i@
i :: IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct => mtrUnitTestingClusterSimpleStruct -> IO (Id NSNumber)
i mtrUnitTestingClusterSimpleStruct =
  sendMessage mtrUnitTestingClusterSimpleStruct iSelector

-- | @- setI:@
setI :: (IsMTRUnitTestingClusterSimpleStruct mtrUnitTestingClusterSimpleStruct, IsNSNumber value) => mtrUnitTestingClusterSimpleStruct -> value -> IO ()
setI mtrUnitTestingClusterSimpleStruct value =
  sendMessage mtrUnitTestingClusterSimpleStruct setISelector (toNSNumber value)

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

-- | @Selector@ for @i@
iSelector :: Selector '[] (Id NSNumber)
iSelector = mkSelector "i"

-- | @Selector@ for @setI:@
setISelector :: Selector '[Id NSNumber] ()
setISelector = mkSelector "setI:"

