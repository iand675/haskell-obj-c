{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterNestedStruct@.
module ObjC.Matter.MTRTestClusterClusterNestedStruct
  ( MTRTestClusterClusterNestedStruct
  , IsMTRTestClusterClusterNestedStruct(..)
  , a
  , setA
  , b
  , setB
  , c
  , setC
  , aSelector
  , bSelector
  , cSelector
  , setASelector
  , setBSelector
  , setCSelector


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
a :: IsMTRTestClusterClusterNestedStruct mtrTestClusterClusterNestedStruct => mtrTestClusterClusterNestedStruct -> IO (Id NSNumber)
a mtrTestClusterClusterNestedStruct =
  sendMessage mtrTestClusterClusterNestedStruct aSelector

-- | @- setA:@
setA :: (IsMTRTestClusterClusterNestedStruct mtrTestClusterClusterNestedStruct, IsNSNumber value) => mtrTestClusterClusterNestedStruct -> value -> IO ()
setA mtrTestClusterClusterNestedStruct value =
  sendMessage mtrTestClusterClusterNestedStruct setASelector (toNSNumber value)

-- | @- b@
b :: IsMTRTestClusterClusterNestedStruct mtrTestClusterClusterNestedStruct => mtrTestClusterClusterNestedStruct -> IO (Id NSNumber)
b mtrTestClusterClusterNestedStruct =
  sendMessage mtrTestClusterClusterNestedStruct bSelector

-- | @- setB:@
setB :: (IsMTRTestClusterClusterNestedStruct mtrTestClusterClusterNestedStruct, IsNSNumber value) => mtrTestClusterClusterNestedStruct -> value -> IO ()
setB mtrTestClusterClusterNestedStruct value =
  sendMessage mtrTestClusterClusterNestedStruct setBSelector (toNSNumber value)

-- | @- c@
c :: IsMTRTestClusterClusterNestedStruct mtrTestClusterClusterNestedStruct => mtrTestClusterClusterNestedStruct -> IO (Id MTRTestClusterClusterSimpleStruct)
c mtrTestClusterClusterNestedStruct =
  sendMessage mtrTestClusterClusterNestedStruct cSelector

-- | @- setC:@
setC :: (IsMTRTestClusterClusterNestedStruct mtrTestClusterClusterNestedStruct, IsMTRTestClusterClusterSimpleStruct value) => mtrTestClusterClusterNestedStruct -> value -> IO ()
setC mtrTestClusterClusterNestedStruct value =
  sendMessage mtrTestClusterClusterNestedStruct setCSelector (toMTRTestClusterClusterSimpleStruct value)

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

