{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterDoubleNestedStructList@.
module ObjC.Matter.MTRTestClusterClusterDoubleNestedStructList
  ( MTRTestClusterClusterDoubleNestedStructList
  , IsMTRTestClusterClusterDoubleNestedStructList(..)
  , a
  , setA
  , aSelector
  , setASelector


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
a :: IsMTRTestClusterClusterDoubleNestedStructList mtrTestClusterClusterDoubleNestedStructList => mtrTestClusterClusterDoubleNestedStructList -> IO (Id NSArray)
a mtrTestClusterClusterDoubleNestedStructList =
  sendMessage mtrTestClusterClusterDoubleNestedStructList aSelector

-- | @- setA:@
setA :: (IsMTRTestClusterClusterDoubleNestedStructList mtrTestClusterClusterDoubleNestedStructList, IsNSArray value) => mtrTestClusterClusterDoubleNestedStructList -> value -> IO ()
setA mtrTestClusterClusterDoubleNestedStructList value =
  sendMessage mtrTestClusterClusterDoubleNestedStructList setASelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @a@
aSelector :: Selector '[] (Id NSArray)
aSelector = mkSelector "a"

-- | @Selector@ for @setA:@
setASelector :: Selector '[Id NSArray] ()
setASelector = mkSelector "setA:"

