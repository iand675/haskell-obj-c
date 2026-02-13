{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterDoubleNestedStructList@.
module ObjC.Matter.MTRUnitTestingClusterDoubleNestedStructList
  ( MTRUnitTestingClusterDoubleNestedStructList
  , IsMTRUnitTestingClusterDoubleNestedStructList(..)
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
a :: IsMTRUnitTestingClusterDoubleNestedStructList mtrUnitTestingClusterDoubleNestedStructList => mtrUnitTestingClusterDoubleNestedStructList -> IO (Id NSArray)
a mtrUnitTestingClusterDoubleNestedStructList =
  sendMessage mtrUnitTestingClusterDoubleNestedStructList aSelector

-- | @- setA:@
setA :: (IsMTRUnitTestingClusterDoubleNestedStructList mtrUnitTestingClusterDoubleNestedStructList, IsNSArray value) => mtrUnitTestingClusterDoubleNestedStructList -> value -> IO ()
setA mtrUnitTestingClusterDoubleNestedStructList value =
  sendMessage mtrUnitTestingClusterDoubleNestedStructList setASelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @a@
aSelector :: Selector '[] (Id NSArray)
aSelector = mkSelector "a"

-- | @Selector@ for @setA:@
setASelector :: Selector '[Id NSArray] ()
setASelector = mkSelector "setA:"

