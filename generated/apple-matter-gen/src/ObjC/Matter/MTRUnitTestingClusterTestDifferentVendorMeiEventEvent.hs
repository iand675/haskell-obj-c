{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestDifferentVendorMeiEventEvent@.
module ObjC.Matter.MTRUnitTestingClusterTestDifferentVendorMeiEventEvent
  ( MTRUnitTestingClusterTestDifferentVendorMeiEventEvent
  , IsMTRUnitTestingClusterTestDifferentVendorMeiEventEvent(..)
  , arg1
  , setArg1
  , arg1Selector
  , setArg1Selector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- arg1@
arg1 :: IsMTRUnitTestingClusterTestDifferentVendorMeiEventEvent mtrUnitTestingClusterTestDifferentVendorMeiEventEvent => mtrUnitTestingClusterTestDifferentVendorMeiEventEvent -> IO (Id NSNumber)
arg1 mtrUnitTestingClusterTestDifferentVendorMeiEventEvent =
  sendMessage mtrUnitTestingClusterTestDifferentVendorMeiEventEvent arg1Selector

-- | @- setArg1:@
setArg1 :: (IsMTRUnitTestingClusterTestDifferentVendorMeiEventEvent mtrUnitTestingClusterTestDifferentVendorMeiEventEvent, IsNSNumber value) => mtrUnitTestingClusterTestDifferentVendorMeiEventEvent -> value -> IO ()
setArg1 mtrUnitTestingClusterTestDifferentVendorMeiEventEvent value =
  sendMessage mtrUnitTestingClusterTestDifferentVendorMeiEventEvent setArg1Selector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @arg1@
arg1Selector :: Selector '[] (Id NSNumber)
arg1Selector = mkSelector "arg1"

-- | @Selector@ for @setArg1:@
setArg1Selector :: Selector '[Id NSNumber] ()
setArg1Selector = mkSelector "setArg1:"

