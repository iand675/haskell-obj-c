{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestEventEvent@.
module ObjC.Matter.MTRTestClusterClusterTestEventEvent
  ( MTRTestClusterClusterTestEventEvent
  , IsMTRTestClusterClusterTestEventEvent(..)
  , arg1
  , setArg1
  , arg2
  , setArg2
  , arg3
  , setArg3
  , arg4
  , setArg4
  , arg5
  , setArg5
  , arg6
  , setArg6
  , arg1Selector
  , arg2Selector
  , arg3Selector
  , arg4Selector
  , arg5Selector
  , arg6Selector
  , setArg1Selector
  , setArg2Selector
  , setArg3Selector
  , setArg4Selector
  , setArg5Selector
  , setArg6Selector


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
arg1 :: IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent => mtrTestClusterClusterTestEventEvent -> IO (Id NSNumber)
arg1 mtrTestClusterClusterTestEventEvent =
  sendMessage mtrTestClusterClusterTestEventEvent arg1Selector

-- | @- setArg1:@
setArg1 :: (IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent, IsNSNumber value) => mtrTestClusterClusterTestEventEvent -> value -> IO ()
setArg1 mtrTestClusterClusterTestEventEvent value =
  sendMessage mtrTestClusterClusterTestEventEvent setArg1Selector (toNSNumber value)

-- | @- arg2@
arg2 :: IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent => mtrTestClusterClusterTestEventEvent -> IO (Id NSNumber)
arg2 mtrTestClusterClusterTestEventEvent =
  sendMessage mtrTestClusterClusterTestEventEvent arg2Selector

-- | @- setArg2:@
setArg2 :: (IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent, IsNSNumber value) => mtrTestClusterClusterTestEventEvent -> value -> IO ()
setArg2 mtrTestClusterClusterTestEventEvent value =
  sendMessage mtrTestClusterClusterTestEventEvent setArg2Selector (toNSNumber value)

-- | @- arg3@
arg3 :: IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent => mtrTestClusterClusterTestEventEvent -> IO (Id NSNumber)
arg3 mtrTestClusterClusterTestEventEvent =
  sendMessage mtrTestClusterClusterTestEventEvent arg3Selector

-- | @- setArg3:@
setArg3 :: (IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent, IsNSNumber value) => mtrTestClusterClusterTestEventEvent -> value -> IO ()
setArg3 mtrTestClusterClusterTestEventEvent value =
  sendMessage mtrTestClusterClusterTestEventEvent setArg3Selector (toNSNumber value)

-- | @- arg4@
arg4 :: IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent => mtrTestClusterClusterTestEventEvent -> IO (Id MTRTestClusterClusterSimpleStruct)
arg4 mtrTestClusterClusterTestEventEvent =
  sendMessage mtrTestClusterClusterTestEventEvent arg4Selector

-- | @- setArg4:@
setArg4 :: (IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent, IsMTRTestClusterClusterSimpleStruct value) => mtrTestClusterClusterTestEventEvent -> value -> IO ()
setArg4 mtrTestClusterClusterTestEventEvent value =
  sendMessage mtrTestClusterClusterTestEventEvent setArg4Selector (toMTRTestClusterClusterSimpleStruct value)

-- | @- arg5@
arg5 :: IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent => mtrTestClusterClusterTestEventEvent -> IO (Id NSArray)
arg5 mtrTestClusterClusterTestEventEvent =
  sendMessage mtrTestClusterClusterTestEventEvent arg5Selector

-- | @- setArg5:@
setArg5 :: (IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent, IsNSArray value) => mtrTestClusterClusterTestEventEvent -> value -> IO ()
setArg5 mtrTestClusterClusterTestEventEvent value =
  sendMessage mtrTestClusterClusterTestEventEvent setArg5Selector (toNSArray value)

-- | @- arg6@
arg6 :: IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent => mtrTestClusterClusterTestEventEvent -> IO (Id NSArray)
arg6 mtrTestClusterClusterTestEventEvent =
  sendMessage mtrTestClusterClusterTestEventEvent arg6Selector

-- | @- setArg6:@
setArg6 :: (IsMTRTestClusterClusterTestEventEvent mtrTestClusterClusterTestEventEvent, IsNSArray value) => mtrTestClusterClusterTestEventEvent -> value -> IO ()
setArg6 mtrTestClusterClusterTestEventEvent value =
  sendMessage mtrTestClusterClusterTestEventEvent setArg6Selector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @arg1@
arg1Selector :: Selector '[] (Id NSNumber)
arg1Selector = mkSelector "arg1"

-- | @Selector@ for @setArg1:@
setArg1Selector :: Selector '[Id NSNumber] ()
setArg1Selector = mkSelector "setArg1:"

-- | @Selector@ for @arg2@
arg2Selector :: Selector '[] (Id NSNumber)
arg2Selector = mkSelector "arg2"

-- | @Selector@ for @setArg2:@
setArg2Selector :: Selector '[Id NSNumber] ()
setArg2Selector = mkSelector "setArg2:"

-- | @Selector@ for @arg3@
arg3Selector :: Selector '[] (Id NSNumber)
arg3Selector = mkSelector "arg3"

-- | @Selector@ for @setArg3:@
setArg3Selector :: Selector '[Id NSNumber] ()
setArg3Selector = mkSelector "setArg3:"

-- | @Selector@ for @arg4@
arg4Selector :: Selector '[] (Id MTRTestClusterClusterSimpleStruct)
arg4Selector = mkSelector "arg4"

-- | @Selector@ for @setArg4:@
setArg4Selector :: Selector '[Id MTRTestClusterClusterSimpleStruct] ()
setArg4Selector = mkSelector "setArg4:"

-- | @Selector@ for @arg5@
arg5Selector :: Selector '[] (Id NSArray)
arg5Selector = mkSelector "arg5"

-- | @Selector@ for @setArg5:@
setArg5Selector :: Selector '[Id NSArray] ()
setArg5Selector = mkSelector "setArg5:"

-- | @Selector@ for @arg6@
arg6Selector :: Selector '[] (Id NSArray)
arg6Selector = mkSelector "arg6"

-- | @Selector@ for @setArg6:@
setArg6Selector :: Selector '[Id NSArray] ()
setArg6Selector = mkSelector "setArg6:"

