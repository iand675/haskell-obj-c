{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestEventEvent@.
module ObjC.Matter.MTRUnitTestingClusterTestEventEvent
  ( MTRUnitTestingClusterTestEventEvent
  , IsMTRUnitTestingClusterTestEventEvent(..)
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
arg1 :: IsMTRUnitTestingClusterTestEventEvent mtrUnitTestingClusterTestEventEvent => mtrUnitTestingClusterTestEventEvent -> IO (Id NSNumber)
arg1 mtrUnitTestingClusterTestEventEvent =
  sendMessage mtrUnitTestingClusterTestEventEvent arg1Selector

-- | @- setArg1:@
setArg1 :: (IsMTRUnitTestingClusterTestEventEvent mtrUnitTestingClusterTestEventEvent, IsNSNumber value) => mtrUnitTestingClusterTestEventEvent -> value -> IO ()
setArg1 mtrUnitTestingClusterTestEventEvent value =
  sendMessage mtrUnitTestingClusterTestEventEvent setArg1Selector (toNSNumber value)

-- | @- arg2@
arg2 :: IsMTRUnitTestingClusterTestEventEvent mtrUnitTestingClusterTestEventEvent => mtrUnitTestingClusterTestEventEvent -> IO (Id NSNumber)
arg2 mtrUnitTestingClusterTestEventEvent =
  sendMessage mtrUnitTestingClusterTestEventEvent arg2Selector

-- | @- setArg2:@
setArg2 :: (IsMTRUnitTestingClusterTestEventEvent mtrUnitTestingClusterTestEventEvent, IsNSNumber value) => mtrUnitTestingClusterTestEventEvent -> value -> IO ()
setArg2 mtrUnitTestingClusterTestEventEvent value =
  sendMessage mtrUnitTestingClusterTestEventEvent setArg2Selector (toNSNumber value)

-- | @- arg3@
arg3 :: IsMTRUnitTestingClusterTestEventEvent mtrUnitTestingClusterTestEventEvent => mtrUnitTestingClusterTestEventEvent -> IO (Id NSNumber)
arg3 mtrUnitTestingClusterTestEventEvent =
  sendMessage mtrUnitTestingClusterTestEventEvent arg3Selector

-- | @- setArg3:@
setArg3 :: (IsMTRUnitTestingClusterTestEventEvent mtrUnitTestingClusterTestEventEvent, IsNSNumber value) => mtrUnitTestingClusterTestEventEvent -> value -> IO ()
setArg3 mtrUnitTestingClusterTestEventEvent value =
  sendMessage mtrUnitTestingClusterTestEventEvent setArg3Selector (toNSNumber value)

-- | @- arg4@
arg4 :: IsMTRUnitTestingClusterTestEventEvent mtrUnitTestingClusterTestEventEvent => mtrUnitTestingClusterTestEventEvent -> IO (Id MTRUnitTestingClusterSimpleStruct)
arg4 mtrUnitTestingClusterTestEventEvent =
  sendMessage mtrUnitTestingClusterTestEventEvent arg4Selector

-- | @- setArg4:@
setArg4 :: (IsMTRUnitTestingClusterTestEventEvent mtrUnitTestingClusterTestEventEvent, IsMTRUnitTestingClusterSimpleStruct value) => mtrUnitTestingClusterTestEventEvent -> value -> IO ()
setArg4 mtrUnitTestingClusterTestEventEvent value =
  sendMessage mtrUnitTestingClusterTestEventEvent setArg4Selector (toMTRUnitTestingClusterSimpleStruct value)

-- | @- arg5@
arg5 :: IsMTRUnitTestingClusterTestEventEvent mtrUnitTestingClusterTestEventEvent => mtrUnitTestingClusterTestEventEvent -> IO (Id NSArray)
arg5 mtrUnitTestingClusterTestEventEvent =
  sendMessage mtrUnitTestingClusterTestEventEvent arg5Selector

-- | @- setArg5:@
setArg5 :: (IsMTRUnitTestingClusterTestEventEvent mtrUnitTestingClusterTestEventEvent, IsNSArray value) => mtrUnitTestingClusterTestEventEvent -> value -> IO ()
setArg5 mtrUnitTestingClusterTestEventEvent value =
  sendMessage mtrUnitTestingClusterTestEventEvent setArg5Selector (toNSArray value)

-- | @- arg6@
arg6 :: IsMTRUnitTestingClusterTestEventEvent mtrUnitTestingClusterTestEventEvent => mtrUnitTestingClusterTestEventEvent -> IO (Id NSArray)
arg6 mtrUnitTestingClusterTestEventEvent =
  sendMessage mtrUnitTestingClusterTestEventEvent arg6Selector

-- | @- setArg6:@
setArg6 :: (IsMTRUnitTestingClusterTestEventEvent mtrUnitTestingClusterTestEventEvent, IsNSArray value) => mtrUnitTestingClusterTestEventEvent -> value -> IO ()
setArg6 mtrUnitTestingClusterTestEventEvent value =
  sendMessage mtrUnitTestingClusterTestEventEvent setArg6Selector (toNSArray value)

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
arg4Selector :: Selector '[] (Id MTRUnitTestingClusterSimpleStruct)
arg4Selector = mkSelector "arg4"

-- | @Selector@ for @setArg4:@
setArg4Selector :: Selector '[Id MTRUnitTestingClusterSimpleStruct] ()
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

