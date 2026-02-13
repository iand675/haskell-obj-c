{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestListStructOctet@.
module ObjC.Matter.MTRTestClusterClusterTestListStructOctet
  ( MTRTestClusterClusterTestListStructOctet
  , IsMTRTestClusterClusterTestListStructOctet(..)
  , member1
  , setMember1
  , member2
  , setMember2
  , member1Selector
  , member2Selector
  , setMember1Selector
  , setMember2Selector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- member1@
member1 :: IsMTRTestClusterClusterTestListStructOctet mtrTestClusterClusterTestListStructOctet => mtrTestClusterClusterTestListStructOctet -> IO (Id NSNumber)
member1 mtrTestClusterClusterTestListStructOctet =
  sendMessage mtrTestClusterClusterTestListStructOctet member1Selector

-- | @- setMember1:@
setMember1 :: (IsMTRTestClusterClusterTestListStructOctet mtrTestClusterClusterTestListStructOctet, IsNSNumber value) => mtrTestClusterClusterTestListStructOctet -> value -> IO ()
setMember1 mtrTestClusterClusterTestListStructOctet value =
  sendMessage mtrTestClusterClusterTestListStructOctet setMember1Selector (toNSNumber value)

-- | @- member2@
member2 :: IsMTRTestClusterClusterTestListStructOctet mtrTestClusterClusterTestListStructOctet => mtrTestClusterClusterTestListStructOctet -> IO (Id NSData)
member2 mtrTestClusterClusterTestListStructOctet =
  sendMessage mtrTestClusterClusterTestListStructOctet member2Selector

-- | @- setMember2:@
setMember2 :: (IsMTRTestClusterClusterTestListStructOctet mtrTestClusterClusterTestListStructOctet, IsNSData value) => mtrTestClusterClusterTestListStructOctet -> value -> IO ()
setMember2 mtrTestClusterClusterTestListStructOctet value =
  sendMessage mtrTestClusterClusterTestListStructOctet setMember2Selector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @member1@
member1Selector :: Selector '[] (Id NSNumber)
member1Selector = mkSelector "member1"

-- | @Selector@ for @setMember1:@
setMember1Selector :: Selector '[Id NSNumber] ()
setMember1Selector = mkSelector "setMember1:"

-- | @Selector@ for @member2@
member2Selector :: Selector '[] (Id NSData)
member2Selector = mkSelector "member2"

-- | @Selector@ for @setMember2:@
setMember2Selector :: Selector '[Id NSData] ()
setMember2Selector = mkSelector "setMember2:"

