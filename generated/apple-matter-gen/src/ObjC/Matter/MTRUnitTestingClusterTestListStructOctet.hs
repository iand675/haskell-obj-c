{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterTestListStructOctet@.
module ObjC.Matter.MTRUnitTestingClusterTestListStructOctet
  ( MTRUnitTestingClusterTestListStructOctet
  , IsMTRUnitTestingClusterTestListStructOctet(..)
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
member1 :: IsMTRUnitTestingClusterTestListStructOctet mtrUnitTestingClusterTestListStructOctet => mtrUnitTestingClusterTestListStructOctet -> IO (Id NSNumber)
member1 mtrUnitTestingClusterTestListStructOctet =
  sendMessage mtrUnitTestingClusterTestListStructOctet member1Selector

-- | @- setMember1:@
setMember1 :: (IsMTRUnitTestingClusterTestListStructOctet mtrUnitTestingClusterTestListStructOctet, IsNSNumber value) => mtrUnitTestingClusterTestListStructOctet -> value -> IO ()
setMember1 mtrUnitTestingClusterTestListStructOctet value =
  sendMessage mtrUnitTestingClusterTestListStructOctet setMember1Selector (toNSNumber value)

-- | @- member2@
member2 :: IsMTRUnitTestingClusterTestListStructOctet mtrUnitTestingClusterTestListStructOctet => mtrUnitTestingClusterTestListStructOctet -> IO (Id NSData)
member2 mtrUnitTestingClusterTestListStructOctet =
  sendMessage mtrUnitTestingClusterTestListStructOctet member2Selector

-- | @- setMember2:@
setMember2 :: (IsMTRUnitTestingClusterTestListStructOctet mtrUnitTestingClusterTestListStructOctet, IsNSData value) => mtrUnitTestingClusterTestListStructOctet -> value -> IO ()
setMember2 mtrUnitTestingClusterTestListStructOctet value =
  sendMessage mtrUnitTestingClusterTestListStructOctet setMember2Selector (toNSData value)

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

