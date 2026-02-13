{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSLogicalTest@.
module ObjC.Foundation.NSLogicalTest
  ( NSLogicalTest
  , IsNSLogicalTest(..)
  , initAndTestWithTests
  , initOrTestWithTests
  , initNotTestWithTest
  , initAndTestWithTestsSelector
  , initNotTestWithTestSelector
  , initOrTestWithTestsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initAndTestWithTests:@
initAndTestWithTests :: (IsNSLogicalTest nsLogicalTest, IsNSArray subTests) => nsLogicalTest -> subTests -> IO (Id NSLogicalTest)
initAndTestWithTests nsLogicalTest subTests =
  sendOwnedMessage nsLogicalTest initAndTestWithTestsSelector (toNSArray subTests)

-- | @- initOrTestWithTests:@
initOrTestWithTests :: (IsNSLogicalTest nsLogicalTest, IsNSArray subTests) => nsLogicalTest -> subTests -> IO (Id NSLogicalTest)
initOrTestWithTests nsLogicalTest subTests =
  sendOwnedMessage nsLogicalTest initOrTestWithTestsSelector (toNSArray subTests)

-- | @- initNotTestWithTest:@
initNotTestWithTest :: (IsNSLogicalTest nsLogicalTest, IsNSScriptWhoseTest subTest) => nsLogicalTest -> subTest -> IO (Id NSLogicalTest)
initNotTestWithTest nsLogicalTest subTest =
  sendOwnedMessage nsLogicalTest initNotTestWithTestSelector (toNSScriptWhoseTest subTest)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initAndTestWithTests:@
initAndTestWithTestsSelector :: Selector '[Id NSArray] (Id NSLogicalTest)
initAndTestWithTestsSelector = mkSelector "initAndTestWithTests:"

-- | @Selector@ for @initOrTestWithTests:@
initOrTestWithTestsSelector :: Selector '[Id NSArray] (Id NSLogicalTest)
initOrTestWithTestsSelector = mkSelector "initOrTestWithTests:"

-- | @Selector@ for @initNotTestWithTest:@
initNotTestWithTestSelector :: Selector '[Id NSScriptWhoseTest] (Id NSLogicalTest)
initNotTestWithTestSelector = mkSelector "initNotTestWithTest:"

