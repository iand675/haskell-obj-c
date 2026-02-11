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
  , initOrTestWithTestsSelector
  , initNotTestWithTestSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- initAndTestWithTests:@
initAndTestWithTests :: (IsNSLogicalTest nsLogicalTest, IsNSArray subTests) => nsLogicalTest -> subTests -> IO (Id NSLogicalTest)
initAndTestWithTests nsLogicalTest  subTests =
withObjCPtr subTests $ \raw_subTests ->
    sendMsg nsLogicalTest (mkSelector "initAndTestWithTests:") (retPtr retVoid) [argPtr (castPtr raw_subTests :: Ptr ())] >>= ownedObject . castPtr

-- | @- initOrTestWithTests:@
initOrTestWithTests :: (IsNSLogicalTest nsLogicalTest, IsNSArray subTests) => nsLogicalTest -> subTests -> IO (Id NSLogicalTest)
initOrTestWithTests nsLogicalTest  subTests =
withObjCPtr subTests $ \raw_subTests ->
    sendMsg nsLogicalTest (mkSelector "initOrTestWithTests:") (retPtr retVoid) [argPtr (castPtr raw_subTests :: Ptr ())] >>= ownedObject . castPtr

-- | @- initNotTestWithTest:@
initNotTestWithTest :: (IsNSLogicalTest nsLogicalTest, IsNSScriptWhoseTest subTest) => nsLogicalTest -> subTest -> IO (Id NSLogicalTest)
initNotTestWithTest nsLogicalTest  subTest =
withObjCPtr subTest $ \raw_subTest ->
    sendMsg nsLogicalTest (mkSelector "initNotTestWithTest:") (retPtr retVoid) [argPtr (castPtr raw_subTest :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initAndTestWithTests:@
initAndTestWithTestsSelector :: Selector
initAndTestWithTestsSelector = mkSelector "initAndTestWithTests:"

-- | @Selector@ for @initOrTestWithTests:@
initOrTestWithTestsSelector :: Selector
initOrTestWithTestsSelector = mkSelector "initOrTestWithTests:"

-- | @Selector@ for @initNotTestWithTest:@
initNotTestWithTestSelector :: Selector
initNotTestWithTestSelector = mkSelector "initNotTestWithTest:"

