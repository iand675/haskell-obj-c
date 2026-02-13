{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSpecifierTest@.
module ObjC.Foundation.NSSpecifierTest
  ( NSSpecifierTest
  , IsNSSpecifierTest(..)
  , init_
  , initWithCoder
  , initWithObjectSpecifier_comparisonOperator_testObject
  , initSelector
  , initWithCoderSelector
  , initWithObjectSpecifier_comparisonOperator_testObjectSelector

  -- * Enum types
  , NSTestComparisonOperation(NSTestComparisonOperation)
  , pattern NSEqualToComparison
  , pattern NSLessThanOrEqualToComparison
  , pattern NSLessThanComparison
  , pattern NSGreaterThanOrEqualToComparison
  , pattern NSGreaterThanComparison
  , pattern NSBeginsWithComparison
  , pattern NSEndsWithComparison
  , pattern NSContainsComparison

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- init@
init_ :: IsNSSpecifierTest nsSpecifierTest => nsSpecifierTest -> IO (Id NSSpecifierTest)
init_ nsSpecifierTest =
  sendOwnedMessage nsSpecifierTest initSelector

-- | @- initWithCoder:@
initWithCoder :: (IsNSSpecifierTest nsSpecifierTest, IsNSCoder inCoder) => nsSpecifierTest -> inCoder -> IO (Id NSSpecifierTest)
initWithCoder nsSpecifierTest inCoder =
  sendOwnedMessage nsSpecifierTest initWithCoderSelector (toNSCoder inCoder)

-- | @- initWithObjectSpecifier:comparisonOperator:testObject:@
initWithObjectSpecifier_comparisonOperator_testObject :: (IsNSSpecifierTest nsSpecifierTest, IsNSScriptObjectSpecifier obj1) => nsSpecifierTest -> obj1 -> NSTestComparisonOperation -> RawId -> IO (Id NSSpecifierTest)
initWithObjectSpecifier_comparisonOperator_testObject nsSpecifierTest obj1 compOp obj2 =
  sendOwnedMessage nsSpecifierTest initWithObjectSpecifier_comparisonOperator_testObjectSelector (toNSScriptObjectSpecifier obj1) compOp obj2

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSSpecifierTest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSSpecifierTest)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithObjectSpecifier:comparisonOperator:testObject:@
initWithObjectSpecifier_comparisonOperator_testObjectSelector :: Selector '[Id NSScriptObjectSpecifier, NSTestComparisonOperation, RawId] (Id NSSpecifierTest)
initWithObjectSpecifier_comparisonOperator_testObjectSelector = mkSelector "initWithObjectSpecifier:comparisonOperator:testObject:"

