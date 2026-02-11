{-# LANGUAGE PatternSynonyms #-}
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
import ObjC.Foundation.Internal.Enums

-- | @- init@
init_ :: IsNSSpecifierTest nsSpecifierTest => nsSpecifierTest -> IO (Id NSSpecifierTest)
init_ nsSpecifierTest  =
  sendMsg nsSpecifierTest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSSpecifierTest nsSpecifierTest, IsNSCoder inCoder) => nsSpecifierTest -> inCoder -> IO (Id NSSpecifierTest)
initWithCoder nsSpecifierTest  inCoder =
withObjCPtr inCoder $ \raw_inCoder ->
    sendMsg nsSpecifierTest (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_inCoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithObjectSpecifier:comparisonOperator:testObject:@
initWithObjectSpecifier_comparisonOperator_testObject :: (IsNSSpecifierTest nsSpecifierTest, IsNSScriptObjectSpecifier obj1) => nsSpecifierTest -> obj1 -> NSTestComparisonOperation -> RawId -> IO (Id NSSpecifierTest)
initWithObjectSpecifier_comparisonOperator_testObject nsSpecifierTest  obj1 compOp obj2 =
withObjCPtr obj1 $ \raw_obj1 ->
    sendMsg nsSpecifierTest (mkSelector "initWithObjectSpecifier:comparisonOperator:testObject:") (retPtr retVoid) [argPtr (castPtr raw_obj1 :: Ptr ()), argCULong (coerce compOp), argPtr (castPtr (unRawId obj2) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithObjectSpecifier:comparisonOperator:testObject:@
initWithObjectSpecifier_comparisonOperator_testObjectSelector :: Selector
initWithObjectSpecifier_comparisonOperator_testObjectSelector = mkSelector "initWithObjectSpecifier:comparisonOperator:testObject:"

