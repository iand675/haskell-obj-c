{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSScriptWhoseTest@.
module ObjC.Foundation.NSScriptWhoseTest
  ( NSScriptWhoseTest
  , IsNSScriptWhoseTest(..)
  , isTrue
  , init_
  , initWithCoder
  , isTrueSelector
  , initSelector
  , initWithCoderSelector


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

-- | @- isTrue@
isTrue :: IsNSScriptWhoseTest nsScriptWhoseTest => nsScriptWhoseTest -> IO Bool
isTrue nsScriptWhoseTest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsScriptWhoseTest (mkSelector "isTrue") retCULong []

-- | @- init@
init_ :: IsNSScriptWhoseTest nsScriptWhoseTest => nsScriptWhoseTest -> IO (Id NSScriptWhoseTest)
init_ nsScriptWhoseTest  =
  sendMsg nsScriptWhoseTest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSScriptWhoseTest nsScriptWhoseTest, IsNSCoder inCoder) => nsScriptWhoseTest -> inCoder -> IO (Id NSScriptWhoseTest)
initWithCoder nsScriptWhoseTest  inCoder =
withObjCPtr inCoder $ \raw_inCoder ->
    sendMsg nsScriptWhoseTest (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_inCoder :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isTrue@
isTrueSelector :: Selector
isTrueSelector = mkSelector "isTrue"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

