{-# LANGUAGE DataKinds #-}
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
  , initSelector
  , initWithCoderSelector
  , isTrueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- isTrue@
isTrue :: IsNSScriptWhoseTest nsScriptWhoseTest => nsScriptWhoseTest -> IO Bool
isTrue nsScriptWhoseTest =
  sendMessage nsScriptWhoseTest isTrueSelector

-- | @- init@
init_ :: IsNSScriptWhoseTest nsScriptWhoseTest => nsScriptWhoseTest -> IO (Id NSScriptWhoseTest)
init_ nsScriptWhoseTest =
  sendOwnedMessage nsScriptWhoseTest initSelector

-- | @- initWithCoder:@
initWithCoder :: (IsNSScriptWhoseTest nsScriptWhoseTest, IsNSCoder inCoder) => nsScriptWhoseTest -> inCoder -> IO (Id NSScriptWhoseTest)
initWithCoder nsScriptWhoseTest inCoder =
  sendOwnedMessage nsScriptWhoseTest initWithCoderSelector (toNSCoder inCoder)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isTrue@
isTrueSelector :: Selector '[] Bool
isTrueSelector = mkSelector "isTrue"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSScriptWhoseTest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSScriptWhoseTest)
initWithCoderSelector = mkSelector "initWithCoder:"

