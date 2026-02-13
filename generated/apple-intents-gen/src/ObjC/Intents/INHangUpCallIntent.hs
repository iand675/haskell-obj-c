{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INHangUpCallIntent@.
module ObjC.Intents.INHangUpCallIntent
  ( INHangUpCallIntent
  , IsINHangUpCallIntent(..)
  , initWithCallIdentifier
  , callIdentifier
  , callIdentifierSelector
  , initWithCallIdentifierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCallIdentifier:@
initWithCallIdentifier :: (IsINHangUpCallIntent inHangUpCallIntent, IsNSString callIdentifier) => inHangUpCallIntent -> callIdentifier -> IO (Id INHangUpCallIntent)
initWithCallIdentifier inHangUpCallIntent callIdentifier =
  sendOwnedMessage inHangUpCallIntent initWithCallIdentifierSelector (toNSString callIdentifier)

-- | @- callIdentifier@
callIdentifier :: IsINHangUpCallIntent inHangUpCallIntent => inHangUpCallIntent -> IO (Id NSString)
callIdentifier inHangUpCallIntent =
  sendMessage inHangUpCallIntent callIdentifierSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCallIdentifier:@
initWithCallIdentifierSelector :: Selector '[Id NSString] (Id INHangUpCallIntent)
initWithCallIdentifierSelector = mkSelector "initWithCallIdentifier:"

-- | @Selector@ for @callIdentifier@
callIdentifierSelector :: Selector '[] (Id NSString)
callIdentifierSelector = mkSelector "callIdentifier"

