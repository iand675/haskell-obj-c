{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INHangUpCallIntent@.
module ObjC.Intents.INHangUpCallIntent
  ( INHangUpCallIntent
  , IsINHangUpCallIntent(..)
  , initWithCallIdentifier
  , callIdentifier
  , initWithCallIdentifierSelector
  , callIdentifierSelector


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

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCallIdentifier:@
initWithCallIdentifier :: (IsINHangUpCallIntent inHangUpCallIntent, IsNSString callIdentifier) => inHangUpCallIntent -> callIdentifier -> IO (Id INHangUpCallIntent)
initWithCallIdentifier inHangUpCallIntent  callIdentifier =
withObjCPtr callIdentifier $ \raw_callIdentifier ->
    sendMsg inHangUpCallIntent (mkSelector "initWithCallIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_callIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- callIdentifier@
callIdentifier :: IsINHangUpCallIntent inHangUpCallIntent => inHangUpCallIntent -> IO (Id NSString)
callIdentifier inHangUpCallIntent  =
  sendMsg inHangUpCallIntent (mkSelector "callIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCallIdentifier:@
initWithCallIdentifierSelector :: Selector
initWithCallIdentifierSelector = mkSelector "initWithCallIdentifier:"

-- | @Selector@ for @callIdentifier@
callIdentifierSelector :: Selector
callIdentifierSelector = mkSelector "callIdentifier"

