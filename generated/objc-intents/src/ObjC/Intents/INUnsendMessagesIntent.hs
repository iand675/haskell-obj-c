{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INUnsendMessagesIntent@.
module ObjC.Intents.INUnsendMessagesIntent
  ( INUnsendMessagesIntent
  , IsINUnsendMessagesIntent(..)
  , initWithMessageIdentifiers
  , messageIdentifiers
  , initWithMessageIdentifiersSelector
  , messageIdentifiersSelector


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

-- | @- initWithMessageIdentifiers:@
initWithMessageIdentifiers :: (IsINUnsendMessagesIntent inUnsendMessagesIntent, IsNSArray messageIdentifiers) => inUnsendMessagesIntent -> messageIdentifiers -> IO (Id INUnsendMessagesIntent)
initWithMessageIdentifiers inUnsendMessagesIntent  messageIdentifiers =
withObjCPtr messageIdentifiers $ \raw_messageIdentifiers ->
    sendMsg inUnsendMessagesIntent (mkSelector "initWithMessageIdentifiers:") (retPtr retVoid) [argPtr (castPtr raw_messageIdentifiers :: Ptr ())] >>= ownedObject . castPtr

-- | @- messageIdentifiers@
messageIdentifiers :: IsINUnsendMessagesIntent inUnsendMessagesIntent => inUnsendMessagesIntent -> IO (Id NSArray)
messageIdentifiers inUnsendMessagesIntent  =
  sendMsg inUnsendMessagesIntent (mkSelector "messageIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMessageIdentifiers:@
initWithMessageIdentifiersSelector :: Selector
initWithMessageIdentifiersSelector = mkSelector "initWithMessageIdentifiers:"

-- | @Selector@ for @messageIdentifiers@
messageIdentifiersSelector :: Selector
messageIdentifiersSelector = mkSelector "messageIdentifiers"

