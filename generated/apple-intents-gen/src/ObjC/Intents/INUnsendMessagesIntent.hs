{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithMessageIdentifiers:@
initWithMessageIdentifiers :: (IsINUnsendMessagesIntent inUnsendMessagesIntent, IsNSArray messageIdentifiers) => inUnsendMessagesIntent -> messageIdentifiers -> IO (Id INUnsendMessagesIntent)
initWithMessageIdentifiers inUnsendMessagesIntent messageIdentifiers =
  sendOwnedMessage inUnsendMessagesIntent initWithMessageIdentifiersSelector (toNSArray messageIdentifiers)

-- | @- messageIdentifiers@
messageIdentifiers :: IsINUnsendMessagesIntent inUnsendMessagesIntent => inUnsendMessagesIntent -> IO (Id NSArray)
messageIdentifiers inUnsendMessagesIntent =
  sendMessage inUnsendMessagesIntent messageIdentifiersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMessageIdentifiers:@
initWithMessageIdentifiersSelector :: Selector '[Id NSArray] (Id INUnsendMessagesIntent)
initWithMessageIdentifiersSelector = mkSelector "initWithMessageIdentifiers:"

-- | @Selector@ for @messageIdentifiers@
messageIdentifiersSelector :: Selector '[] (Id NSArray)
messageIdentifiersSelector = mkSelector "messageIdentifiers"

