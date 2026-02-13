{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDistantObjectRequest@.
module ObjC.Foundation.NSDistantObjectRequest
  ( NSDistantObjectRequest
  , IsNSDistantObjectRequest(..)
  , replyWithException
  , invocation
  , connection
  , conversation
  , connectionSelector
  , conversationSelector
  , invocationSelector
  , replyWithExceptionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- replyWithException:@
replyWithException :: (IsNSDistantObjectRequest nsDistantObjectRequest, IsNSException exception) => nsDistantObjectRequest -> exception -> IO ()
replyWithException nsDistantObjectRequest exception =
  sendMessage nsDistantObjectRequest replyWithExceptionSelector (toNSException exception)

-- | @- invocation@
invocation :: IsNSDistantObjectRequest nsDistantObjectRequest => nsDistantObjectRequest -> IO (Id NSInvocation)
invocation nsDistantObjectRequest =
  sendMessage nsDistantObjectRequest invocationSelector

-- | @- connection@
connection :: IsNSDistantObjectRequest nsDistantObjectRequest => nsDistantObjectRequest -> IO (Id NSConnection)
connection nsDistantObjectRequest =
  sendMessage nsDistantObjectRequest connectionSelector

-- | @- conversation@
conversation :: IsNSDistantObjectRequest nsDistantObjectRequest => nsDistantObjectRequest -> IO RawId
conversation nsDistantObjectRequest =
  sendMessage nsDistantObjectRequest conversationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @replyWithException:@
replyWithExceptionSelector :: Selector '[Id NSException] ()
replyWithExceptionSelector = mkSelector "replyWithException:"

-- | @Selector@ for @invocation@
invocationSelector :: Selector '[] (Id NSInvocation)
invocationSelector = mkSelector "invocation"

-- | @Selector@ for @connection@
connectionSelector :: Selector '[] (Id NSConnection)
connectionSelector = mkSelector "connection"

-- | @Selector@ for @conversation@
conversationSelector :: Selector '[] RawId
conversationSelector = mkSelector "conversation"

