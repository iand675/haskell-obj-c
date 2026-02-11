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
  , replyWithExceptionSelector
  , invocationSelector
  , connectionSelector
  , conversationSelector


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

-- | @- replyWithException:@
replyWithException :: (IsNSDistantObjectRequest nsDistantObjectRequest, IsNSException exception) => nsDistantObjectRequest -> exception -> IO ()
replyWithException nsDistantObjectRequest  exception =
withObjCPtr exception $ \raw_exception ->
    sendMsg nsDistantObjectRequest (mkSelector "replyWithException:") retVoid [argPtr (castPtr raw_exception :: Ptr ())]

-- | @- invocation@
invocation :: IsNSDistantObjectRequest nsDistantObjectRequest => nsDistantObjectRequest -> IO (Id NSInvocation)
invocation nsDistantObjectRequest  =
  sendMsg nsDistantObjectRequest (mkSelector "invocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- connection@
connection :: IsNSDistantObjectRequest nsDistantObjectRequest => nsDistantObjectRequest -> IO (Id NSConnection)
connection nsDistantObjectRequest  =
  sendMsg nsDistantObjectRequest (mkSelector "connection") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- conversation@
conversation :: IsNSDistantObjectRequest nsDistantObjectRequest => nsDistantObjectRequest -> IO RawId
conversation nsDistantObjectRequest  =
  fmap (RawId . castPtr) $ sendMsg nsDistantObjectRequest (mkSelector "conversation") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @replyWithException:@
replyWithExceptionSelector :: Selector
replyWithExceptionSelector = mkSelector "replyWithException:"

-- | @Selector@ for @invocation@
invocationSelector :: Selector
invocationSelector = mkSelector "invocation"

-- | @Selector@ for @connection@
connectionSelector :: Selector
connectionSelector = mkSelector "connection"

-- | @Selector@ for @conversation@
conversationSelector :: Selector
conversationSelector = mkSelector "conversation"

