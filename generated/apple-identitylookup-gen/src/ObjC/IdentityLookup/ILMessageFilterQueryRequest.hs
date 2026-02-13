{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request to query a MessageFilter extension about how to interpret a received message.
--
-- Generated bindings for @ILMessageFilterQueryRequest@.
module ObjC.IdentityLookup.ILMessageFilterQueryRequest
  ( ILMessageFilterQueryRequest
  , IsILMessageFilterQueryRequest(..)
  , init_
  , new
  , sender
  , messageBody
  , receiverISOCountryCode
  , initSelector
  , messageBodySelector
  , newSelector
  , receiverISOCountryCodeSelector
  , senderSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.IdentityLookup.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsILMessageFilterQueryRequest ilMessageFilterQueryRequest => ilMessageFilterQueryRequest -> IO (Id ILMessageFilterQueryRequest)
init_ ilMessageFilterQueryRequest =
  sendOwnedMessage ilMessageFilterQueryRequest initSelector

-- | @+ new@
new :: IO (Id ILMessageFilterQueryRequest)
new  =
  do
    cls' <- getRequiredClass "ILMessageFilterQueryRequest"
    sendOwnedClassMessage cls' newSelector

-- | The sender of the message the receiver relates to.
--
-- ObjC selector: @- sender@
sender :: IsILMessageFilterQueryRequest ilMessageFilterQueryRequest => ilMessageFilterQueryRequest -> IO (Id NSString)
sender ilMessageFilterQueryRequest =
  sendMessage ilMessageFilterQueryRequest senderSelector

-- | The body of the message the receiver relates to.
--
-- ObjC selector: @- messageBody@
messageBody :: IsILMessageFilterQueryRequest ilMessageFilterQueryRequest => ilMessageFilterQueryRequest -> IO (Id NSString)
messageBody ilMessageFilterQueryRequest =
  sendMessage ilMessageFilterQueryRequest messageBodySelector

-- | The ISO Country Code of the receiving phone number, in format specified by the ISO 3166-2 standard
--
-- ObjC selector: @- receiverISOCountryCode@
receiverISOCountryCode :: IsILMessageFilterQueryRequest ilMessageFilterQueryRequest => ilMessageFilterQueryRequest -> IO (Id NSString)
receiverISOCountryCode ilMessageFilterQueryRequest =
  sendMessage ilMessageFilterQueryRequest receiverISOCountryCodeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id ILMessageFilterQueryRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id ILMessageFilterQueryRequest)
newSelector = mkSelector "new"

-- | @Selector@ for @sender@
senderSelector :: Selector '[] (Id NSString)
senderSelector = mkSelector "sender"

-- | @Selector@ for @messageBody@
messageBodySelector :: Selector '[] (Id NSString)
messageBodySelector = mkSelector "messageBody"

-- | @Selector@ for @receiverISOCountryCode@
receiverISOCountryCodeSelector :: Selector '[] (Id NSString)
receiverISOCountryCodeSelector = mkSelector "receiverISOCountryCode"

