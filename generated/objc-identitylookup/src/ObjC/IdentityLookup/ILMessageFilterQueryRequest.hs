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
  , initSelector
  , newSelector
  , senderSelector
  , messageBodySelector


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

import ObjC.IdentityLookup.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsILMessageFilterQueryRequest ilMessageFilterQueryRequest => ilMessageFilterQueryRequest -> IO (Id ILMessageFilterQueryRequest)
init_ ilMessageFilterQueryRequest  =
  sendMsg ilMessageFilterQueryRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id ILMessageFilterQueryRequest)
new  =
  do
    cls' <- getRequiredClass "ILMessageFilterQueryRequest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The sender of the message the receiver relates to.
--
-- ObjC selector: @- sender@
sender :: IsILMessageFilterQueryRequest ilMessageFilterQueryRequest => ilMessageFilterQueryRequest -> IO (Id NSString)
sender ilMessageFilterQueryRequest  =
  sendMsg ilMessageFilterQueryRequest (mkSelector "sender") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The body of the message the receiver relates to.
--
-- ObjC selector: @- messageBody@
messageBody :: IsILMessageFilterQueryRequest ilMessageFilterQueryRequest => ilMessageFilterQueryRequest -> IO (Id NSString)
messageBody ilMessageFilterQueryRequest  =
  sendMsg ilMessageFilterQueryRequest (mkSelector "messageBody") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @sender@
senderSelector :: Selector
senderSelector = mkSelector "sender"

-- | @Selector@ for @messageBody@
messageBodySelector :: Selector
messageBodySelector = mkSelector "messageBody"

