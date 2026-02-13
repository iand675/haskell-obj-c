{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NFC session that's related to NFC smart card slot which was created.
--
-- Lifetime of this session object is tied to the NFC smart card slot lifetime             and once the NFC slot disappears (eg. after a user cancellation, calling end session, or an NFC timeout)             the functions will start to fail and return @TKErrorCodeObjectNotFound@ error.
--
-- Generated bindings for @TKSmartCardSlotNFCSession@.
module ObjC.CryptoTokenKit.TKSmartCardSlotNFCSession
  ( TKSmartCardSlotNFCSession
  , IsTKSmartCardSlotNFCSession(..)
  , init_
  , updateWithMessage_error
  , endSession
  , slotName
  , endSessionSelector
  , initSelector
  , slotNameSelector
  , updateWithMessage_errorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Clients should only receive the session
--
-- ObjC selector: @- init@
init_ :: IsTKSmartCardSlotNFCSession tkSmartCardSlotNFCSession => tkSmartCardSlotNFCSession -> IO (Id TKSmartCardSlotNFCSession)
init_ tkSmartCardSlotNFCSession =
  sendOwnedMessage tkSmartCardSlotNFCSession initSelector

-- | Updates the message of the system-presented NFC UI.
--
-- @message@ — Message that should be displayed
--
-- @error@ — Specific error describing why the operation failed
--
-- Returns: Returns @YES@ if the alert message was updated, @NO@ if an error occured.
--
-- ObjC selector: @- updateWithMessage:error:@
updateWithMessage_error :: (IsTKSmartCardSlotNFCSession tkSmartCardSlotNFCSession, IsNSString message, IsNSError error_) => tkSmartCardSlotNFCSession -> message -> error_ -> IO Bool
updateWithMessage_error tkSmartCardSlotNFCSession message error_ =
  sendMessage tkSmartCardSlotNFCSession updateWithMessage_errorSelector (toNSString message) (toNSError error_)

-- | Ends the NFC slot session and dismisses the system-presented NFC UI (if present).
--
-- ObjC selector: @- endSession@
endSession :: IsTKSmartCardSlotNFCSession tkSmartCardSlotNFCSession => tkSmartCardSlotNFCSession -> IO ()
endSession tkSmartCardSlotNFCSession =
  sendMessage tkSmartCardSlotNFCSession endSessionSelector

-- | Smart card slot name of the NFC slot that was created together with this session.
--
-- ObjC selector: @- slotName@
slotName :: IsTKSmartCardSlotNFCSession tkSmartCardSlotNFCSession => tkSmartCardSlotNFCSession -> IO (Id NSString)
slotName tkSmartCardSlotNFCSession =
  sendMessage tkSmartCardSlotNFCSession slotNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id TKSmartCardSlotNFCSession)
initSelector = mkSelector "init"

-- | @Selector@ for @updateWithMessage:error:@
updateWithMessage_errorSelector :: Selector '[Id NSString, Id NSError] Bool
updateWithMessage_errorSelector = mkSelector "updateWithMessage:error:"

-- | @Selector@ for @endSession@
endSessionSelector :: Selector '[] ()
endSessionSelector = mkSelector "endSession"

-- | @Selector@ for @slotName@
slotNameSelector :: Selector '[] (Id NSString)
slotNameSelector = mkSelector "slotName"

