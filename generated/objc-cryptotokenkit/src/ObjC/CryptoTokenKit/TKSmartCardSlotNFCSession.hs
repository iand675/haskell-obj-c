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
  , initSelector
  , updateWithMessage_errorSelector
  , endSessionSelector
  , slotNameSelector


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

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Clients should only receive the session
--
-- ObjC selector: @- init@
init_ :: IsTKSmartCardSlotNFCSession tkSmartCardSlotNFCSession => tkSmartCardSlotNFCSession -> IO (Id TKSmartCardSlotNFCSession)
init_ tkSmartCardSlotNFCSession  =
  sendMsg tkSmartCardSlotNFCSession (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
updateWithMessage_error tkSmartCardSlotNFCSession  message error_ =
withObjCPtr message $ \raw_message ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg tkSmartCardSlotNFCSession (mkSelector "updateWithMessage:error:") retCULong [argPtr (castPtr raw_message :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Ends the NFC slot session and dismisses the system-presented NFC UI (if present).
--
-- ObjC selector: @- endSession@
endSession :: IsTKSmartCardSlotNFCSession tkSmartCardSlotNFCSession => tkSmartCardSlotNFCSession -> IO ()
endSession tkSmartCardSlotNFCSession  =
  sendMsg tkSmartCardSlotNFCSession (mkSelector "endSession") retVoid []

-- | Smart card slot name of the NFC slot that was created together with this session.
--
-- ObjC selector: @- slotName@
slotName :: IsTKSmartCardSlotNFCSession tkSmartCardSlotNFCSession => tkSmartCardSlotNFCSession -> IO (Id NSString)
slotName tkSmartCardSlotNFCSession  =
  sendMsg tkSmartCardSlotNFCSession (mkSelector "slotName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @updateWithMessage:error:@
updateWithMessage_errorSelector :: Selector
updateWithMessage_errorSelector = mkSelector "updateWithMessage:error:"

-- | @Selector@ for @endSession@
endSessionSelector :: Selector
endSessionSelector = mkSelector "endSession"

-- | @Selector@ for @slotName@
slotNameSelector :: Selector
slotNameSelector = mkSelector "slotName"

