{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents pool of SmartCard reader slots.
--
-- Generated bindings for @TKSmartCardSlotManager@.
module ObjC.CryptoTokenKit.TKSmartCardSlotManager
  ( TKSmartCardSlotManager
  , IsTKSmartCardSlotManager(..)
  , getSlotWithName_reply
  , slotNamed
  , createNFCSlotWithMessage_completion
  , isNFCSupported
  , defaultManager
  , slotNames
  , getSlotWithName_replySelector
  , slotNamedSelector
  , createNFCSlotWithMessage_completionSelector
  , isNFCSupportedSelector
  , defaultManagerSelector
  , slotNamesSelector


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

-- | Instantiates smartcard reader slot of specified name.  If specified name is not registered, reports nil.
--
-- ObjC selector: @- getSlotWithName:reply:@
getSlotWithName_reply :: (IsTKSmartCardSlotManager tkSmartCardSlotManager, IsNSString name) => tkSmartCardSlotManager -> name -> Ptr () -> IO ()
getSlotWithName_reply tkSmartCardSlotManager  name reply =
withObjCPtr name $ \raw_name ->
    sendMsg tkSmartCardSlotManager (mkSelector "getSlotWithName:reply:") retVoid [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr reply :: Ptr ())]

-- | Gets SmartCard reader slot with specified name.  If reader slot with this name does not exist, returns nil.
--
-- ObjC selector: @- slotNamed:@
slotNamed :: (IsTKSmartCardSlotManager tkSmartCardSlotManager, IsNSString name) => tkSmartCardSlotManager -> name -> IO (Id TKSmartCardSlot)
slotNamed tkSmartCardSlotManager  name =
withObjCPtr name $ \raw_name ->
    sendMsg tkSmartCardSlotManager (mkSelector "slotNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | Creates an NFC smart card slot using the device's hardware and presents a system UI.
--
-- @message@ — Message shown in the system-presented UI
--
-- @completion@ — Completion handler which returns the NFC session of the created slot or an error on failure.                   If an NFC slot already exists and current caller is not the initial creator @TKErrorCodeObjectNotFound@ error is returned.
--
-- To finish the NFC session and dismiss the system-presented UI use @TKSmartCardSlotNFCSession.endSession@.
--
-- Warning: Caller requires @com.apple.developer.nfc.readersession.iso7816.select-identifiers@ Info.plist record which specifies application identifiers of the NFC cards
--
-- https://developer.apple.com/documentation/bundleresources/information-property-list/com.apple.developer.nfc.readersession.iso7816.select-identifiers
--
-- ObjC selector: @- createNFCSlotWithMessage:completion:@
createNFCSlotWithMessage_completion :: (IsTKSmartCardSlotManager tkSmartCardSlotManager, IsNSString message) => tkSmartCardSlotManager -> message -> Ptr () -> IO ()
createNFCSlotWithMessage_completion tkSmartCardSlotManager  message completion =
withObjCPtr message $ \raw_message ->
    sendMsg tkSmartCardSlotManager (mkSelector "createNFCSlotWithMessage:completion:") retVoid [argPtr (castPtr raw_message :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Determines whether NFC (Near Field Communication) is supported on this device.
--
-- Returns: @YES@ if NFC is supported and available for use, NO otherwise.
--
-- ObjC selector: @- isNFCSupported@
isNFCSupported :: IsTKSmartCardSlotManager tkSmartCardSlotManager => tkSmartCardSlotManager -> IO Bool
isNFCSupported tkSmartCardSlotManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg tkSmartCardSlotManager (mkSelector "isNFCSupported") retCULong []

-- | Global pool of SmartCard reader slots. macOS: Note that defaultManager instance is accessible only if the calling application has 'com.apple.security.smartcard' entitlement set to Boolean:YES.  If the calling application does not have this entitlement, defaultManager is always set to nil. iOS: The defaultManager instance is always accessible.
--
-- ObjC selector: @+ defaultManager@
defaultManager :: IO (Id TKSmartCardSlotManager)
defaultManager  =
  do
    cls' <- getRequiredClass "TKSmartCardSlotManager"
    sendClassMsg cls' (mkSelector "defaultManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Array of currently known slots in the system.  Slots are identified by NSString name instances.  Use KVO to be notified about slots arrivals and removals.
--
-- ObjC selector: @- slotNames@
slotNames :: IsTKSmartCardSlotManager tkSmartCardSlotManager => tkSmartCardSlotManager -> IO (Id NSArray)
slotNames tkSmartCardSlotManager  =
  sendMsg tkSmartCardSlotManager (mkSelector "slotNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getSlotWithName:reply:@
getSlotWithName_replySelector :: Selector
getSlotWithName_replySelector = mkSelector "getSlotWithName:reply:"

-- | @Selector@ for @slotNamed:@
slotNamedSelector :: Selector
slotNamedSelector = mkSelector "slotNamed:"

-- | @Selector@ for @createNFCSlotWithMessage:completion:@
createNFCSlotWithMessage_completionSelector :: Selector
createNFCSlotWithMessage_completionSelector = mkSelector "createNFCSlotWithMessage:completion:"

-- | @Selector@ for @isNFCSupported@
isNFCSupportedSelector :: Selector
isNFCSupportedSelector = mkSelector "isNFCSupported"

-- | @Selector@ for @defaultManager@
defaultManagerSelector :: Selector
defaultManagerSelector = mkSelector "defaultManager"

-- | @Selector@ for @slotNames@
slotNamesSelector :: Selector
slotNamesSelector = mkSelector "slotNames"

