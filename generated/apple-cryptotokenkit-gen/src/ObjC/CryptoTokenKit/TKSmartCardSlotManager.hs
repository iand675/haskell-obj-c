{-# LANGUAGE DataKinds #-}
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
  , createNFCSlotWithMessage_completionSelector
  , defaultManagerSelector
  , getSlotWithName_replySelector
  , isNFCSupportedSelector
  , slotNamedSelector
  , slotNamesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Instantiates smartcard reader slot of specified name.  If specified name is not registered, reports nil.
--
-- ObjC selector: @- getSlotWithName:reply:@
getSlotWithName_reply :: (IsTKSmartCardSlotManager tkSmartCardSlotManager, IsNSString name) => tkSmartCardSlotManager -> name -> Ptr () -> IO ()
getSlotWithName_reply tkSmartCardSlotManager name reply =
  sendMessage tkSmartCardSlotManager getSlotWithName_replySelector (toNSString name) reply

-- | Gets SmartCard reader slot with specified name.  If reader slot with this name does not exist, returns nil.
--
-- ObjC selector: @- slotNamed:@
slotNamed :: (IsTKSmartCardSlotManager tkSmartCardSlotManager, IsNSString name) => tkSmartCardSlotManager -> name -> IO (Id TKSmartCardSlot)
slotNamed tkSmartCardSlotManager name =
  sendMessage tkSmartCardSlotManager slotNamedSelector (toNSString name)

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
createNFCSlotWithMessage_completion tkSmartCardSlotManager message completion =
  sendMessage tkSmartCardSlotManager createNFCSlotWithMessage_completionSelector (toNSString message) completion

-- | Determines whether NFC (Near Field Communication) is supported on this device.
--
-- Returns: @YES@ if NFC is supported and available for use, NO otherwise.
--
-- ObjC selector: @- isNFCSupported@
isNFCSupported :: IsTKSmartCardSlotManager tkSmartCardSlotManager => tkSmartCardSlotManager -> IO Bool
isNFCSupported tkSmartCardSlotManager =
  sendMessage tkSmartCardSlotManager isNFCSupportedSelector

-- | Global pool of SmartCard reader slots. macOS: Note that defaultManager instance is accessible only if the calling application has 'com.apple.security.smartcard' entitlement set to Boolean:YES.  If the calling application does not have this entitlement, defaultManager is always set to nil. iOS: The defaultManager instance is always accessible.
--
-- ObjC selector: @+ defaultManager@
defaultManager :: IO (Id TKSmartCardSlotManager)
defaultManager  =
  do
    cls' <- getRequiredClass "TKSmartCardSlotManager"
    sendClassMessage cls' defaultManagerSelector

-- | Array of currently known slots in the system.  Slots are identified by NSString name instances.  Use KVO to be notified about slots arrivals and removals.
--
-- ObjC selector: @- slotNames@
slotNames :: IsTKSmartCardSlotManager tkSmartCardSlotManager => tkSmartCardSlotManager -> IO (Id NSArray)
slotNames tkSmartCardSlotManager =
  sendMessage tkSmartCardSlotManager slotNamesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getSlotWithName:reply:@
getSlotWithName_replySelector :: Selector '[Id NSString, Ptr ()] ()
getSlotWithName_replySelector = mkSelector "getSlotWithName:reply:"

-- | @Selector@ for @slotNamed:@
slotNamedSelector :: Selector '[Id NSString] (Id TKSmartCardSlot)
slotNamedSelector = mkSelector "slotNamed:"

-- | @Selector@ for @createNFCSlotWithMessage:completion:@
createNFCSlotWithMessage_completionSelector :: Selector '[Id NSString, Ptr ()] ()
createNFCSlotWithMessage_completionSelector = mkSelector "createNFCSlotWithMessage:completion:"

-- | @Selector@ for @isNFCSupported@
isNFCSupportedSelector :: Selector '[] Bool
isNFCSupportedSelector = mkSelector "isNFCSupported"

-- | @Selector@ for @defaultManager@
defaultManagerSelector :: Selector '[] (Id TKSmartCardSlotManager)
defaultManagerSelector = mkSelector "defaultManager"

-- | @Selector@ for @slotNames@
slotNamesSelector :: Selector '[] (Id NSArray)
slotNamesSelector = mkSelector "slotNames"

