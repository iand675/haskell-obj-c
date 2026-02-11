{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | EKVirtualConferenceProvider
--
-- Provides virtual conferences to Calendar.
--
-- Subclass this class in your extension and override the below two methods.
--
-- Generated bindings for @EKVirtualConferenceProvider@.
module ObjC.EventKit.EKVirtualConferenceProvider
  ( EKVirtualConferenceProvider
  , IsEKVirtualConferenceProvider(..)
  , fetchVirtualConferenceForIdentifier_completionHandler
  , fetchVirtualConferenceForIdentifier_completionHandlerSelector


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

import ObjC.EventKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | fetchVirtualConferenceForIdentifier:completionHandler:
--
-- Called to fetch the specific virtual conference details to add to an event.
--
-- Your extension must override this method in order to add virtual conferences to calendar events.                When your extension has finished retrieving the requested virtual conference details, create an                 EKVirtualConferenceDescriptor object containing the virtual conference details and call the completion handler                with the EKVirtualConferenceDescriptor object as the first argument.
--
-- @identifier@ — Represents the room type that the user chose. This is the same identifier that your extension                                    chose for this EKVirtualConferenceRoomTypeDescriptor in an earlier call to                                    fetchAvailableRoomTypesWithCompletionHandler:.
--
-- @completionHandler@ — A block to call when your extension has finished retrieving the virtual conference details.                                    If your extension is unable to retrieve virtual conference details at this time (for example,                                    because network access is not available), call this block with nil for the first argument and                                    an appropriate NSError object for the second argument. Do not call this block with nil for                                    both arguments. Similarly, do not call this block with both a non-nil                                    EKVirtualConferenceDescriptor and a non-nil NSError.  This block must be called when your                                    extension has finished its work.
--
-- ObjC selector: @- fetchVirtualConferenceForIdentifier:completionHandler:@
fetchVirtualConferenceForIdentifier_completionHandler :: (IsEKVirtualConferenceProvider ekVirtualConferenceProvider, IsNSString identifier) => ekVirtualConferenceProvider -> identifier -> Ptr () -> IO ()
fetchVirtualConferenceForIdentifier_completionHandler ekVirtualConferenceProvider  identifier completionHandler =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg ekVirtualConferenceProvider (mkSelector "fetchVirtualConferenceForIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchVirtualConferenceForIdentifier:completionHandler:@
fetchVirtualConferenceForIdentifier_completionHandlerSelector :: Selector
fetchVirtualConferenceForIdentifier_completionHandlerSelector = mkSelector "fetchVirtualConferenceForIdentifier:completionHandler:"

