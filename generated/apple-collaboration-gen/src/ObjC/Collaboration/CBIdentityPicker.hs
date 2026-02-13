{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A @CBIdentityPicker@ object allows a user to select identities—for example, user or group objects—that it wants one or more services or shared resources to have access to. An identity picker can be displayed either as an application-modal dialog or as a sheet attached to a document window. An identity picker returns the selected records to be added to access control lists using Collaboration. If a selected record is not a user or group identity, then an identity picker prompts the user for additional information—such as a password—to promote that record to a sharing account.
--
-- Generated bindings for @CBIdentityPicker@.
module ObjC.Collaboration.CBIdentityPicker
  ( CBIdentityPicker
  , IsCBIdentityPicker(..)
  , runModal
  , runModalForWindow_modalDelegate_didEndSelector_contextInfo
  , runModalForWindow_completionHandler
  , title
  , setTitle
  , allowsMultipleSelection
  , setAllowsMultipleSelection
  , identities
  , allowsMultipleSelectionSelector
  , identitiesSelector
  , runModalForWindow_completionHandlerSelector
  , runModalForWindow_modalDelegate_didEndSelector_contextInfoSelector
  , runModalSelector
  , setAllowsMultipleSelectionSelector
  , setTitleSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Collaboration.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Runs the receiver as an application-modal dialog.
--
-- The receiver may create identities for selected records if necessary.
--
-- - Returns: @NSOKButton@ if the user selected OK; otherwise, @NSCancelButton@.
--
-- ObjC selector: @- runModal@
runModal :: IsCBIdentityPicker cbIdentityPicker => cbIdentityPicker -> IO CLong
runModal cbIdentityPicker =
  sendMessage cbIdentityPicker runModalSelector

-- | Runs the receiver modally as a sheet attached to a specified window.
--
-- The @didEndSelector@ parameter is a selector that takes three arguments. The corresponding method should have a declaration modeled on the following example:
--
-- ```swift  - (void)identityPickerDidEnd:(CBIdentityPicker *)identityPicker returnCode:(NSInteger)returnCode contextInfo:(void *)contextInfo; ```
--
-- where the @identityPicker@ argument is the identity picker object, the @returnCode@ argument is the button the user clicked, and @contextInfo@ is the same @contextInfo@ argument that was passed in the original message.
--
-- - Parameters:   - window: The parent window for the sheet.
--
-- - delegate: The delegate for the modal session.
--
-- - didEndSelector: A message sent to the delegate after the user responds but before the sheet is dismissed.
--
-- - contextInfo: Contextual data passed to the delegate in the @didEndSelector@ message.
--
-- ObjC selector: @- runModalForWindow:modalDelegate:didEndSelector:contextInfo:@
runModalForWindow_modalDelegate_didEndSelector_contextInfo :: (IsCBIdentityPicker cbIdentityPicker, IsNSWindow window) => cbIdentityPicker -> window -> RawId -> Sel -> Ptr () -> IO ()
runModalForWindow_modalDelegate_didEndSelector_contextInfo cbIdentityPicker window delegate didEndSelector contextInfo =
  sendMessage cbIdentityPicker runModalForWindow_modalDelegate_didEndSelector_contextInfoSelector (toNSWindow window) delegate didEndSelector contextInfo

-- | Runs the identity picker modally as a sheet attached to a specified window.
--
-- - Parameters:   - window: The parent window for the sheet.
--
-- - completionHandler: The handler to run after the return value is known, but before the sheet is dismissed.
--
-- ObjC selector: @- runModalForWindow:completionHandler:@
runModalForWindow_completionHandler :: (IsCBIdentityPicker cbIdentityPicker, IsNSWindow window) => cbIdentityPicker -> window -> Ptr () -> IO ()
runModalForWindow_completionHandler cbIdentityPicker window completionHandler =
  sendMessage cbIdentityPicker runModalForWindow_completionHandlerSelector (toNSWindow window) completionHandler

-- | The title of the identity picker.
--
-- The value of this property is the title text that appears at the top of the panel. By default, the title is "Select a person to share with:".
--
-- ObjC selector: @- title@
title :: IsCBIdentityPicker cbIdentityPicker => cbIdentityPicker -> IO (Id NSString)
title cbIdentityPicker =
  sendMessage cbIdentityPicker titleSelector

-- | The title of the identity picker.
--
-- The value of this property is the title text that appears at the top of the panel. By default, the title is "Select a person to share with:".
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsCBIdentityPicker cbIdentityPicker, IsNSString value) => cbIdentityPicker -> value -> IO ()
setTitle cbIdentityPicker value =
  sendMessage cbIdentityPicker setTitleSelector (toNSString value)

-- | A Boolean value indicating whether the user is allowed to select multiple identities.
--
-- The value of this property is <doc://com.apple.documentation/documentation/objectivec/yes> if the user can select multiple records; otherwise, <doc://com.apple.documentation/documentation/objectivec/no>. The default value is <doc://com.apple.documentation/documentation/objectivec/no>.
--
-- ObjC selector: @- allowsMultipleSelection@
allowsMultipleSelection :: IsCBIdentityPicker cbIdentityPicker => cbIdentityPicker -> IO Bool
allowsMultipleSelection cbIdentityPicker =
  sendMessage cbIdentityPicker allowsMultipleSelectionSelector

-- | A Boolean value indicating whether the user is allowed to select multiple identities.
--
-- The value of this property is <doc://com.apple.documentation/documentation/objectivec/yes> if the user can select multiple records; otherwise, <doc://com.apple.documentation/documentation/objectivec/no>. The default value is <doc://com.apple.documentation/documentation/objectivec/no>.
--
-- ObjC selector: @- setAllowsMultipleSelection:@
setAllowsMultipleSelection :: IsCBIdentityPicker cbIdentityPicker => cbIdentityPicker -> Bool -> IO ()
setAllowsMultipleSelection cbIdentityPicker value =
  sendMessage cbIdentityPicker setAllowsMultipleSelectionSelector value

-- | The array of identities (represented by @CBIdentity@ objects) selected using the identity picker.
--
-- ObjC selector: @- identities@
identities :: IsCBIdentityPicker cbIdentityPicker => cbIdentityPicker -> IO (Id NSArray)
identities cbIdentityPicker =
  sendMessage cbIdentityPicker identitiesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @runModal@
runModalSelector :: Selector '[] CLong
runModalSelector = mkSelector "runModal"

-- | @Selector@ for @runModalForWindow:modalDelegate:didEndSelector:contextInfo:@
runModalForWindow_modalDelegate_didEndSelector_contextInfoSelector :: Selector '[Id NSWindow, RawId, Sel, Ptr ()] ()
runModalForWindow_modalDelegate_didEndSelector_contextInfoSelector = mkSelector "runModalForWindow:modalDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @runModalForWindow:completionHandler:@
runModalForWindow_completionHandlerSelector :: Selector '[Id NSWindow, Ptr ()] ()
runModalForWindow_completionHandlerSelector = mkSelector "runModalForWindow:completionHandler:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @allowsMultipleSelection@
allowsMultipleSelectionSelector :: Selector '[] Bool
allowsMultipleSelectionSelector = mkSelector "allowsMultipleSelection"

-- | @Selector@ for @setAllowsMultipleSelection:@
setAllowsMultipleSelectionSelector :: Selector '[Bool] ()
setAllowsMultipleSelectionSelector = mkSelector "setAllowsMultipleSelection:"

-- | @Selector@ for @identities@
identitiesSelector :: Selector '[] (Id NSArray)
identitiesSelector = mkSelector "identities"

