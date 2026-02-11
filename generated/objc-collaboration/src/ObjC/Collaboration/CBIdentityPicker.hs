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
  , runModalSelector
  , runModalForWindow_modalDelegate_didEndSelector_contextInfoSelector
  , runModalForWindow_completionHandlerSelector
  , titleSelector
  , setTitleSelector
  , allowsMultipleSelectionSelector
  , setAllowsMultipleSelectionSelector
  , identitiesSelector


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
runModal cbIdentityPicker  =
  sendMsg cbIdentityPicker (mkSelector "runModal") retCLong []

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
runModalForWindow_modalDelegate_didEndSelector_contextInfo :: (IsCBIdentityPicker cbIdentityPicker, IsNSWindow window) => cbIdentityPicker -> window -> RawId -> Selector -> Ptr () -> IO ()
runModalForWindow_modalDelegate_didEndSelector_contextInfo cbIdentityPicker  window delegate didEndSelector contextInfo =
withObjCPtr window $ \raw_window ->
    sendMsg cbIdentityPicker (mkSelector "runModalForWindow:modalDelegate:didEndSelector:contextInfo:") retVoid [argPtr (castPtr raw_window :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didEndSelector), argPtr contextInfo]

-- | Runs the identity picker modally as a sheet attached to a specified window.
--
-- - Parameters:   - window: The parent window for the sheet.
--
-- - completionHandler: The handler to run after the return value is known, but before the sheet is dismissed.
--
-- ObjC selector: @- runModalForWindow:completionHandler:@
runModalForWindow_completionHandler :: (IsCBIdentityPicker cbIdentityPicker, IsNSWindow window) => cbIdentityPicker -> window -> Ptr () -> IO ()
runModalForWindow_completionHandler cbIdentityPicker  window completionHandler =
withObjCPtr window $ \raw_window ->
    sendMsg cbIdentityPicker (mkSelector "runModalForWindow:completionHandler:") retVoid [argPtr (castPtr raw_window :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | The title of the identity picker.
--
-- The value of this property is the title text that appears at the top of the panel. By default, the title is "Select a person to share with:".
--
-- ObjC selector: @- title@
title :: IsCBIdentityPicker cbIdentityPicker => cbIdentityPicker -> IO (Id NSString)
title cbIdentityPicker  =
  sendMsg cbIdentityPicker (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The title of the identity picker.
--
-- The value of this property is the title text that appears at the top of the panel. By default, the title is "Select a person to share with:".
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsCBIdentityPicker cbIdentityPicker, IsNSString value) => cbIdentityPicker -> value -> IO ()
setTitle cbIdentityPicker  value =
withObjCPtr value $ \raw_value ->
    sendMsg cbIdentityPicker (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A Boolean value indicating whether the user is allowed to select multiple identities.
--
-- The value of this property is <doc://com.apple.documentation/documentation/objectivec/yes> if the user can select multiple records; otherwise, <doc://com.apple.documentation/documentation/objectivec/no>. The default value is <doc://com.apple.documentation/documentation/objectivec/no>.
--
-- ObjC selector: @- allowsMultipleSelection@
allowsMultipleSelection :: IsCBIdentityPicker cbIdentityPicker => cbIdentityPicker -> IO Bool
allowsMultipleSelection cbIdentityPicker  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cbIdentityPicker (mkSelector "allowsMultipleSelection") retCULong []

-- | A Boolean value indicating whether the user is allowed to select multiple identities.
--
-- The value of this property is <doc://com.apple.documentation/documentation/objectivec/yes> if the user can select multiple records; otherwise, <doc://com.apple.documentation/documentation/objectivec/no>. The default value is <doc://com.apple.documentation/documentation/objectivec/no>.
--
-- ObjC selector: @- setAllowsMultipleSelection:@
setAllowsMultipleSelection :: IsCBIdentityPicker cbIdentityPicker => cbIdentityPicker -> Bool -> IO ()
setAllowsMultipleSelection cbIdentityPicker  value =
  sendMsg cbIdentityPicker (mkSelector "setAllowsMultipleSelection:") retVoid [argCULong (if value then 1 else 0)]

-- | The array of identities (represented by @CBIdentity@ objects) selected using the identity picker.
--
-- ObjC selector: @- identities@
identities :: IsCBIdentityPicker cbIdentityPicker => cbIdentityPicker -> IO (Id NSArray)
identities cbIdentityPicker  =
  sendMsg cbIdentityPicker (mkSelector "identities") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @runModal@
runModalSelector :: Selector
runModalSelector = mkSelector "runModal"

-- | @Selector@ for @runModalForWindow:modalDelegate:didEndSelector:contextInfo:@
runModalForWindow_modalDelegate_didEndSelector_contextInfoSelector :: Selector
runModalForWindow_modalDelegate_didEndSelector_contextInfoSelector = mkSelector "runModalForWindow:modalDelegate:didEndSelector:contextInfo:"

-- | @Selector@ for @runModalForWindow:completionHandler:@
runModalForWindow_completionHandlerSelector :: Selector
runModalForWindow_completionHandlerSelector = mkSelector "runModalForWindow:completionHandler:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @allowsMultipleSelection@
allowsMultipleSelectionSelector :: Selector
allowsMultipleSelectionSelector = mkSelector "allowsMultipleSelection"

-- | @Selector@ for @setAllowsMultipleSelection:@
setAllowsMultipleSelectionSelector :: Selector
setAllowsMultipleSelectionSelector = mkSelector "setAllowsMultipleSelection:"

-- | @Selector@ for @identities@
identitiesSelector :: Selector
identitiesSelector = mkSelector "identities"

