{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The custom user interface used to perform a selected action.
--
-- Subclass this view controller to provide the user interface for your actions.
--
-- No matter how many actions you define, your File Provider UI extension has only one ``FPUIActionExtensionViewController`` subclass. When the user selects one of your actions, the system instantiates a copy of your subclass, calls its ``FPUIActionExtensionViewController/prepareForActionWithIdentifier:itemIdentifiers:`` method, and presents it to the user.
--
-- Your subclass must do the following:
--
-- - Override the ``FPUIActionExtensionViewController/prepareForActionWithIdentifier:itemIdentifiers:`` method to check the action identifiers and present an appropriate user interface for the selected actions. - Provide some sort of feedback, even if the action doesn't require interaction with the user. For example, present a view that quickly fades out and automatically completes the action. - Call the ``FPUIActionExtensionViewController/extensionContext`` object's ``FPUIActionExtensionContext/cancelRequestWithError:`` or ``FPUIActionExtensionContext/completeRequest`` method when the action is finished to complete the action.
--
-- Generated bindings for @FPUIActionExtensionViewController@.
module ObjC.FileProviderUI.FPUIActionExtensionViewController
  ( FPUIActionExtensionViewController
  , IsFPUIActionExtensionViewController(..)
  , prepareForError
  , prepareForActionWithIdentifier_itemIdentifiers
  , extensionContext
  , extensionContextSelector
  , prepareForActionWithIdentifier_itemIdentifiersSelector
  , prepareForErrorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FileProviderUI.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Performs any necessary setup or configuration when an authentication error occurs.
--
-- While your file provider is enumerating its content, the system calls this method whenever your file provider returns an <doc://com.apple.documentation/documentation/fileprovider/nsfileprovidererrordomain> error with a <doc://com.apple.documentation/documentation/fileprovider/nsfileprovidererrorcode/nsfileprovidererrornotauthenticated> code. Use this method to present an interface to authenticate the user.
--
-- - Parameters:   - error: An object representing the authentication error. Your File Provider   extension can pass additional information in the error's   <doc://com.apple.documentation/documentation/foundation/nserror/1411580-userinfo>   property.
--
-- ObjC selector: @- prepareForError:@
prepareForError :: (IsFPUIActionExtensionViewController fpuiActionExtensionViewController, IsNSError error_) => fpuiActionExtensionViewController -> error_ -> IO ()
prepareForError fpuiActionExtensionViewController error_ =
  sendMessage fpuiActionExtensionViewController prepareForErrorSelector (toNSError error_)

-- | Performs any necessary setup or configuration for the specified action.
--
-- Use this method to prepare a user interface for handling the action. At a minimum, you should display feedback about the action.
--
-- For more information, see <doc:adding-actions-to-the-context-menu>.
--
-- - Parameters:   - actionIdentifier: The identifier for the action performed by the user.
--
-- - itemIdentifiers: The identifiers of the items affected by the action.
--
-- ObjC selector: @- prepareForActionWithIdentifier:itemIdentifiers:@
prepareForActionWithIdentifier_itemIdentifiers :: (IsFPUIActionExtensionViewController fpuiActionExtensionViewController, IsNSString actionIdentifier, IsNSArray itemIdentifiers) => fpuiActionExtensionViewController -> actionIdentifier -> itemIdentifiers -> IO ()
prepareForActionWithIdentifier_itemIdentifiers fpuiActionExtensionViewController actionIdentifier itemIdentifiers =
  sendMessage fpuiActionExtensionViewController prepareForActionWithIdentifier_itemIdentifiersSelector (toNSString actionIdentifier) (toNSArray itemIdentifiers)

-- | The extension context provided by the host app.
--
-- ObjC selector: @- extensionContext@
extensionContext :: IsFPUIActionExtensionViewController fpuiActionExtensionViewController => fpuiActionExtensionViewController -> IO (Id FPUIActionExtensionContext)
extensionContext fpuiActionExtensionViewController =
  sendMessage fpuiActionExtensionViewController extensionContextSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @prepareForError:@
prepareForErrorSelector :: Selector '[Id NSError] ()
prepareForErrorSelector = mkSelector "prepareForError:"

-- | @Selector@ for @prepareForActionWithIdentifier:itemIdentifiers:@
prepareForActionWithIdentifier_itemIdentifiersSelector :: Selector '[Id NSString, Id NSArray] ()
prepareForActionWithIdentifier_itemIdentifiersSelector = mkSelector "prepareForActionWithIdentifier:itemIdentifiers:"

-- | @Selector@ for @extensionContext@
extensionContextSelector :: Selector '[] (Id FPUIActionExtensionContext)
extensionContextSelector = mkSelector "extensionContext"

