{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A view controller that hosts remote views provided by an app extension.
--
-- Present this view controller from your app’s interface to display the content for an associated app extension. Configure the view controller with the app extension identity and the specific scene you want to display. Use the associated delegate object to receive notifications when the app extension becomes active or inactive.
--
-- For more information about presenting this view controller and using it to display an app extension’s UI, see <doc://com.apple.documentation/documentation/extensionkit/including-extension-based-ui-in-your-interface>.
--
-- Generated bindings for @EXHostViewController@.
module ObjC.ExtensionKit.EXHostViewController
  ( EXHostViewController
  , IsEXHostViewController(..)
  , makeXPCConnectionWithError
  , delegate
  , setDelegate
  , placeholderView
  , setPlaceholderView
  , makeXPCConnectionWithErrorSelector
  , delegateSelector
  , setDelegateSelector
  , placeholderViewSelector
  , setPlaceholderViewSelector


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

import ObjC.ExtensionKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initiates an XPC connection to the app extension’s scene.
--
-- Call this method from your delegate's ``EXHostViewControllerDelegate/hostViewControllerDidActivate:`` method to initiate a scene-specific connection to the app extension.
--
-- - Returns: An object representing the connection.
--
-- ObjC selector: @- makeXPCConnectionWithError:@
makeXPCConnectionWithError :: (IsEXHostViewController exHostViewController, IsNSError error_) => exHostViewController -> error_ -> IO (Id NSXPCConnection)
makeXPCConnectionWithError exHostViewController  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg exHostViewController (mkSelector "makeXPCConnectionWithError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | A custom delegate object you use to receive notifications about the activation and deactivation of the app extension.
--
-- ObjC selector: @- delegate@
delegate :: IsEXHostViewController exHostViewController => exHostViewController -> IO RawId
delegate exHostViewController  =
    fmap (RawId . castPtr) $ sendMsg exHostViewController (mkSelector "delegate") (retPtr retVoid) []

-- | A custom delegate object you use to receive notifications about the activation and deactivation of the app extension.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsEXHostViewController exHostViewController => exHostViewController -> RawId -> IO ()
setDelegate exHostViewController  value =
    sendMsg exHostViewController (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | The view to display when the view controller has no app extension content to display.
--
-- ObjC selector: @- placeholderView@
placeholderView :: IsEXHostViewController exHostViewController => exHostViewController -> IO (Id NSView)
placeholderView exHostViewController  =
    sendMsg exHostViewController (mkSelector "placeholderView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The view to display when the view controller has no app extension content to display.
--
-- ObjC selector: @- setPlaceholderView:@
setPlaceholderView :: (IsEXHostViewController exHostViewController, IsNSView value) => exHostViewController -> value -> IO ()
setPlaceholderView exHostViewController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg exHostViewController (mkSelector "setPlaceholderView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @makeXPCConnectionWithError:@
makeXPCConnectionWithErrorSelector :: Selector
makeXPCConnectionWithErrorSelector = mkSelector "makeXPCConnectionWithError:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @placeholderView@
placeholderViewSelector :: Selector
placeholderViewSelector = mkSelector "placeholderView"

-- | @Selector@ for @setPlaceholderView:@
setPlaceholderViewSelector :: Selector
setPlaceholderViewSelector = mkSelector "setPlaceholderView:"

