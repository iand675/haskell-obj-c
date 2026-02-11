{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSObject@.
module ObjC.SecurityInterface.NSObject
  ( NSObject
  , IsNSObject(..)
  , chooseIdentityPanelShowHelp
  , certificatePanelShowHelp
  , authorizationViewDidAuthorize
  , authorizationViewDidDeauthorize
  , authorizationViewShouldDeauthorize
  , authorizationViewCreatedAuthorization
  , authorizationViewReleasedAuthorization
  , authorizationViewDidHide
  , chooseIdentityPanelShowHelpSelector
  , certificatePanelShowHelpSelector
  , authorizationViewDidAuthorizeSelector
  , authorizationViewDidDeauthorizeSelector
  , authorizationViewShouldDeauthorizeSelector
  , authorizationViewCreatedAuthorizationSelector
  , authorizationViewReleasedAuthorizationSelector
  , authorizationViewDidHideSelector


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

import ObjC.SecurityInterface.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- chooseIdentityPanelShowHelp:@
chooseIdentityPanelShowHelp :: (IsNSObject nsObject, IsSFChooseIdentityPanel sender) => nsObject -> sender -> IO Bool
chooseIdentityPanelShowHelp nsObject  sender =
withObjCPtr sender $ \raw_sender ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "chooseIdentityPanelShowHelp:") retCULong [argPtr (castPtr raw_sender :: Ptr ())]

-- | @- certificatePanelShowHelp:@
certificatePanelShowHelp :: (IsNSObject nsObject, IsSFCertificatePanel sender) => nsObject -> sender -> IO Bool
certificatePanelShowHelp nsObject  sender =
withObjCPtr sender $ \raw_sender ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "certificatePanelShowHelp:") retCULong [argPtr (castPtr raw_sender :: Ptr ())]

-- | authorizationViewDidAuthorize:
--
-- @view@ —
--
-- ObjC selector: @- authorizationViewDidAuthorize:@
authorizationViewDidAuthorize :: (IsNSObject nsObject, IsSFAuthorizationView view) => nsObject -> view -> IO ()
authorizationViewDidAuthorize nsObject  view =
withObjCPtr view $ \raw_view ->
    sendMsg nsObject (mkSelector "authorizationViewDidAuthorize:") retVoid [argPtr (castPtr raw_view :: Ptr ())]

-- | authorizationViewDidDeauthorize:
--
-- @view@ —
--
-- ObjC selector: @- authorizationViewDidDeauthorize:@
authorizationViewDidDeauthorize :: (IsNSObject nsObject, IsSFAuthorizationView view) => nsObject -> view -> IO ()
authorizationViewDidDeauthorize nsObject  view =
withObjCPtr view $ \raw_view ->
    sendMsg nsObject (mkSelector "authorizationViewDidDeauthorize:") retVoid [argPtr (castPtr raw_view :: Ptr ())]

-- | authorizationViewShouldDeauthorize:
--
-- @view@ —
--
-- ObjC selector: @- authorizationViewShouldDeauthorize:@
authorizationViewShouldDeauthorize :: (IsNSObject nsObject, IsSFAuthorizationView view) => nsObject -> view -> IO Bool
authorizationViewShouldDeauthorize nsObject  view =
withObjCPtr view $ \raw_view ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsObject (mkSelector "authorizationViewShouldDeauthorize:") retCULong [argPtr (castPtr raw_view :: Ptr ())]

-- | authorizationViewCreatedAuthorization:
--
-- @view@ —
--
-- ObjC selector: @- authorizationViewCreatedAuthorization:@
authorizationViewCreatedAuthorization :: (IsNSObject nsObject, IsSFAuthorizationView view) => nsObject -> view -> IO ()
authorizationViewCreatedAuthorization nsObject  view =
withObjCPtr view $ \raw_view ->
    sendMsg nsObject (mkSelector "authorizationViewCreatedAuthorization:") retVoid [argPtr (castPtr raw_view :: Ptr ())]

-- | authorizationViewReleasedAuthorization:
--
-- @view@ —
--
-- ObjC selector: @- authorizationViewReleasedAuthorization:@
authorizationViewReleasedAuthorization :: (IsNSObject nsObject, IsSFAuthorizationView view) => nsObject -> view -> IO ()
authorizationViewReleasedAuthorization nsObject  view =
withObjCPtr view $ \raw_view ->
    sendMsg nsObject (mkSelector "authorizationViewReleasedAuthorization:") retVoid [argPtr (castPtr raw_view :: Ptr ())]

-- | authorizationViewDidHide:
--
-- @view@ —
--
-- ObjC selector: @- authorizationViewDidHide:@
authorizationViewDidHide :: (IsNSObject nsObject, IsSFAuthorizationView view) => nsObject -> view -> IO ()
authorizationViewDidHide nsObject  view =
withObjCPtr view $ \raw_view ->
    sendMsg nsObject (mkSelector "authorizationViewDidHide:") retVoid [argPtr (castPtr raw_view :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @chooseIdentityPanelShowHelp:@
chooseIdentityPanelShowHelpSelector :: Selector
chooseIdentityPanelShowHelpSelector = mkSelector "chooseIdentityPanelShowHelp:"

-- | @Selector@ for @certificatePanelShowHelp:@
certificatePanelShowHelpSelector :: Selector
certificatePanelShowHelpSelector = mkSelector "certificatePanelShowHelp:"

-- | @Selector@ for @authorizationViewDidAuthorize:@
authorizationViewDidAuthorizeSelector :: Selector
authorizationViewDidAuthorizeSelector = mkSelector "authorizationViewDidAuthorize:"

-- | @Selector@ for @authorizationViewDidDeauthorize:@
authorizationViewDidDeauthorizeSelector :: Selector
authorizationViewDidDeauthorizeSelector = mkSelector "authorizationViewDidDeauthorize:"

-- | @Selector@ for @authorizationViewShouldDeauthorize:@
authorizationViewShouldDeauthorizeSelector :: Selector
authorizationViewShouldDeauthorizeSelector = mkSelector "authorizationViewShouldDeauthorize:"

-- | @Selector@ for @authorizationViewCreatedAuthorization:@
authorizationViewCreatedAuthorizationSelector :: Selector
authorizationViewCreatedAuthorizationSelector = mkSelector "authorizationViewCreatedAuthorization:"

-- | @Selector@ for @authorizationViewReleasedAuthorization:@
authorizationViewReleasedAuthorizationSelector :: Selector
authorizationViewReleasedAuthorizationSelector = mkSelector "authorizationViewReleasedAuthorization:"

-- | @Selector@ for @authorizationViewDidHide:@
authorizationViewDidHideSelector :: Selector
authorizationViewDidHideSelector = mkSelector "authorizationViewDidHide:"

