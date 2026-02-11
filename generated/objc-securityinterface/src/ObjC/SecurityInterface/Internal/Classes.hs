{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.SecurityInterface.Internal.Classes (
    module ObjC.SecurityInterface.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.SecurityFoundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.SecurityFoundation.Internal.Classes

-- ---------- SFAuthorizationViewDelegate ----------

-- | SFAuthorizationView
--
-- SFAuthorizationView is a custom view that you can use to visually represent restricted functionality, requiring authorization for access.
--
-- You can add SFAuthorizationView to your application as a custom view, make your controller the delegate for the view and initialize the view by setting a right string (setString:) or rights structure (setAuthorizationRights:) to check for, as well as auto-updates (setAutoupdate:) or manual updates (updateStatus:). Note that you'll have to call updateStatus: to set the lock icon to its initial state. Changes to the current state as well as the startup state (after the initial updateStatus) are communicated to the delegate.  Implementing any of the following is optional):          authorized: changed to unlocked      deauthorized: changed to locked      shouldDeauthorize: when a user wants to lock, the delegates can react to this before it happens and avoid it by returning NO.      cancelAuthorization: the user cancelled authorization.    Calls to updateStatus: return YES if in the unlocked state, NO otherwise. Note that when committing changes or performing actions, authorization has to be checked again before going ahead with it. The default behavior of this view is to pre-authorize rights, if this is not possible it will unlock and wait for authorization to be checked when explicitly required. For handing the SFAuthorization (authorization:) to another process it's NSCoder support can be used.
-- 
-- Phantom type for @SFAuthorizationViewDelegate@.
data SFAuthorizationViewDelegate

instance IsObjCObject (Id SFAuthorizationViewDelegate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFAuthorizationViewDelegate"

class IsObjCObject a => IsSFAuthorizationViewDelegate a where
  toSFAuthorizationViewDelegate :: a -> Id SFAuthorizationViewDelegate

instance IsSFAuthorizationViewDelegate (Id SFAuthorizationViewDelegate) where
  toSFAuthorizationViewDelegate = unsafeCastId

-- ---------- SFAutoLockTextValue ----------

-- | SFKeychainSettingsPanel
--
-- SFKeychainSettingsPanel is a panel and sheet interface that allows users to change their keychain settings.
-- 
-- Phantom type for @SFAutoLockTextValue@.
data SFAutoLockTextValue

instance IsObjCObject (Id SFAutoLockTextValue) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFAutoLockTextValue"

class IsObjCObject a => IsSFAutoLockTextValue a where
  toSFAutoLockTextValue :: a -> Id SFAutoLockTextValue

instance IsSFAutoLockTextValue (Id SFAutoLockTextValue) where
  toSFAutoLockTextValue = unsafeCastId

-- ---------- SFAuthorizationPluginView ----------

-- | SFAuthorizationPluginView
--
-- SFAuthorizationPluginView is a class that you can use to insert an NSView into AuthorizationPlugin interfaces.
--
-- SFAuthorizationPluginView provides AuthorizationPlugin writers with an easy way to provide a user interface for their AuthorizationPlugin without having to duplicate the standard authentication dialog or the login window dialog.  This class was designed to be subclassed by the AuthorizationPlugin writer.  The following methods were designed to be overridden: buttonPressed:, didActivate, willActivateWithUser:, didDeactivate, firstKeyView, firstResponder, lastKeyView, setEnabled:, and viewForType:.  In order to display the user interface, the AuthorizationPlugin should create an instance of your subclass and then call displayView.  That will cause the appropriate dialog to be displayed and when credentials are needed, the overridden methods will be called in order to display the NSView provided by the subclass.
-- 
-- Phantom type for @SFAuthorizationPluginView@.
data SFAuthorizationPluginView

instance IsObjCObject (Id SFAuthorizationPluginView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFAuthorizationPluginView"

class IsNSObject a => IsSFAuthorizationPluginView a where
  toSFAuthorizationPluginView :: a -> Id SFAuthorizationPluginView

instance IsSFAuthorizationPluginView (Id SFAuthorizationPluginView) where
  toSFAuthorizationPluginView = unsafeCastId

instance IsNSObject (Id SFAuthorizationPluginView) where
  toNSObject = unsafeCastId

-- ---------- SFAuthorizationView ----------

-- | Phantom type for @SFAuthorizationView@.
data SFAuthorizationView

instance IsObjCObject (Id SFAuthorizationView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFAuthorizationView"

class IsNSView a => IsSFAuthorizationView a where
  toSFAuthorizationView :: a -> Id SFAuthorizationView

instance IsSFAuthorizationView (Id SFAuthorizationView) where
  toSFAuthorizationView = unsafeCastId

instance IsNSObject (Id SFAuthorizationView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SFAuthorizationView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id SFAuthorizationView) where
  toNSView = unsafeCastId

-- ---------- SFChooseIdentityTableCellView ----------

-- | Phantom type for @SFChooseIdentityTableCellView@.
data SFChooseIdentityTableCellView

instance IsObjCObject (Id SFChooseIdentityTableCellView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFChooseIdentityTableCellView"

class IsNSTableCellView a => IsSFChooseIdentityTableCellView a where
  toSFChooseIdentityTableCellView :: a -> Id SFChooseIdentityTableCellView

instance IsSFChooseIdentityTableCellView (Id SFChooseIdentityTableCellView) where
  toSFChooseIdentityTableCellView = unsafeCastId

instance IsNSObject (Id SFChooseIdentityTableCellView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SFChooseIdentityTableCellView) where
  toNSResponder = unsafeCastId

instance IsNSTableCellView (Id SFChooseIdentityTableCellView) where
  toNSTableCellView = unsafeCastId

instance IsNSView (Id SFChooseIdentityTableCellView) where
  toNSView = unsafeCastId

-- ---------- SFCertificateView ----------

-- | SFCertificateView
--
-- SFCertificateView is a NSView that displays the contents of a certificate.
-- 
-- Phantom type for @SFCertificateView@.
data SFCertificateView

instance IsObjCObject (Id SFCertificateView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFCertificateView"

class IsNSVisualEffectView a => IsSFCertificateView a where
  toSFCertificateView :: a -> Id SFCertificateView

instance IsSFCertificateView (Id SFCertificateView) where
  toSFCertificateView = unsafeCastId

instance IsNSObject (Id SFCertificateView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id SFCertificateView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id SFCertificateView) where
  toNSView = unsafeCastId

instance IsNSVisualEffectView (Id SFCertificateView) where
  toNSVisualEffectView = unsafeCastId

-- ---------- SFCertificatePanel ----------

-- | SFCertificatePanel
--
-- SFCertificatePanel is a panel and sheet interface that displays one or more certificates.
-- 
-- Phantom type for @SFCertificatePanel@.
data SFCertificatePanel

instance IsObjCObject (Id SFCertificatePanel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFCertificatePanel"

class IsNSPanel a => IsSFCertificatePanel a where
  toSFCertificatePanel :: a -> Id SFCertificatePanel

instance IsSFCertificatePanel (Id SFCertificatePanel) where
  toSFCertificatePanel = unsafeCastId

instance IsNSObject (Id SFCertificatePanel) where
  toNSObject = unsafeCastId

instance IsNSPanel (Id SFCertificatePanel) where
  toNSPanel = unsafeCastId

instance IsNSResponder (Id SFCertificatePanel) where
  toNSResponder = unsafeCastId

instance IsNSWindow (Id SFCertificatePanel) where
  toNSWindow = unsafeCastId

-- ---------- SFChooseIdentityPanel ----------

-- | SFChooseIdentityPanel
--
-- SFChooseIdentityPanel is a panel and sheet interface that allows a user to select an identity from a list.
-- 
-- Phantom type for @SFChooseIdentityPanel@.
data SFChooseIdentityPanel

instance IsObjCObject (Id SFChooseIdentityPanel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFChooseIdentityPanel"

class IsNSPanel a => IsSFChooseIdentityPanel a where
  toSFChooseIdentityPanel :: a -> Id SFChooseIdentityPanel

instance IsSFChooseIdentityPanel (Id SFChooseIdentityPanel) where
  toSFChooseIdentityPanel = unsafeCastId

instance IsNSObject (Id SFChooseIdentityPanel) where
  toNSObject = unsafeCastId

instance IsNSPanel (Id SFChooseIdentityPanel) where
  toNSPanel = unsafeCastId

instance IsNSResponder (Id SFChooseIdentityPanel) where
  toNSResponder = unsafeCastId

instance IsNSWindow (Id SFChooseIdentityPanel) where
  toNSWindow = unsafeCastId

-- ---------- SFKeychainSettingsPanel ----------

-- | Phantom type for @SFKeychainSettingsPanel@.
data SFKeychainSettingsPanel

instance IsObjCObject (Id SFKeychainSettingsPanel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFKeychainSettingsPanel"

class IsNSPanel a => IsSFKeychainSettingsPanel a where
  toSFKeychainSettingsPanel :: a -> Id SFKeychainSettingsPanel

instance IsSFKeychainSettingsPanel (Id SFKeychainSettingsPanel) where
  toSFKeychainSettingsPanel = unsafeCastId

instance IsNSObject (Id SFKeychainSettingsPanel) where
  toNSObject = unsafeCastId

instance IsNSPanel (Id SFKeychainSettingsPanel) where
  toNSPanel = unsafeCastId

instance IsNSResponder (Id SFKeychainSettingsPanel) where
  toNSResponder = unsafeCastId

instance IsNSWindow (Id SFKeychainSettingsPanel) where
  toNSWindow = unsafeCastId

-- ---------- SFKeychainSavePanel ----------

-- | SFKeychainSavePanel
--
-- SFKeychainSavePanel is a panel and sheet interface used to create a keychain using the NSSavePanel UI.
-- 
-- Phantom type for @SFKeychainSavePanel@.
data SFKeychainSavePanel

instance IsObjCObject (Id SFKeychainSavePanel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFKeychainSavePanel"

class IsNSSavePanel a => IsSFKeychainSavePanel a where
  toSFKeychainSavePanel :: a -> Id SFKeychainSavePanel

instance IsSFKeychainSavePanel (Id SFKeychainSavePanel) where
  toSFKeychainSavePanel = unsafeCastId

instance IsNSObject (Id SFKeychainSavePanel) where
  toNSObject = unsafeCastId

instance IsNSPanel (Id SFKeychainSavePanel) where
  toNSPanel = unsafeCastId

instance IsNSResponder (Id SFKeychainSavePanel) where
  toNSResponder = unsafeCastId

instance IsNSSavePanel (Id SFKeychainSavePanel) where
  toNSSavePanel = unsafeCastId

instance IsNSWindow (Id SFKeychainSavePanel) where
  toNSWindow = unsafeCastId

-- ---------- SFCertificateTrustPanel ----------

-- | SFCertificateTrustPanel
--
-- SFCertificateTrustPanel is a panel and sheet interface that allows a user to make trust decisions	when one or more certificates involved in an operation are invalid or cannot be verified. It should be used	whenever confirmation is required before proceeding with a certificate-related operation. It can also be	displayed as an informative alert without requiring a decision to be made (if the operation or transaction	has already occurred.)
-- 
-- Phantom type for @SFCertificateTrustPanel@.
data SFCertificateTrustPanel

instance IsObjCObject (Id SFCertificateTrustPanel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "SFCertificateTrustPanel"

class IsSFCertificatePanel a => IsSFCertificateTrustPanel a where
  toSFCertificateTrustPanel :: a -> Id SFCertificateTrustPanel

instance IsSFCertificateTrustPanel (Id SFCertificateTrustPanel) where
  toSFCertificateTrustPanel = unsafeCastId

instance IsNSObject (Id SFCertificateTrustPanel) where
  toNSObject = unsafeCastId

instance IsNSPanel (Id SFCertificateTrustPanel) where
  toNSPanel = unsafeCastId

instance IsNSResponder (Id SFCertificateTrustPanel) where
  toNSResponder = unsafeCastId

instance IsNSWindow (Id SFCertificateTrustPanel) where
  toNSWindow = unsafeCastId

instance IsSFCertificatePanel (Id SFCertificateTrustPanel) where
  toSFCertificatePanel = unsafeCastId
