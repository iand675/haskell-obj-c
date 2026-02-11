{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SFCertificateView
--
-- SFCertificateView is a NSView that displays the contents of a certificate.
--
-- Generated bindings for @SFCertificateView@.
module ObjC.SecurityInterface.SFCertificateView
  ( SFCertificateView
  , IsSFCertificateView(..)
  , setCertificate
  , certificate
  , setPolicies
  , policies
  , setEditableTrust
  , isEditable
  , setDisplayTrust
  , isTrustDisplayed
  , saveTrustSettings
  , setDisplayDetails
  , detailsDisplayed
  , setDetailsDisclosed
  , detailsDisclosed
  , setPoliciesDisclosed
  , policiesDisclosed
  , setCertificateSelector
  , certificateSelector
  , setPoliciesSelector
  , policiesSelector
  , setEditableTrustSelector
  , isEditableSelector
  , setDisplayTrustSelector
  , isTrustDisplayedSelector
  , saveTrustSettingsSelector
  , setDisplayDetailsSelector
  , detailsDisplayedSelector
  , setDetailsDisclosedSelector
  , detailsDisclosedSelector
  , setPoliciesDisclosedSelector
  , policiesDisclosedSelector


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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | setCertificate:
--
-- Specifies the certificate that's displayed in the view.
--
-- @certificate@ — The new certificate for the view.
--
-- ObjC selector: @- setCertificate:@
setCertificate :: IsSFCertificateView sfCertificateView => sfCertificateView -> Ptr () -> IO ()
setCertificate sfCertificateView  certificate =
  sendMsg sfCertificateView (mkSelector "setCertificate:") retVoid [argPtr certificate]

-- | certificate
--
-- Returns the certificate currently displayed in the view.
--
-- ObjC selector: @- certificate@
certificate :: IsSFCertificateView sfCertificateView => sfCertificateView -> IO (Ptr ())
certificate sfCertificateView  =
  fmap castPtr $ sendMsg sfCertificateView (mkSelector "certificate") (retPtr retVoid) []

-- | setPolicies:
--
-- Specifies one or more policies that apply to the displayed certificate.
--
-- @policies@ — The policies to use when evaluating this certificate's status.		You can pass either a SecPolicyRef or a NSArray (containing one or more SecPolicyRef instances) in this parameter.		If policies is set to nil, the Apple X.509 Basic Policy will be used.
--
-- Applications will typically display a SFCertificateView in the context of a specific usage, such as SSL or S/MIME.	You should set only the policy references which apply to your intended usage.
--
-- ObjC selector: @- setPolicies:@
setPolicies :: IsSFCertificateView sfCertificateView => sfCertificateView -> RawId -> IO ()
setPolicies sfCertificateView  policies =
  sendMsg sfCertificateView (mkSelector "setPolicies:") retVoid [argPtr (castPtr (unRawId policies) :: Ptr ())]

-- | policies
--
-- Returns an array of policies used to evaluate the status of the displayed certificate.
--
-- This method returns an autoreleased NSArray containing one or more SecPolicyRef instances, as set by a previous	setPolicies: call. The array will always contain at least one item (the Apple X.509 Basic Policy).
--
-- ObjC selector: @- policies@
policies :: IsSFCertificateView sfCertificateView => sfCertificateView -> IO (Id NSArray)
policies sfCertificateView  =
  sendMsg sfCertificateView (mkSelector "policies") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setEditableTrust:
--
-- Specifies whether the user can edit the certificate's trust settings.
--
-- @editable@ — Pass YES if the trust settings should be editable.
--
-- ObjC selector: @- setEditableTrust:@
setEditableTrust :: IsSFCertificateView sfCertificateView => sfCertificateView -> Bool -> IO ()
setEditableTrust sfCertificateView  editable =
  sendMsg sfCertificateView (mkSelector "setEditableTrust:") retVoid [argCULong (if editable then 1 else 0)]

-- | isEditable
--
-- Indicates if the view allows the user to edit the certificate's trust.
--
-- ObjC selector: @- isEditable@
isEditable :: IsSFCertificateView sfCertificateView => sfCertificateView -> IO Bool
isEditable sfCertificateView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfCertificateView (mkSelector "isEditable") retCULong []

-- | setDisplayTrust:
--
-- Specifies whether the user can see the certificate's trust settings.
--
-- @display@ — Pass YES to display the trust settings, or NO to hide them.
--
-- Certificate trust settings are not displayed by default.	To show the certificate's trust settings, you must explicitly set the display value to YES.
--
-- ObjC selector: @- setDisplayTrust:@
setDisplayTrust :: IsSFCertificateView sfCertificateView => sfCertificateView -> Bool -> IO ()
setDisplayTrust sfCertificateView  display =
  sendMsg sfCertificateView (mkSelector "setDisplayTrust:") retVoid [argCULong (if display then 1 else 0)]

-- | isTrustDisplayed
--
-- Indicates if the view currently shows the certificate's trust settings.
--
-- ObjC selector: @- isTrustDisplayed@
isTrustDisplayed :: IsSFCertificateView sfCertificateView => sfCertificateView -> IO Bool
isTrustDisplayed sfCertificateView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfCertificateView (mkSelector "isTrustDisplayed") retCULong []

-- | saveTrustSettings
--
-- Saves the user's current trust settings for the displayed certificate.
--
-- If trust settings are not editable, this method effectively does nothing.	You can use SecTrustGetUserTrust (see <Security/SecTrust.h>) to subsequently retrieve the trust settings.
--
-- ObjC selector: @- saveTrustSettings@
saveTrustSettings :: IsSFCertificateView sfCertificateView => sfCertificateView -> IO ()
saveTrustSettings sfCertificateView  =
  sendMsg sfCertificateView (mkSelector "saveTrustSettings") retVoid []

-- | setDisplayDetails:
--
-- Specifies whether the user can see the certificate's entire contents.
--
-- @display@ — Pass YES to display the certificate's details, or NO to hide them.
--
-- For behavioral compatibility with Panther, certificate details are displayed by default.	To hide the details of a certificate, you must explicitly set the display value to NO.
--
-- ObjC selector: @- setDisplayDetails:@
setDisplayDetails :: IsSFCertificateView sfCertificateView => sfCertificateView -> Bool -> IO ()
setDisplayDetails sfCertificateView  display =
  sendMsg sfCertificateView (mkSelector "setDisplayDetails:") retVoid [argCULong (if display then 1 else 0)]

-- | detailsDisplayed
--
-- Indicates if the view currently shows the certificate's details.
--
-- ObjC selector: @- detailsDisplayed@
detailsDisplayed :: IsSFCertificateView sfCertificateView => sfCertificateView -> IO Bool
detailsDisplayed sfCertificateView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfCertificateView (mkSelector "detailsDisplayed") retCULong []

-- | setDetailsDisclosed:
--
-- Specifies whether the certificate details subview is disclosed (i.e. the triangle is turned down).
--
-- @disclosed@ — Pass YES to disclose the certificate details subview, or NO to collapse it.
--
-- By default, the certificate details subview is not disclosed.	Note that changing the disclosure state of a line item does not affect whether the item itself is shown;	use setDisplayDetails: to cause it to be displayed or hidden.
--
-- ObjC selector: @- setDetailsDisclosed:@
setDetailsDisclosed :: IsSFCertificateView sfCertificateView => sfCertificateView -> Bool -> IO ()
setDetailsDisclosed sfCertificateView  disclosed =
  sendMsg sfCertificateView (mkSelector "setDetailsDisclosed:") retVoid [argCULong (if disclosed then 1 else 0)]

-- | detailsDisclosed
--
-- Indicates if the view currently discloses the certificate's details.
--
-- ObjC selector: @- detailsDisclosed@
detailsDisclosed :: IsSFCertificateView sfCertificateView => sfCertificateView -> IO Bool
detailsDisclosed sfCertificateView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfCertificateView (mkSelector "detailsDisclosed") retCULong []

-- | setPoliciesDisclosed:
--
-- Specifies whether the trust policy settings are disclosed (i.e. the triangle is turned down).
--
-- @disclosed@ — Pass YES to disclose the trust policies subview, or NO to collapse it.
--
-- By default, the trust policies subview is not disclosed.	Note that changing the disclosure state of a line item does not affect whether the item itself is shown;	use setDisplayTrust: to cause it to be displayed or hidden.
--
-- ObjC selector: @- setPoliciesDisclosed:@
setPoliciesDisclosed :: IsSFCertificateView sfCertificateView => sfCertificateView -> Bool -> IO ()
setPoliciesDisclosed sfCertificateView  disclosed =
  sendMsg sfCertificateView (mkSelector "setPoliciesDisclosed:") retVoid [argCULong (if disclosed then 1 else 0)]

-- | policiesDisclosed
--
-- Indicates if the view currently discloses the trust policy settings.
--
-- ObjC selector: @- policiesDisclosed@
policiesDisclosed :: IsSFCertificateView sfCertificateView => sfCertificateView -> IO Bool
policiesDisclosed sfCertificateView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfCertificateView (mkSelector "policiesDisclosed") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setCertificate:@
setCertificateSelector :: Selector
setCertificateSelector = mkSelector "setCertificate:"

-- | @Selector@ for @certificate@
certificateSelector :: Selector
certificateSelector = mkSelector "certificate"

-- | @Selector@ for @setPolicies:@
setPoliciesSelector :: Selector
setPoliciesSelector = mkSelector "setPolicies:"

-- | @Selector@ for @policies@
policiesSelector :: Selector
policiesSelector = mkSelector "policies"

-- | @Selector@ for @setEditableTrust:@
setEditableTrustSelector :: Selector
setEditableTrustSelector = mkSelector "setEditableTrust:"

-- | @Selector@ for @isEditable@
isEditableSelector :: Selector
isEditableSelector = mkSelector "isEditable"

-- | @Selector@ for @setDisplayTrust:@
setDisplayTrustSelector :: Selector
setDisplayTrustSelector = mkSelector "setDisplayTrust:"

-- | @Selector@ for @isTrustDisplayed@
isTrustDisplayedSelector :: Selector
isTrustDisplayedSelector = mkSelector "isTrustDisplayed"

-- | @Selector@ for @saveTrustSettings@
saveTrustSettingsSelector :: Selector
saveTrustSettingsSelector = mkSelector "saveTrustSettings"

-- | @Selector@ for @setDisplayDetails:@
setDisplayDetailsSelector :: Selector
setDisplayDetailsSelector = mkSelector "setDisplayDetails:"

-- | @Selector@ for @detailsDisplayed@
detailsDisplayedSelector :: Selector
detailsDisplayedSelector = mkSelector "detailsDisplayed"

-- | @Selector@ for @setDetailsDisclosed:@
setDetailsDisclosedSelector :: Selector
setDetailsDisclosedSelector = mkSelector "setDetailsDisclosed:"

-- | @Selector@ for @detailsDisclosed@
detailsDisclosedSelector :: Selector
detailsDisclosedSelector = mkSelector "detailsDisclosed"

-- | @Selector@ for @setPoliciesDisclosed:@
setPoliciesDisclosedSelector :: Selector
setPoliciesDisclosedSelector = mkSelector "setPoliciesDisclosed:"

-- | @Selector@ for @policiesDisclosed@
policiesDisclosedSelector :: Selector
policiesDisclosedSelector = mkSelector "policiesDisclosed"

