{-# LANGUAGE DataKinds #-}
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
  , certificateSelector
  , detailsDisclosedSelector
  , detailsDisplayedSelector
  , isEditableSelector
  , isTrustDisplayedSelector
  , policiesDisclosedSelector
  , policiesSelector
  , saveTrustSettingsSelector
  , setCertificateSelector
  , setDetailsDisclosedSelector
  , setDisplayDetailsSelector
  , setDisplayTrustSelector
  , setEditableTrustSelector
  , setPoliciesDisclosedSelector
  , setPoliciesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
setCertificate sfCertificateView certificate =
  sendMessage sfCertificateView setCertificateSelector certificate

-- | certificate
--
-- Returns the certificate currently displayed in the view.
--
-- ObjC selector: @- certificate@
certificate :: IsSFCertificateView sfCertificateView => sfCertificateView -> IO (Ptr ())
certificate sfCertificateView =
  sendMessage sfCertificateView certificateSelector

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
setPolicies sfCertificateView policies =
  sendMessage sfCertificateView setPoliciesSelector policies

-- | policies
--
-- Returns an array of policies used to evaluate the status of the displayed certificate.
--
-- This method returns an autoreleased NSArray containing one or more SecPolicyRef instances, as set by a previous	setPolicies: call. The array will always contain at least one item (the Apple X.509 Basic Policy).
--
-- ObjC selector: @- policies@
policies :: IsSFCertificateView sfCertificateView => sfCertificateView -> IO (Id NSArray)
policies sfCertificateView =
  sendMessage sfCertificateView policiesSelector

-- | setEditableTrust:
--
-- Specifies whether the user can edit the certificate's trust settings.
--
-- @editable@ — Pass YES if the trust settings should be editable.
--
-- ObjC selector: @- setEditableTrust:@
setEditableTrust :: IsSFCertificateView sfCertificateView => sfCertificateView -> Bool -> IO ()
setEditableTrust sfCertificateView editable =
  sendMessage sfCertificateView setEditableTrustSelector editable

-- | isEditable
--
-- Indicates if the view allows the user to edit the certificate's trust.
--
-- ObjC selector: @- isEditable@
isEditable :: IsSFCertificateView sfCertificateView => sfCertificateView -> IO Bool
isEditable sfCertificateView =
  sendMessage sfCertificateView isEditableSelector

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
setDisplayTrust sfCertificateView display =
  sendMessage sfCertificateView setDisplayTrustSelector display

-- | isTrustDisplayed
--
-- Indicates if the view currently shows the certificate's trust settings.
--
-- ObjC selector: @- isTrustDisplayed@
isTrustDisplayed :: IsSFCertificateView sfCertificateView => sfCertificateView -> IO Bool
isTrustDisplayed sfCertificateView =
  sendMessage sfCertificateView isTrustDisplayedSelector

-- | saveTrustSettings
--
-- Saves the user's current trust settings for the displayed certificate.
--
-- If trust settings are not editable, this method effectively does nothing.	You can use SecTrustGetUserTrust (see <Security/SecTrust.h>) to subsequently retrieve the trust settings.
--
-- ObjC selector: @- saveTrustSettings@
saveTrustSettings :: IsSFCertificateView sfCertificateView => sfCertificateView -> IO ()
saveTrustSettings sfCertificateView =
  sendMessage sfCertificateView saveTrustSettingsSelector

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
setDisplayDetails sfCertificateView display =
  sendMessage sfCertificateView setDisplayDetailsSelector display

-- | detailsDisplayed
--
-- Indicates if the view currently shows the certificate's details.
--
-- ObjC selector: @- detailsDisplayed@
detailsDisplayed :: IsSFCertificateView sfCertificateView => sfCertificateView -> IO Bool
detailsDisplayed sfCertificateView =
  sendMessage sfCertificateView detailsDisplayedSelector

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
setDetailsDisclosed sfCertificateView disclosed =
  sendMessage sfCertificateView setDetailsDisclosedSelector disclosed

-- | detailsDisclosed
--
-- Indicates if the view currently discloses the certificate's details.
--
-- ObjC selector: @- detailsDisclosed@
detailsDisclosed :: IsSFCertificateView sfCertificateView => sfCertificateView -> IO Bool
detailsDisclosed sfCertificateView =
  sendMessage sfCertificateView detailsDisclosedSelector

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
setPoliciesDisclosed sfCertificateView disclosed =
  sendMessage sfCertificateView setPoliciesDisclosedSelector disclosed

-- | policiesDisclosed
--
-- Indicates if the view currently discloses the trust policy settings.
--
-- ObjC selector: @- policiesDisclosed@
policiesDisclosed :: IsSFCertificateView sfCertificateView => sfCertificateView -> IO Bool
policiesDisclosed sfCertificateView =
  sendMessage sfCertificateView policiesDisclosedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setCertificate:@
setCertificateSelector :: Selector '[Ptr ()] ()
setCertificateSelector = mkSelector "setCertificate:"

-- | @Selector@ for @certificate@
certificateSelector :: Selector '[] (Ptr ())
certificateSelector = mkSelector "certificate"

-- | @Selector@ for @setPolicies:@
setPoliciesSelector :: Selector '[RawId] ()
setPoliciesSelector = mkSelector "setPolicies:"

-- | @Selector@ for @policies@
policiesSelector :: Selector '[] (Id NSArray)
policiesSelector = mkSelector "policies"

-- | @Selector@ for @setEditableTrust:@
setEditableTrustSelector :: Selector '[Bool] ()
setEditableTrustSelector = mkSelector "setEditableTrust:"

-- | @Selector@ for @isEditable@
isEditableSelector :: Selector '[] Bool
isEditableSelector = mkSelector "isEditable"

-- | @Selector@ for @setDisplayTrust:@
setDisplayTrustSelector :: Selector '[Bool] ()
setDisplayTrustSelector = mkSelector "setDisplayTrust:"

-- | @Selector@ for @isTrustDisplayed@
isTrustDisplayedSelector :: Selector '[] Bool
isTrustDisplayedSelector = mkSelector "isTrustDisplayed"

-- | @Selector@ for @saveTrustSettings@
saveTrustSettingsSelector :: Selector '[] ()
saveTrustSettingsSelector = mkSelector "saveTrustSettings"

-- | @Selector@ for @setDisplayDetails:@
setDisplayDetailsSelector :: Selector '[Bool] ()
setDisplayDetailsSelector = mkSelector "setDisplayDetails:"

-- | @Selector@ for @detailsDisplayed@
detailsDisplayedSelector :: Selector '[] Bool
detailsDisplayedSelector = mkSelector "detailsDisplayed"

-- | @Selector@ for @setDetailsDisclosed:@
setDetailsDisclosedSelector :: Selector '[Bool] ()
setDetailsDisclosedSelector = mkSelector "setDetailsDisclosed:"

-- | @Selector@ for @detailsDisclosed@
detailsDisclosedSelector :: Selector '[] Bool
detailsDisclosedSelector = mkSelector "detailsDisclosed"

-- | @Selector@ for @setPoliciesDisclosed:@
setPoliciesDisclosedSelector :: Selector '[Bool] ()
setPoliciesDisclosedSelector = mkSelector "setPoliciesDisclosed:"

-- | @Selector@ for @policiesDisclosed@
policiesDisclosedSelector :: Selector '[] Bool
policiesDisclosedSelector = mkSelector "policiesDisclosed"

