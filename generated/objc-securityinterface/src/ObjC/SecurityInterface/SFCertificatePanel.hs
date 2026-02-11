{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SFCertificatePanel
--
-- SFCertificatePanel is a panel and sheet interface that displays one or more certificates.
--
-- Generated bindings for @SFCertificatePanel@.
module ObjC.SecurityInterface.SFCertificatePanel
  ( SFCertificatePanel
  , IsSFCertificatePanel(..)
  , sharedCertificatePanel
  , runModalForTrust_showGroup
  , runModalForCertificates_showGroup
  , beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_trust_showGroup
  , beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_certificates_showGroup
  , setPolicies
  , policies
  , setDefaultButtonTitle
  , setAlternateButtonTitle
  , setShowsHelp
  , showsHelp
  , setHelpAnchor
  , helpAnchor
  , certificateView
  , sharedCertificatePanelSelector
  , runModalForTrust_showGroupSelector
  , runModalForCertificates_showGroupSelector
  , beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_trust_showGroupSelector
  , beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_certificates_showGroupSelector
  , setPoliciesSelector
  , policiesSelector
  , setDefaultButtonTitleSelector
  , setAlternateButtonTitleSelector
  , setShowsHelpSelector
  , showsHelpSelector
  , setHelpAnchorSelector
  , helpAnchorSelector
  , certificateViewSelector


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

-- | sharedCertificatePanel
--
-- Returns a shared instance of SFCertificatePanel.
--
-- If your application can display multiple SFCertificatePanels at once,	you should allocate (alloc) and initialize (init) separate object instances instead of using this class method.
--
-- ObjC selector: @+ sharedCertificatePanel@
sharedCertificatePanel :: IO (Id SFCertificatePanel)
sharedCertificatePanel  =
  do
    cls' <- getRequiredClass "SFCertificatePanel"
    sendClassMsg cls' (mkSelector "sharedCertificatePanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | runModalForTrust:showGroup:
--
-- Displays a certificate chain in a modal panel, returning NSOKButton when dismissed.		This method is preferred over runModalForCertificates, since the SecTrustRef parameter lets you		specify policies that determine whether the certificate is valid within your application's context.
--
-- @trust@ — A trust reference which contains the certificates to display.
--
-- @showGroup@ — Specifies whether additional certificates (other than the leaf certificate) are displayed.
--
-- ObjC selector: @- runModalForTrust:showGroup:@
runModalForTrust_showGroup :: IsSFCertificatePanel sfCertificatePanel => sfCertificatePanel -> Ptr () -> Bool -> IO CLong
runModalForTrust_showGroup sfCertificatePanel  trust showGroup =
  sendMsg sfCertificatePanel (mkSelector "runModalForTrust:showGroup:") retCLong [argPtr trust, argCULong (if showGroup then 1 else 0)]

-- | runModalForCertificates:showGroup:
--
-- Displays one or more specified certificates in a modal panel, returning NSOKButton when dismissed.
--
-- @certificates@ — The certificates to display.		Pass a NSArray containing one or more SecCertificateRef instances in this parameter.		The leaf certificate is assumed to be at index 0; the order of additional certificates in the array is not critical.
--
-- @showGroup@ — Specifies whether additional certificates (other than the leaf certificate) are displayed.		To show only a single certificate, specify only one SecCertificateRef in the array and set showGroup to NO.
--
-- ObjC selector: @- runModalForCertificates:showGroup:@
runModalForCertificates_showGroup :: (IsSFCertificatePanel sfCertificatePanel, IsNSArray certificates) => sfCertificatePanel -> certificates -> Bool -> IO CLong
runModalForCertificates_showGroup sfCertificatePanel  certificates showGroup =
withObjCPtr certificates $ \raw_certificates ->
    sendMsg sfCertificatePanel (mkSelector "runModalForCertificates:showGroup:") retCLong [argPtr (castPtr raw_certificates :: Ptr ()), argCULong (if showGroup then 1 else 0)]

-- | beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:trust:showGroup:
--
-- Displays a certificate chain in a modal sheet.		This is the preferred sheet method for SFCertificatePanel, since the SecTrustRef parameter lets you		specify policies that determine whether the certificate is valid within your application's context.
--
-- @docWindow@ — The parent window to which the sheet is attached.
--
-- @modalDelegate@ — The object whose didEndSelector method will be called when the sheet is dismissed.
--
-- @didEndSelector@ — This method is called when the sheet is dismissed.
--
-- @contextInfo@ — Client-defined contextual data which will be passed to the didEndSelector method.
--
-- @trust@ — A trust reference which contains the certificates to display.
--
-- @showGroup@ — Specifies whether additional certificates (other than the leaf certificate) are displayed.
--
-- The didEndSelector method should have the following signature:        - (void)certificateSheetDidEnd:(NSWindow *)sheet returnCode:(NSInteger)returnCode contextInfo:(void *)contextInfo
--
-- ObjC selector: @- beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:trust:showGroup:@
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_trust_showGroup :: (IsSFCertificatePanel sfCertificatePanel, IsNSWindow docWindow) => sfCertificatePanel -> docWindow -> RawId -> Selector -> Ptr () -> Ptr () -> Bool -> IO ()
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_trust_showGroup sfCertificatePanel  docWindow delegate didEndSelector contextInfo trust showGroup =
withObjCPtr docWindow $ \raw_docWindow ->
    sendMsg sfCertificatePanel (mkSelector "beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:trust:showGroup:") retVoid [argPtr (castPtr raw_docWindow :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didEndSelector), argPtr contextInfo, argPtr trust, argCULong (if showGroup then 1 else 0)]

-- | beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:certificates:showGroup:
--
-- Displays one or more specified certificates in a modal sheet.
--
-- @docWindow@ — The parent window to which the sheet is attached.
--
-- @modalDelegate@ — The object whose didEndSelector method will be called when the sheet is dismissed.
--
-- @didEndSelector@ — This method is called when the sheet is dismissed.
--
-- @contextInfo@ — Client-defined contextual data which will be passed to the didEndSelector method.
--
-- @certificates@ — The certificates to display.		Pass a NSArray containing one or more SecCertificateRef instances in this parameter.
--
-- @showGroup@ — Specifies whether additional certificates (other than the leaf certificate) are displayed.
--
-- The didEndSelector method should have the following signature:        - (void)certificateSheetDidEnd:(NSWindow *)sheet returnCode:(NSInteger)returnCode contextInfo:(void *)contextInfo
--
-- ObjC selector: @- beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:certificates:showGroup:@
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_certificates_showGroup :: (IsSFCertificatePanel sfCertificatePanel, IsNSWindow docWindow, IsNSArray certificates) => sfCertificatePanel -> docWindow -> RawId -> Selector -> Ptr () -> certificates -> Bool -> IO ()
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_certificates_showGroup sfCertificatePanel  docWindow delegate didEndSelector contextInfo certificates showGroup =
withObjCPtr docWindow $ \raw_docWindow ->
  withObjCPtr certificates $ \raw_certificates ->
      sendMsg sfCertificatePanel (mkSelector "beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:certificates:showGroup:") retVoid [argPtr (castPtr raw_docWindow :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didEndSelector), argPtr contextInfo, argPtr (castPtr raw_certificates :: Ptr ()), argCULong (if showGroup then 1 else 0)]

-- | setPolicies:
--
-- Specifies one or more policies that apply to the displayed certificates.
--
-- @policies@ — The policies to use when evaluating the certificates' status.		You can pass either a SecPolicyRef or a NSArray (containing one or more SecPolicyRef instances) in this parameter.		If policies is set to nil, the Apple X.509 Basic Policy will be used.
--
-- Applications will typically display a SFCertificatePanel in the context of a specific usage, such as SSL or S/MIME.	You should set only the policy references which apply to your intended usage.
--
-- ObjC selector: @- setPolicies:@
setPolicies :: IsSFCertificatePanel sfCertificatePanel => sfCertificatePanel -> RawId -> IO ()
setPolicies sfCertificatePanel  policies =
  sendMsg sfCertificatePanel (mkSelector "setPolicies:") retVoid [argPtr (castPtr (unRawId policies) :: Ptr ())]

-- | policies
--
-- Returns an array of policies used to evaluate the status of the displayed certificates.
--
-- This method returns an autoreleased NSArray containing one or more SecPolicyRef instances, as set by a previous setPolicies: call.
--
-- ObjC selector: @- policies@
policies :: IsSFCertificatePanel sfCertificatePanel => sfCertificatePanel -> IO (Id NSArray)
policies sfCertificatePanel  =
  sendMsg sfCertificatePanel (mkSelector "policies") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setDefaultButtonTitle:
--
-- Customizes the title of the default button.
--
-- @title@ — The new title for the default button.
--
-- ObjC selector: @- setDefaultButtonTitle:@
setDefaultButtonTitle :: (IsSFCertificatePanel sfCertificatePanel, IsNSString title) => sfCertificatePanel -> title -> IO ()
setDefaultButtonTitle sfCertificatePanel  title =
withObjCPtr title $ \raw_title ->
    sendMsg sfCertificatePanel (mkSelector "setDefaultButtonTitle:") retVoid [argPtr (castPtr raw_title :: Ptr ())]

-- | setAlternateButtonTitle:
--
-- Customizes the title of the alternate button.
--
-- @title@ — The new title for the alternate button. If title is set to nil, the button will not be shown.
--
-- ObjC selector: @- setAlternateButtonTitle:@
setAlternateButtonTitle :: (IsSFCertificatePanel sfCertificatePanel, IsNSString title) => sfCertificatePanel -> title -> IO ()
setAlternateButtonTitle sfCertificatePanel  title =
withObjCPtr title $ \raw_title ->
    sendMsg sfCertificatePanel (mkSelector "setAlternateButtonTitle:") retVoid [argPtr (castPtr raw_title :: Ptr ())]

-- | @- setShowsHelp:@
setShowsHelp :: IsSFCertificatePanel sfCertificatePanel => sfCertificatePanel -> Bool -> IO ()
setShowsHelp sfCertificatePanel  showsHelp =
  sendMsg sfCertificatePanel (mkSelector "setShowsHelp:") retVoid [argCULong (if showsHelp then 1 else 0)]

-- | @- showsHelp@
showsHelp :: IsSFCertificatePanel sfCertificatePanel => sfCertificatePanel -> IO Bool
showsHelp sfCertificatePanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfCertificatePanel (mkSelector "showsHelp") retCULong []

-- | @- setHelpAnchor:@
setHelpAnchor :: (IsSFCertificatePanel sfCertificatePanel, IsNSString anchor) => sfCertificatePanel -> anchor -> IO ()
setHelpAnchor sfCertificatePanel  anchor =
withObjCPtr anchor $ \raw_anchor ->
    sendMsg sfCertificatePanel (mkSelector "setHelpAnchor:") retVoid [argPtr (castPtr raw_anchor :: Ptr ())]

-- | @- helpAnchor@
helpAnchor :: IsSFCertificatePanel sfCertificatePanel => sfCertificatePanel -> IO (Id NSString)
helpAnchor sfCertificatePanel  =
  sendMsg sfCertificatePanel (mkSelector "helpAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- certificateView@
certificateView :: IsSFCertificatePanel sfCertificatePanel => sfCertificatePanel -> IO (Id SFCertificateView)
certificateView sfCertificatePanel  =
  sendMsg sfCertificatePanel (mkSelector "certificateView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedCertificatePanel@
sharedCertificatePanelSelector :: Selector
sharedCertificatePanelSelector = mkSelector "sharedCertificatePanel"

-- | @Selector@ for @runModalForTrust:showGroup:@
runModalForTrust_showGroupSelector :: Selector
runModalForTrust_showGroupSelector = mkSelector "runModalForTrust:showGroup:"

-- | @Selector@ for @runModalForCertificates:showGroup:@
runModalForCertificates_showGroupSelector :: Selector
runModalForCertificates_showGroupSelector = mkSelector "runModalForCertificates:showGroup:"

-- | @Selector@ for @beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:trust:showGroup:@
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_trust_showGroupSelector :: Selector
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_trust_showGroupSelector = mkSelector "beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:trust:showGroup:"

-- | @Selector@ for @beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:certificates:showGroup:@
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_certificates_showGroupSelector :: Selector
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_certificates_showGroupSelector = mkSelector "beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:certificates:showGroup:"

-- | @Selector@ for @setPolicies:@
setPoliciesSelector :: Selector
setPoliciesSelector = mkSelector "setPolicies:"

-- | @Selector@ for @policies@
policiesSelector :: Selector
policiesSelector = mkSelector "policies"

-- | @Selector@ for @setDefaultButtonTitle:@
setDefaultButtonTitleSelector :: Selector
setDefaultButtonTitleSelector = mkSelector "setDefaultButtonTitle:"

-- | @Selector@ for @setAlternateButtonTitle:@
setAlternateButtonTitleSelector :: Selector
setAlternateButtonTitleSelector = mkSelector "setAlternateButtonTitle:"

-- | @Selector@ for @setShowsHelp:@
setShowsHelpSelector :: Selector
setShowsHelpSelector = mkSelector "setShowsHelp:"

-- | @Selector@ for @showsHelp@
showsHelpSelector :: Selector
showsHelpSelector = mkSelector "showsHelp"

-- | @Selector@ for @setHelpAnchor:@
setHelpAnchorSelector :: Selector
setHelpAnchorSelector = mkSelector "setHelpAnchor:"

-- | @Selector@ for @helpAnchor@
helpAnchorSelector :: Selector
helpAnchorSelector = mkSelector "helpAnchor"

-- | @Selector@ for @certificateView@
certificateViewSelector :: Selector
certificateViewSelector = mkSelector "certificateView"

