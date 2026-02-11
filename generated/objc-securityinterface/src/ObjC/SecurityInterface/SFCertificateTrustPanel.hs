{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SFCertificateTrustPanel
--
-- SFCertificateTrustPanel is a panel and sheet interface that allows a user to make trust decisions	when one or more certificates involved in an operation are invalid or cannot be verified. It should be used	whenever confirmation is required before proceeding with a certificate-related operation. It can also be	displayed as an informative alert without requiring a decision to be made (if the operation or transaction	has already occurred.)
--
-- Generated bindings for @SFCertificateTrustPanel@.
module ObjC.SecurityInterface.SFCertificateTrustPanel
  ( SFCertificateTrustPanel
  , IsSFCertificateTrustPanel(..)
  , sharedCertificateTrustPanel
  , runModalForTrust_message
  , beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_trust_message
  , setInformativeText
  , informativeText
  , sharedCertificateTrustPanelSelector
  , runModalForTrust_messageSelector
  , beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_trust_messageSelector
  , setInformativeTextSelector
  , informativeTextSelector


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

-- | sharedCertificateTrustPanel
--
-- Returns a shared instance of SFCertificateTrustPanel.
--
-- If your application can display multiple SFCertificateTrustPanels at once,	you should allocate (alloc) and initialize (init) separate object instances instead of using this class method.
--
-- ObjC selector: @+ sharedCertificateTrustPanel@
sharedCertificateTrustPanel :: IO (Id SFCertificateTrustPanel)
sharedCertificateTrustPanel  =
  do
    cls' <- getRequiredClass "SFCertificateTrustPanel"
    sendClassMsg cls' (mkSelector "sharedCertificateTrustPanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | runModalForTrust:message:
--
-- Displays a modal panel that shows the results of a certificate trust evaluation.		Returns NSOKButton if the default button is pressed, or NSCancelButton if the alternate button is pressed.		Note that the user can edit trust decisions in this panel; call SecTrustGetResult after the panel is dismissed		to obtain the current trust result for the SecTrustRef.
--
-- @trust@ — A trust reference, previously created with SecTrustCreateWithCertificates (see <Security/SecTrust.h>).
--
-- @message@ — Client-defined message string to display in the panel.
--
-- ObjC selector: @- runModalForTrust:message:@
runModalForTrust_message :: (IsSFCertificateTrustPanel sfCertificateTrustPanel, IsNSString message) => sfCertificateTrustPanel -> Ptr () -> message -> IO CLong
runModalForTrust_message sfCertificateTrustPanel  trust message =
withObjCPtr message $ \raw_message ->
    sendMsg sfCertificateTrustPanel (mkSelector "runModalForTrust:message:") retCLong [argPtr trust, argPtr (castPtr raw_message :: Ptr ())]

-- | beginSheetForWindow:trust:message:modalDelegate:didEndSelector:contextInfo:
--
-- Displays a modal sheet that shows the results of a certificate trust evaluation.
--
-- The didEndSelector method should have the following signature:        - (void)certificateTrustSheetDidEnd:(NSWindow *)sheet returnCode:(NSInteger)returnCode contextInfo:(void *)contextInfo		returnCode will contain either NSOKButton or NSCancelButton.		Note that the user can edit trust decisions in this panel; call SecTrustGetResult after the panel is dismissed		to obtain the current trust result for the SecTrustRef.
--
-- @docWindow@ — The parent window to which the sheet is attached.
--
-- @modalDelegate@ — The object whose didEndSelector method will be called when the sheet is dismissed.
--
-- @didEndSelector@ — This method is called when the sheet is dismissed.
--
-- @contextInfo@ — Client-defined contextual data which will be passed to the didEndSelector method.
--
-- @trust@ — A trust reference, previously created with SecTrustCreateWithCertificates (see <Security/SecTrust.h>).
--
-- @message@ — Client-defined message string to display in the panel.
--
-- ObjC selector: @- beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:trust:message:@
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_trust_message :: (IsSFCertificateTrustPanel sfCertificateTrustPanel, IsNSWindow docWindow, IsNSString message) => sfCertificateTrustPanel -> docWindow -> RawId -> Selector -> Ptr () -> Ptr () -> message -> IO ()
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_trust_message sfCertificateTrustPanel  docWindow delegate didEndSelector contextInfo trust message =
withObjCPtr docWindow $ \raw_docWindow ->
  withObjCPtr message $ \raw_message ->
      sendMsg sfCertificateTrustPanel (mkSelector "beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:trust:message:") retVoid [argPtr (castPtr raw_docWindow :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didEndSelector), argPtr contextInfo, argPtr trust, argPtr (castPtr raw_message :: Ptr ())]

-- | setInformativeText:
--
-- Sets the optional informative text displayed in the SFCertificateTrustPanel.
--
-- @informativeText@ — The informative text to display in the panel.
--
-- By default, informative text describing the current certificate trust status is displayed.		Call this method only if your application needs to customize the displayed informative text.
--
-- ObjC selector: @- setInformativeText:@
setInformativeText :: (IsSFCertificateTrustPanel sfCertificateTrustPanel, IsNSString informativeText) => sfCertificateTrustPanel -> informativeText -> IO ()
setInformativeText sfCertificateTrustPanel  informativeText =
withObjCPtr informativeText $ \raw_informativeText ->
    sendMsg sfCertificateTrustPanel (mkSelector "setInformativeText:") retVoid [argPtr (castPtr raw_informativeText :: Ptr ())]

-- | informativeText
--
-- Returns the informative text currently displayed in the SFCertificateTrustPanel.
--
-- ObjC selector: @- informativeText@
informativeText :: IsSFCertificateTrustPanel sfCertificateTrustPanel => sfCertificateTrustPanel -> IO (Id NSString)
informativeText sfCertificateTrustPanel  =
  sendMsg sfCertificateTrustPanel (mkSelector "informativeText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedCertificateTrustPanel@
sharedCertificateTrustPanelSelector :: Selector
sharedCertificateTrustPanelSelector = mkSelector "sharedCertificateTrustPanel"

-- | @Selector@ for @runModalForTrust:message:@
runModalForTrust_messageSelector :: Selector
runModalForTrust_messageSelector = mkSelector "runModalForTrust:message:"

-- | @Selector@ for @beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:trust:message:@
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_trust_messageSelector :: Selector
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_trust_messageSelector = mkSelector "beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:trust:message:"

-- | @Selector@ for @setInformativeText:@
setInformativeTextSelector :: Selector
setInformativeTextSelector = mkSelector "setInformativeText:"

-- | @Selector@ for @informativeText@
informativeTextSelector :: Selector
informativeTextSelector = mkSelector "informativeText"

