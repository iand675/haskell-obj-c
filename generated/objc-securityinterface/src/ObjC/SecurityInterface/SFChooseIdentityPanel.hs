{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SFChooseIdentityPanel
--
-- SFChooseIdentityPanel is a panel and sheet interface that allows a user to select an identity from a list.
--
-- Generated bindings for @SFChooseIdentityPanel@.
module ObjC.SecurityInterface.SFChooseIdentityPanel
  ( SFChooseIdentityPanel
  , IsSFChooseIdentityPanel(..)
  , sharedChooseIdentityPanel
  , runModalForIdentities_message
  , beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_identities_message
  , identity
  , setPolicies
  , policies
  , setDefaultButtonTitle
  , setAlternateButtonTitle
  , setShowsHelp
  , showsHelp
  , setHelpAnchor
  , helpAnchor
  , setInformativeText
  , informativeText
  , setDomain
  , domain
  , sharedChooseIdentityPanelSelector
  , runModalForIdentities_messageSelector
  , beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_identities_messageSelector
  , identitySelector
  , setPoliciesSelector
  , policiesSelector
  , setDefaultButtonTitleSelector
  , setAlternateButtonTitleSelector
  , setShowsHelpSelector
  , showsHelpSelector
  , setHelpAnchorSelector
  , helpAnchorSelector
  , setInformativeTextSelector
  , informativeTextSelector
  , setDomainSelector
  , domainSelector


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

-- | sharedChooseIdentityPanel
--
-- Returns a shared instance of SFChooseIdentityPanel.
--
-- If your application can display multiple SFChooseIdentityPanels at once,	you should allocate (alloc) and initialize (init) separate object instances instead of using this class method.
--
-- ObjC selector: @+ sharedChooseIdentityPanel@
sharedChooseIdentityPanel :: IO (Id SFChooseIdentityPanel)
sharedChooseIdentityPanel  =
  do
    cls' <- getRequiredClass "SFChooseIdentityPanel"
    sendClassMsg cls' (mkSelector "sharedChooseIdentityPanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | runModalForIdentities:message:
--
-- Displays a supplied list of identities in a modal panel, returning NSOKButton or NSCancelButton when dismissed.	Use the -identity method to subsequently obtain the identity chosen by the user.
--
-- @identities@ — An array of SecIdentityRef objects, usually obtained from an identity search (see <Security/SecIdentitySearch.h>).
--
-- @message@ — Client-defined message string to display in the panel.
--
-- ObjC selector: @- runModalForIdentities:message:@
runModalForIdentities_message :: (IsSFChooseIdentityPanel sfChooseIdentityPanel, IsNSArray identities, IsNSString message) => sfChooseIdentityPanel -> identities -> message -> IO CLong
runModalForIdentities_message sfChooseIdentityPanel  identities message =
withObjCPtr identities $ \raw_identities ->
  withObjCPtr message $ \raw_message ->
      sendMsg sfChooseIdentityPanel (mkSelector "runModalForIdentities:message:") retCLong [argPtr (castPtr raw_identities :: Ptr ()), argPtr (castPtr raw_message :: Ptr ())]

-- | beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:identities:message:
--
-- Displays a sheet version of the SFChooseIdentityPanel. The didEndSelector returnCode will contain either NSOKButton or NSCancelButton.
--
-- @docWindow@ — The parent window to which the sheet is attached.
--
-- @modalDelegate@ — The object whose didEndSelector method will be called when the sheet is dismissed.
--
-- @didEndSelector@ — This method is called when the sheet is dismissed.
--
-- @contextInfo@ — Client-defined contextual data which will be passed to the didEndSelector method.
--
-- @identities@ — An array of SecIdentityRef objects, usually obtained from an identity search (see <Security/SecIdentitySearch.h>).
--
-- @message@ — Client-defined message string to display in the panel.
--
-- The didEndSelector method should have the following signature:        - (void)chooseIdentitySheetDidEnd:(NSWindow *)sheet returnCode:(NSInteger)returnCode contextInfo:(void *)contextInfo
--
-- ObjC selector: @- beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:identities:message:@
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_identities_message :: (IsSFChooseIdentityPanel sfChooseIdentityPanel, IsNSWindow docWindow, IsNSArray identities, IsNSString message) => sfChooseIdentityPanel -> docWindow -> RawId -> Selector -> Ptr () -> identities -> message -> IO ()
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_identities_message sfChooseIdentityPanel  docWindow delegate didEndSelector contextInfo identities message =
withObjCPtr docWindow $ \raw_docWindow ->
  withObjCPtr identities $ \raw_identities ->
    withObjCPtr message $ \raw_message ->
        sendMsg sfChooseIdentityPanel (mkSelector "beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:identities:message:") retVoid [argPtr (castPtr raw_docWindow :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (unSelector didEndSelector), argPtr contextInfo, argPtr (castPtr raw_identities :: Ptr ()), argPtr (castPtr raw_message :: Ptr ())]

-- | identity
--
-- Returns the identity that the user chose in the panel.
--
-- ObjC selector: @- identity@
identity :: IsSFChooseIdentityPanel sfChooseIdentityPanel => sfChooseIdentityPanel -> IO (Ptr ())
identity sfChooseIdentityPanel  =
  fmap castPtr $ sendMsg sfChooseIdentityPanel (mkSelector "identity") (retPtr retVoid) []

-- | setPolicies:
--
-- Specifies one or more policies that apply to the displayed certificates.
--
-- @policies@ — The policies to use when evaluating the certificates' status.		You can pass either a SecPolicyRef or a NSArray (containing one or more SecPolicyRef instances) in this parameter.		If policies is set to nil, the Apple X.509 Basic Policy will be used.
--
-- Applications will typically display a SFChooseIdentityPanel in the context of a specific usage, such as SSL or S/MIME.	You should set only the policy references which apply to your intended usage.
--
-- ObjC selector: @- setPolicies:@
setPolicies :: IsSFChooseIdentityPanel sfChooseIdentityPanel => sfChooseIdentityPanel -> RawId -> IO ()
setPolicies sfChooseIdentityPanel  policies =
  sendMsg sfChooseIdentityPanel (mkSelector "setPolicies:") retVoid [argPtr (castPtr (unRawId policies) :: Ptr ())]

-- | policies
--
-- Returns an array of policies used to evaluate the status of the displayed certificates.
--
-- This method returns an autoreleased NSArray containing one or more SecPolicyRef instances, as set by a previous setPolicies: call.
--
-- ObjC selector: @- policies@
policies :: IsSFChooseIdentityPanel sfChooseIdentityPanel => sfChooseIdentityPanel -> IO (Id NSArray)
policies sfChooseIdentityPanel  =
  sendMsg sfChooseIdentityPanel (mkSelector "policies") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setDefaultButtonTitle:
--
-- Customizes the title of the default button.
--
-- @title@ — The new title for the default button.
--
-- ObjC selector: @- setDefaultButtonTitle:@
setDefaultButtonTitle :: (IsSFChooseIdentityPanel sfChooseIdentityPanel, IsNSString title) => sfChooseIdentityPanel -> title -> IO ()
setDefaultButtonTitle sfChooseIdentityPanel  title =
withObjCPtr title $ \raw_title ->
    sendMsg sfChooseIdentityPanel (mkSelector "setDefaultButtonTitle:") retVoid [argPtr (castPtr raw_title :: Ptr ())]

-- | setAlternateButtonTitle:
--
-- Customizes the title of the alternate button.
--
-- @title@ — The new title for the alternate button. If title is set to nil, the button will not be shown.
--
-- ObjC selector: @- setAlternateButtonTitle:@
setAlternateButtonTitle :: (IsSFChooseIdentityPanel sfChooseIdentityPanel, IsNSString title) => sfChooseIdentityPanel -> title -> IO ()
setAlternateButtonTitle sfChooseIdentityPanel  title =
withObjCPtr title $ \raw_title ->
    sendMsg sfChooseIdentityPanel (mkSelector "setAlternateButtonTitle:") retVoid [argPtr (castPtr raw_title :: Ptr ())]

-- | @- setShowsHelp:@
setShowsHelp :: IsSFChooseIdentityPanel sfChooseIdentityPanel => sfChooseIdentityPanel -> Bool -> IO ()
setShowsHelp sfChooseIdentityPanel  showsHelp =
  sendMsg sfChooseIdentityPanel (mkSelector "setShowsHelp:") retVoid [argCULong (if showsHelp then 1 else 0)]

-- | @- showsHelp@
showsHelp :: IsSFChooseIdentityPanel sfChooseIdentityPanel => sfChooseIdentityPanel -> IO Bool
showsHelp sfChooseIdentityPanel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg sfChooseIdentityPanel (mkSelector "showsHelp") retCULong []

-- | @- setHelpAnchor:@
setHelpAnchor :: (IsSFChooseIdentityPanel sfChooseIdentityPanel, IsNSString anchor) => sfChooseIdentityPanel -> anchor -> IO ()
setHelpAnchor sfChooseIdentityPanel  anchor =
withObjCPtr anchor $ \raw_anchor ->
    sendMsg sfChooseIdentityPanel (mkSelector "setHelpAnchor:") retVoid [argPtr (castPtr raw_anchor :: Ptr ())]

-- | @- helpAnchor@
helpAnchor :: IsSFChooseIdentityPanel sfChooseIdentityPanel => sfChooseIdentityPanel -> IO (Id NSString)
helpAnchor sfChooseIdentityPanel  =
  sendMsg sfChooseIdentityPanel (mkSelector "helpAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setInformativeText:
--
-- Sets the optional informative text displayed in the SFChooseIdentityPanel.
--
-- @informativeText@ — The informative text to display in the panel.
--
-- Call this method to set the informative text to be displayed.
--
-- ObjC selector: @- setInformativeText:@
setInformativeText :: (IsSFChooseIdentityPanel sfChooseIdentityPanel, IsNSString informativeText) => sfChooseIdentityPanel -> informativeText -> IO ()
setInformativeText sfChooseIdentityPanel  informativeText =
withObjCPtr informativeText $ \raw_informativeText ->
    sendMsg sfChooseIdentityPanel (mkSelector "setInformativeText:") retVoid [argPtr (castPtr raw_informativeText :: Ptr ())]

-- | informativeText
--
-- Returns the informative text currently displayed in the SFChooseIdentityPanel.
--
-- ObjC selector: @- informativeText@
informativeText :: IsSFChooseIdentityPanel sfChooseIdentityPanel => sfChooseIdentityPanel -> IO (Id NSString)
informativeText sfChooseIdentityPanel  =
  sendMsg sfChooseIdentityPanel (mkSelector "informativeText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | setDomain:
--
-- Sets an optional domain in which the identity is to be used.
--
-- @domainString@ — A string containing a hostname, RFC822 name (email address), URL, or similar identifier.
--
-- Call this method to associate a domain with the chosen identity.		If the user chooses an identity and a domain has been set, an identity preference item will be created in the default keychain. Subsequently, calling SecIdentitySearchCreateWithPolicy and SecIdentitySearchCopyNext will return the preferred identity for this domain first.
--
-- ObjC selector: @- setDomain:@
setDomain :: (IsSFChooseIdentityPanel sfChooseIdentityPanel, IsNSString domainString) => sfChooseIdentityPanel -> domainString -> IO ()
setDomain sfChooseIdentityPanel  domainString =
withObjCPtr domainString $ \raw_domainString ->
    sendMsg sfChooseIdentityPanel (mkSelector "setDomain:") retVoid [argPtr (castPtr raw_domainString :: Ptr ())]

-- | domain
--
-- Returns the domain which will be associated with the chosen identity.
--
-- ObjC selector: @- domain@
domain :: IsSFChooseIdentityPanel sfChooseIdentityPanel => sfChooseIdentityPanel -> IO (Id NSString)
domain sfChooseIdentityPanel  =
  sendMsg sfChooseIdentityPanel (mkSelector "domain") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedChooseIdentityPanel@
sharedChooseIdentityPanelSelector :: Selector
sharedChooseIdentityPanelSelector = mkSelector "sharedChooseIdentityPanel"

-- | @Selector@ for @runModalForIdentities:message:@
runModalForIdentities_messageSelector :: Selector
runModalForIdentities_messageSelector = mkSelector "runModalForIdentities:message:"

-- | @Selector@ for @beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:identities:message:@
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_identities_messageSelector :: Selector
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_identities_messageSelector = mkSelector "beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:identities:message:"

-- | @Selector@ for @identity@
identitySelector :: Selector
identitySelector = mkSelector "identity"

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

-- | @Selector@ for @setInformativeText:@
setInformativeTextSelector :: Selector
setInformativeTextSelector = mkSelector "setInformativeText:"

-- | @Selector@ for @informativeText@
informativeTextSelector :: Selector
informativeTextSelector = mkSelector "informativeText"

-- | @Selector@ for @setDomain:@
setDomainSelector :: Selector
setDomainSelector = mkSelector "setDomain:"

-- | @Selector@ for @domain@
domainSelector :: Selector
domainSelector = mkSelector "domain"

