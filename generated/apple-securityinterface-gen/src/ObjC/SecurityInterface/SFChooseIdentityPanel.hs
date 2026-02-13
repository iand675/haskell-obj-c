{-# LANGUAGE DataKinds #-}
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
  , beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_identities_messageSelector
  , domainSelector
  , helpAnchorSelector
  , identitySelector
  , informativeTextSelector
  , policiesSelector
  , runModalForIdentities_messageSelector
  , setAlternateButtonTitleSelector
  , setDefaultButtonTitleSelector
  , setDomainSelector
  , setHelpAnchorSelector
  , setInformativeTextSelector
  , setPoliciesSelector
  , setShowsHelpSelector
  , sharedChooseIdentityPanelSelector
  , showsHelpSelector


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
    sendClassMessage cls' sharedChooseIdentityPanelSelector

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
runModalForIdentities_message sfChooseIdentityPanel identities message =
  sendMessage sfChooseIdentityPanel runModalForIdentities_messageSelector (toNSArray identities) (toNSString message)

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
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_identities_message :: (IsSFChooseIdentityPanel sfChooseIdentityPanel, IsNSWindow docWindow, IsNSArray identities, IsNSString message) => sfChooseIdentityPanel -> docWindow -> RawId -> Sel -> Ptr () -> identities -> message -> IO ()
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_identities_message sfChooseIdentityPanel docWindow delegate didEndSelector contextInfo identities message =
  sendMessage sfChooseIdentityPanel beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_identities_messageSelector (toNSWindow docWindow) delegate didEndSelector contextInfo (toNSArray identities) (toNSString message)

-- | identity
--
-- Returns the identity that the user chose in the panel.
--
-- ObjC selector: @- identity@
identity :: IsSFChooseIdentityPanel sfChooseIdentityPanel => sfChooseIdentityPanel -> IO (Ptr ())
identity sfChooseIdentityPanel =
  sendMessage sfChooseIdentityPanel identitySelector

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
setPolicies sfChooseIdentityPanel policies =
  sendMessage sfChooseIdentityPanel setPoliciesSelector policies

-- | policies
--
-- Returns an array of policies used to evaluate the status of the displayed certificates.
--
-- This method returns an autoreleased NSArray containing one or more SecPolicyRef instances, as set by a previous setPolicies: call.
--
-- ObjC selector: @- policies@
policies :: IsSFChooseIdentityPanel sfChooseIdentityPanel => sfChooseIdentityPanel -> IO (Id NSArray)
policies sfChooseIdentityPanel =
  sendMessage sfChooseIdentityPanel policiesSelector

-- | setDefaultButtonTitle:
--
-- Customizes the title of the default button.
--
-- @title@ — The new title for the default button.
--
-- ObjC selector: @- setDefaultButtonTitle:@
setDefaultButtonTitle :: (IsSFChooseIdentityPanel sfChooseIdentityPanel, IsNSString title) => sfChooseIdentityPanel -> title -> IO ()
setDefaultButtonTitle sfChooseIdentityPanel title =
  sendMessage sfChooseIdentityPanel setDefaultButtonTitleSelector (toNSString title)

-- | setAlternateButtonTitle:
--
-- Customizes the title of the alternate button.
--
-- @title@ — The new title for the alternate button. If title is set to nil, the button will not be shown.
--
-- ObjC selector: @- setAlternateButtonTitle:@
setAlternateButtonTitle :: (IsSFChooseIdentityPanel sfChooseIdentityPanel, IsNSString title) => sfChooseIdentityPanel -> title -> IO ()
setAlternateButtonTitle sfChooseIdentityPanel title =
  sendMessage sfChooseIdentityPanel setAlternateButtonTitleSelector (toNSString title)

-- | @- setShowsHelp:@
setShowsHelp :: IsSFChooseIdentityPanel sfChooseIdentityPanel => sfChooseIdentityPanel -> Bool -> IO ()
setShowsHelp sfChooseIdentityPanel showsHelp =
  sendMessage sfChooseIdentityPanel setShowsHelpSelector showsHelp

-- | @- showsHelp@
showsHelp :: IsSFChooseIdentityPanel sfChooseIdentityPanel => sfChooseIdentityPanel -> IO Bool
showsHelp sfChooseIdentityPanel =
  sendMessage sfChooseIdentityPanel showsHelpSelector

-- | @- setHelpAnchor:@
setHelpAnchor :: (IsSFChooseIdentityPanel sfChooseIdentityPanel, IsNSString anchor) => sfChooseIdentityPanel -> anchor -> IO ()
setHelpAnchor sfChooseIdentityPanel anchor =
  sendMessage sfChooseIdentityPanel setHelpAnchorSelector (toNSString anchor)

-- | @- helpAnchor@
helpAnchor :: IsSFChooseIdentityPanel sfChooseIdentityPanel => sfChooseIdentityPanel -> IO (Id NSString)
helpAnchor sfChooseIdentityPanel =
  sendMessage sfChooseIdentityPanel helpAnchorSelector

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
setInformativeText sfChooseIdentityPanel informativeText =
  sendMessage sfChooseIdentityPanel setInformativeTextSelector (toNSString informativeText)

-- | informativeText
--
-- Returns the informative text currently displayed in the SFChooseIdentityPanel.
--
-- ObjC selector: @- informativeText@
informativeText :: IsSFChooseIdentityPanel sfChooseIdentityPanel => sfChooseIdentityPanel -> IO (Id NSString)
informativeText sfChooseIdentityPanel =
  sendMessage sfChooseIdentityPanel informativeTextSelector

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
setDomain sfChooseIdentityPanel domainString =
  sendMessage sfChooseIdentityPanel setDomainSelector (toNSString domainString)

-- | domain
--
-- Returns the domain which will be associated with the chosen identity.
--
-- ObjC selector: @- domain@
domain :: IsSFChooseIdentityPanel sfChooseIdentityPanel => sfChooseIdentityPanel -> IO (Id NSString)
domain sfChooseIdentityPanel =
  sendMessage sfChooseIdentityPanel domainSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedChooseIdentityPanel@
sharedChooseIdentityPanelSelector :: Selector '[] (Id SFChooseIdentityPanel)
sharedChooseIdentityPanelSelector = mkSelector "sharedChooseIdentityPanel"

-- | @Selector@ for @runModalForIdentities:message:@
runModalForIdentities_messageSelector :: Selector '[Id NSArray, Id NSString] CLong
runModalForIdentities_messageSelector = mkSelector "runModalForIdentities:message:"

-- | @Selector@ for @beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:identities:message:@
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_identities_messageSelector :: Selector '[Id NSWindow, RawId, Sel, Ptr (), Id NSArray, Id NSString] ()
beginSheetForWindow_modalDelegate_didEndSelector_contextInfo_identities_messageSelector = mkSelector "beginSheetForWindow:modalDelegate:didEndSelector:contextInfo:identities:message:"

-- | @Selector@ for @identity@
identitySelector :: Selector '[] (Ptr ())
identitySelector = mkSelector "identity"

-- | @Selector@ for @setPolicies:@
setPoliciesSelector :: Selector '[RawId] ()
setPoliciesSelector = mkSelector "setPolicies:"

-- | @Selector@ for @policies@
policiesSelector :: Selector '[] (Id NSArray)
policiesSelector = mkSelector "policies"

-- | @Selector@ for @setDefaultButtonTitle:@
setDefaultButtonTitleSelector :: Selector '[Id NSString] ()
setDefaultButtonTitleSelector = mkSelector "setDefaultButtonTitle:"

-- | @Selector@ for @setAlternateButtonTitle:@
setAlternateButtonTitleSelector :: Selector '[Id NSString] ()
setAlternateButtonTitleSelector = mkSelector "setAlternateButtonTitle:"

-- | @Selector@ for @setShowsHelp:@
setShowsHelpSelector :: Selector '[Bool] ()
setShowsHelpSelector = mkSelector "setShowsHelp:"

-- | @Selector@ for @showsHelp@
showsHelpSelector :: Selector '[] Bool
showsHelpSelector = mkSelector "showsHelp"

-- | @Selector@ for @setHelpAnchor:@
setHelpAnchorSelector :: Selector '[Id NSString] ()
setHelpAnchorSelector = mkSelector "setHelpAnchor:"

-- | @Selector@ for @helpAnchor@
helpAnchorSelector :: Selector '[] (Id NSString)
helpAnchorSelector = mkSelector "helpAnchor"

-- | @Selector@ for @setInformativeText:@
setInformativeTextSelector :: Selector '[Id NSString] ()
setInformativeTextSelector = mkSelector "setInformativeText:"

-- | @Selector@ for @informativeText@
informativeTextSelector :: Selector '[] (Id NSString)
informativeTextSelector = mkSelector "informativeText"

-- | @Selector@ for @setDomain:@
setDomainSelector :: Selector '[Id NSString] ()
setDomainSelector = mkSelector "setDomain:"

-- | @Selector@ for @domain@
domainSelector :: Selector '[] (Id NSString)
domainSelector = mkSelector "domain"

