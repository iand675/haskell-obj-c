{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SWCollaborationView@.
module ObjC.SharedWithYou.SWCollaborationView
  ( SWCollaborationView
  , IsSWCollaborationView(..)
  , setContentView
  , initWithItemProvider
  , dismissPopover
  , setShowManageButton
  , cloudSharingDelegate
  , setCloudSharingDelegate
  , activeParticipantCount
  , setActiveParticipantCount
  , delegate
  , setDelegate
  , headerTitle
  , setHeaderTitle
  , headerSubtitle
  , setHeaderSubtitle
  , headerImage
  , setHeaderImage
  , menuFormRepresentation
  , cloudSharingServiceDelegate
  , setCloudSharingServiceDelegate
  , manageButtonTitle
  , setManageButtonTitle
  , activeParticipantCountSelector
  , cloudSharingDelegateSelector
  , cloudSharingServiceDelegateSelector
  , delegateSelector
  , dismissPopoverSelector
  , headerImageSelector
  , headerSubtitleSelector
  , headerTitleSelector
  , initWithItemProviderSelector
  , manageButtonTitleSelector
  , menuFormRepresentationSelector
  , setActiveParticipantCountSelector
  , setCloudSharingDelegateSelector
  , setCloudSharingServiceDelegateSelector
  , setContentViewSelector
  , setDelegateSelector
  , setHeaderImageSelector
  , setHeaderSubtitleSelector
  , setHeaderTitleSelector
  , setManageButtonTitleSelector
  , setShowManageButtonSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SharedWithYou.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- setContentView:@
setContentView :: (IsSWCollaborationView swCollaborationView, IsNSView detailViewListContentView) => swCollaborationView -> detailViewListContentView -> IO ()
setContentView swCollaborationView detailViewListContentView =
  sendMessage swCollaborationView setContentViewSelector (toNSView detailViewListContentView)

-- | @- initWithItemProvider:@
initWithItemProvider :: (IsSWCollaborationView swCollaborationView, IsNSItemProvider itemProvider) => swCollaborationView -> itemProvider -> IO (Id SWCollaborationView)
initWithItemProvider swCollaborationView itemProvider =
  sendOwnedMessage swCollaborationView initWithItemProviderSelector (toNSItemProvider itemProvider)

-- | Dismisses the popover, if presented.
--
-- @completion@ — Called when the popover dismissal finishes.
--
-- ObjC selector: @- dismissPopover:@
dismissPopover :: IsSWCollaborationView swCollaborationView => swCollaborationView -> Ptr () -> IO ()
dismissPopover swCollaborationView completion =
  sendMessage swCollaborationView dismissPopoverSelector completion

-- | whether the collaboration popover should show the default manage participants button in the popover, defaults to YES
--
-- @showManageButton@ — whether the button should be hidden
--
-- ObjC selector: @- setShowManageButton:@
setShowManageButton :: IsSWCollaborationView swCollaborationView => swCollaborationView -> Bool -> IO ()
setShowManageButton swCollaborationView showManageButton =
  sendMessage swCollaborationView setShowManageButtonSelector showManageButton

-- | @- cloudSharingDelegate@
cloudSharingDelegate :: IsSWCollaborationView swCollaborationView => swCollaborationView -> IO RawId
cloudSharingDelegate swCollaborationView =
  sendMessage swCollaborationView cloudSharingDelegateSelector

-- | @- setCloudSharingDelegate:@
setCloudSharingDelegate :: IsSWCollaborationView swCollaborationView => swCollaborationView -> RawId -> IO ()
setCloudSharingDelegate swCollaborationView value =
  sendMessage swCollaborationView setCloudSharingDelegateSelector value

-- | @- activeParticipantCount@
activeParticipantCount :: IsSWCollaborationView swCollaborationView => swCollaborationView -> IO CULong
activeParticipantCount swCollaborationView =
  sendMessage swCollaborationView activeParticipantCountSelector

-- | @- setActiveParticipantCount:@
setActiveParticipantCount :: IsSWCollaborationView swCollaborationView => swCollaborationView -> CULong -> IO ()
setActiveParticipantCount swCollaborationView value =
  sendMessage swCollaborationView setActiveParticipantCountSelector value

-- | @- delegate@
delegate :: IsSWCollaborationView swCollaborationView => swCollaborationView -> IO RawId
delegate swCollaborationView =
  sendMessage swCollaborationView delegateSelector

-- | @- setDelegate:@
setDelegate :: IsSWCollaborationView swCollaborationView => swCollaborationView -> RawId -> IO ()
setDelegate swCollaborationView value =
  sendMessage swCollaborationView setDelegateSelector value

-- | @- headerTitle@
headerTitle :: IsSWCollaborationView swCollaborationView => swCollaborationView -> IO (Id NSString)
headerTitle swCollaborationView =
  sendMessage swCollaborationView headerTitleSelector

-- | @- setHeaderTitle:@
setHeaderTitle :: (IsSWCollaborationView swCollaborationView, IsNSString value) => swCollaborationView -> value -> IO ()
setHeaderTitle swCollaborationView value =
  sendMessage swCollaborationView setHeaderTitleSelector (toNSString value)

-- | @- headerSubtitle@
headerSubtitle :: IsSWCollaborationView swCollaborationView => swCollaborationView -> IO (Id NSString)
headerSubtitle swCollaborationView =
  sendMessage swCollaborationView headerSubtitleSelector

-- | @- setHeaderSubtitle:@
setHeaderSubtitle :: (IsSWCollaborationView swCollaborationView, IsNSString value) => swCollaborationView -> value -> IO ()
setHeaderSubtitle swCollaborationView value =
  sendMessage swCollaborationView setHeaderSubtitleSelector (toNSString value)

-- | @- headerImage@
headerImage :: IsSWCollaborationView swCollaborationView => swCollaborationView -> IO (Id NSImage)
headerImage swCollaborationView =
  sendMessage swCollaborationView headerImageSelector

-- | @- setHeaderImage:@
setHeaderImage :: (IsSWCollaborationView swCollaborationView, IsNSImage value) => swCollaborationView -> value -> IO ()
setHeaderImage swCollaborationView value =
  sendMessage swCollaborationView setHeaderImageSelector (toNSImage value)

-- | @- menuFormRepresentation@
menuFormRepresentation :: IsSWCollaborationView swCollaborationView => swCollaborationView -> IO (Id NSMenuItem)
menuFormRepresentation swCollaborationView =
  sendMessage swCollaborationView menuFormRepresentationSelector

-- | If you are using the built in manage share button, this delegate property will be forwarded along to the NSCloudSharingService that button presents. If you have your own and suppress the provided one via setShowManageButton, this does nothing.
--
-- ObjC selector: @- cloudSharingServiceDelegate@
cloudSharingServiceDelegate :: IsSWCollaborationView swCollaborationView => swCollaborationView -> IO RawId
cloudSharingServiceDelegate swCollaborationView =
  sendMessage swCollaborationView cloudSharingServiceDelegateSelector

-- | If you are using the built in manage share button, this delegate property will be forwarded along to the NSCloudSharingService that button presents. If you have your own and suppress the provided one via setShowManageButton, this does nothing.
--
-- ObjC selector: @- setCloudSharingServiceDelegate:@
setCloudSharingServiceDelegate :: IsSWCollaborationView swCollaborationView => swCollaborationView -> RawId -> IO ()
setCloudSharingServiceDelegate swCollaborationView value =
  sendMessage swCollaborationView setCloudSharingServiceDelegateSelector value

-- | sets the title of the manage participants button in the collaboration popover to the given string, defaults to "Manage Share"
--
-- ObjC selector: @- manageButtonTitle@
manageButtonTitle :: IsSWCollaborationView swCollaborationView => swCollaborationView -> IO (Id NSString)
manageButtonTitle swCollaborationView =
  sendMessage swCollaborationView manageButtonTitleSelector

-- | sets the title of the manage participants button in the collaboration popover to the given string, defaults to "Manage Share"
--
-- ObjC selector: @- setManageButtonTitle:@
setManageButtonTitle :: (IsSWCollaborationView swCollaborationView, IsNSString value) => swCollaborationView -> value -> IO ()
setManageButtonTitle swCollaborationView value =
  sendMessage swCollaborationView setManageButtonTitleSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setContentView:@
setContentViewSelector :: Selector '[Id NSView] ()
setContentViewSelector = mkSelector "setContentView:"

-- | @Selector@ for @initWithItemProvider:@
initWithItemProviderSelector :: Selector '[Id NSItemProvider] (Id SWCollaborationView)
initWithItemProviderSelector = mkSelector "initWithItemProvider:"

-- | @Selector@ for @dismissPopover:@
dismissPopoverSelector :: Selector '[Ptr ()] ()
dismissPopoverSelector = mkSelector "dismissPopover:"

-- | @Selector@ for @setShowManageButton:@
setShowManageButtonSelector :: Selector '[Bool] ()
setShowManageButtonSelector = mkSelector "setShowManageButton:"

-- | @Selector@ for @cloudSharingDelegate@
cloudSharingDelegateSelector :: Selector '[] RawId
cloudSharingDelegateSelector = mkSelector "cloudSharingDelegate"

-- | @Selector@ for @setCloudSharingDelegate:@
setCloudSharingDelegateSelector :: Selector '[RawId] ()
setCloudSharingDelegateSelector = mkSelector "setCloudSharingDelegate:"

-- | @Selector@ for @activeParticipantCount@
activeParticipantCountSelector :: Selector '[] CULong
activeParticipantCountSelector = mkSelector "activeParticipantCount"

-- | @Selector@ for @setActiveParticipantCount:@
setActiveParticipantCountSelector :: Selector '[CULong] ()
setActiveParticipantCountSelector = mkSelector "setActiveParticipantCount:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @headerTitle@
headerTitleSelector :: Selector '[] (Id NSString)
headerTitleSelector = mkSelector "headerTitle"

-- | @Selector@ for @setHeaderTitle:@
setHeaderTitleSelector :: Selector '[Id NSString] ()
setHeaderTitleSelector = mkSelector "setHeaderTitle:"

-- | @Selector@ for @headerSubtitle@
headerSubtitleSelector :: Selector '[] (Id NSString)
headerSubtitleSelector = mkSelector "headerSubtitle"

-- | @Selector@ for @setHeaderSubtitle:@
setHeaderSubtitleSelector :: Selector '[Id NSString] ()
setHeaderSubtitleSelector = mkSelector "setHeaderSubtitle:"

-- | @Selector@ for @headerImage@
headerImageSelector :: Selector '[] (Id NSImage)
headerImageSelector = mkSelector "headerImage"

-- | @Selector@ for @setHeaderImage:@
setHeaderImageSelector :: Selector '[Id NSImage] ()
setHeaderImageSelector = mkSelector "setHeaderImage:"

-- | @Selector@ for @menuFormRepresentation@
menuFormRepresentationSelector :: Selector '[] (Id NSMenuItem)
menuFormRepresentationSelector = mkSelector "menuFormRepresentation"

-- | @Selector@ for @cloudSharingServiceDelegate@
cloudSharingServiceDelegateSelector :: Selector '[] RawId
cloudSharingServiceDelegateSelector = mkSelector "cloudSharingServiceDelegate"

-- | @Selector@ for @setCloudSharingServiceDelegate:@
setCloudSharingServiceDelegateSelector :: Selector '[RawId] ()
setCloudSharingServiceDelegateSelector = mkSelector "setCloudSharingServiceDelegate:"

-- | @Selector@ for @manageButtonTitle@
manageButtonTitleSelector :: Selector '[] (Id NSString)
manageButtonTitleSelector = mkSelector "manageButtonTitle"

-- | @Selector@ for @setManageButtonTitle:@
setManageButtonTitleSelector :: Selector '[Id NSString] ()
setManageButtonTitleSelector = mkSelector "setManageButtonTitle:"

