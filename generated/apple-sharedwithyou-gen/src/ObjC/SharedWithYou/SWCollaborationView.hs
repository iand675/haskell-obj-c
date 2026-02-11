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
  , setContentViewSelector
  , initWithItemProviderSelector
  , dismissPopoverSelector
  , setShowManageButtonSelector
  , cloudSharingDelegateSelector
  , setCloudSharingDelegateSelector
  , activeParticipantCountSelector
  , setActiveParticipantCountSelector
  , delegateSelector
  , setDelegateSelector
  , headerTitleSelector
  , setHeaderTitleSelector
  , headerSubtitleSelector
  , setHeaderSubtitleSelector
  , headerImageSelector
  , setHeaderImageSelector
  , menuFormRepresentationSelector
  , cloudSharingServiceDelegateSelector
  , setCloudSharingServiceDelegateSelector
  , manageButtonTitleSelector
  , setManageButtonTitleSelector


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

import ObjC.SharedWithYou.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- setContentView:@
setContentView :: (IsSWCollaborationView swCollaborationView, IsNSView detailViewListContentView) => swCollaborationView -> detailViewListContentView -> IO ()
setContentView swCollaborationView  detailViewListContentView =
  withObjCPtr detailViewListContentView $ \raw_detailViewListContentView ->
      sendMsg swCollaborationView (mkSelector "setContentView:") retVoid [argPtr (castPtr raw_detailViewListContentView :: Ptr ())]

-- | @- initWithItemProvider:@
initWithItemProvider :: (IsSWCollaborationView swCollaborationView, IsNSItemProvider itemProvider) => swCollaborationView -> itemProvider -> IO (Id SWCollaborationView)
initWithItemProvider swCollaborationView  itemProvider =
  withObjCPtr itemProvider $ \raw_itemProvider ->
      sendMsg swCollaborationView (mkSelector "initWithItemProvider:") (retPtr retVoid) [argPtr (castPtr raw_itemProvider :: Ptr ())] >>= ownedObject . castPtr

-- | Dismisses the popover, if presented.
--
-- @completion@ — Called when the popover dismissal finishes.
--
-- ObjC selector: @- dismissPopover:@
dismissPopover :: IsSWCollaborationView swCollaborationView => swCollaborationView -> Ptr () -> IO ()
dismissPopover swCollaborationView  completion =
    sendMsg swCollaborationView (mkSelector "dismissPopover:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | whether the collaboration popover should show the default manage participants button in the popover, defaults to YES
--
-- @showManageButton@ — whether the button should be hidden
--
-- ObjC selector: @- setShowManageButton:@
setShowManageButton :: IsSWCollaborationView swCollaborationView => swCollaborationView -> Bool -> IO ()
setShowManageButton swCollaborationView  showManageButton =
    sendMsg swCollaborationView (mkSelector "setShowManageButton:") retVoid [argCULong (if showManageButton then 1 else 0)]

-- | @- cloudSharingDelegate@
cloudSharingDelegate :: IsSWCollaborationView swCollaborationView => swCollaborationView -> IO RawId
cloudSharingDelegate swCollaborationView  =
    fmap (RawId . castPtr) $ sendMsg swCollaborationView (mkSelector "cloudSharingDelegate") (retPtr retVoid) []

-- | @- setCloudSharingDelegate:@
setCloudSharingDelegate :: IsSWCollaborationView swCollaborationView => swCollaborationView -> RawId -> IO ()
setCloudSharingDelegate swCollaborationView  value =
    sendMsg swCollaborationView (mkSelector "setCloudSharingDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- activeParticipantCount@
activeParticipantCount :: IsSWCollaborationView swCollaborationView => swCollaborationView -> IO CULong
activeParticipantCount swCollaborationView  =
    sendMsg swCollaborationView (mkSelector "activeParticipantCount") retCULong []

-- | @- setActiveParticipantCount:@
setActiveParticipantCount :: IsSWCollaborationView swCollaborationView => swCollaborationView -> CULong -> IO ()
setActiveParticipantCount swCollaborationView  value =
    sendMsg swCollaborationView (mkSelector "setActiveParticipantCount:") retVoid [argCULong value]

-- | @- delegate@
delegate :: IsSWCollaborationView swCollaborationView => swCollaborationView -> IO RawId
delegate swCollaborationView  =
    fmap (RawId . castPtr) $ sendMsg swCollaborationView (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsSWCollaborationView swCollaborationView => swCollaborationView -> RawId -> IO ()
setDelegate swCollaborationView  value =
    sendMsg swCollaborationView (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- headerTitle@
headerTitle :: IsSWCollaborationView swCollaborationView => swCollaborationView -> IO (Id NSString)
headerTitle swCollaborationView  =
    sendMsg swCollaborationView (mkSelector "headerTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHeaderTitle:@
setHeaderTitle :: (IsSWCollaborationView swCollaborationView, IsNSString value) => swCollaborationView -> value -> IO ()
setHeaderTitle swCollaborationView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg swCollaborationView (mkSelector "setHeaderTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- headerSubtitle@
headerSubtitle :: IsSWCollaborationView swCollaborationView => swCollaborationView -> IO (Id NSString)
headerSubtitle swCollaborationView  =
    sendMsg swCollaborationView (mkSelector "headerSubtitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHeaderSubtitle:@
setHeaderSubtitle :: (IsSWCollaborationView swCollaborationView, IsNSString value) => swCollaborationView -> value -> IO ()
setHeaderSubtitle swCollaborationView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg swCollaborationView (mkSelector "setHeaderSubtitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- headerImage@
headerImage :: IsSWCollaborationView swCollaborationView => swCollaborationView -> IO (Id NSImage)
headerImage swCollaborationView  =
    sendMsg swCollaborationView (mkSelector "headerImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHeaderImage:@
setHeaderImage :: (IsSWCollaborationView swCollaborationView, IsNSImage value) => swCollaborationView -> value -> IO ()
setHeaderImage swCollaborationView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg swCollaborationView (mkSelector "setHeaderImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- menuFormRepresentation@
menuFormRepresentation :: IsSWCollaborationView swCollaborationView => swCollaborationView -> IO (Id NSMenuItem)
menuFormRepresentation swCollaborationView  =
    sendMsg swCollaborationView (mkSelector "menuFormRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If you are using the built in manage share button, this delegate property will be forwarded along to the NSCloudSharingService that button presents. If you have your own and suppress the provided one via setShowManageButton, this does nothing.
--
-- ObjC selector: @- cloudSharingServiceDelegate@
cloudSharingServiceDelegate :: IsSWCollaborationView swCollaborationView => swCollaborationView -> IO RawId
cloudSharingServiceDelegate swCollaborationView  =
    fmap (RawId . castPtr) $ sendMsg swCollaborationView (mkSelector "cloudSharingServiceDelegate") (retPtr retVoid) []

-- | If you are using the built in manage share button, this delegate property will be forwarded along to the NSCloudSharingService that button presents. If you have your own and suppress the provided one via setShowManageButton, this does nothing.
--
-- ObjC selector: @- setCloudSharingServiceDelegate:@
setCloudSharingServiceDelegate :: IsSWCollaborationView swCollaborationView => swCollaborationView -> RawId -> IO ()
setCloudSharingServiceDelegate swCollaborationView  value =
    sendMsg swCollaborationView (mkSelector "setCloudSharingServiceDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | sets the title of the manage participants button in the collaboration popover to the given string, defaults to "Manage Share"
--
-- ObjC selector: @- manageButtonTitle@
manageButtonTitle :: IsSWCollaborationView swCollaborationView => swCollaborationView -> IO (Id NSString)
manageButtonTitle swCollaborationView  =
    sendMsg swCollaborationView (mkSelector "manageButtonTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sets the title of the manage participants button in the collaboration popover to the given string, defaults to "Manage Share"
--
-- ObjC selector: @- setManageButtonTitle:@
setManageButtonTitle :: (IsSWCollaborationView swCollaborationView, IsNSString value) => swCollaborationView -> value -> IO ()
setManageButtonTitle swCollaborationView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg swCollaborationView (mkSelector "setManageButtonTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setContentView:@
setContentViewSelector :: Selector
setContentViewSelector = mkSelector "setContentView:"

-- | @Selector@ for @initWithItemProvider:@
initWithItemProviderSelector :: Selector
initWithItemProviderSelector = mkSelector "initWithItemProvider:"

-- | @Selector@ for @dismissPopover:@
dismissPopoverSelector :: Selector
dismissPopoverSelector = mkSelector "dismissPopover:"

-- | @Selector@ for @setShowManageButton:@
setShowManageButtonSelector :: Selector
setShowManageButtonSelector = mkSelector "setShowManageButton:"

-- | @Selector@ for @cloudSharingDelegate@
cloudSharingDelegateSelector :: Selector
cloudSharingDelegateSelector = mkSelector "cloudSharingDelegate"

-- | @Selector@ for @setCloudSharingDelegate:@
setCloudSharingDelegateSelector :: Selector
setCloudSharingDelegateSelector = mkSelector "setCloudSharingDelegate:"

-- | @Selector@ for @activeParticipantCount@
activeParticipantCountSelector :: Selector
activeParticipantCountSelector = mkSelector "activeParticipantCount"

-- | @Selector@ for @setActiveParticipantCount:@
setActiveParticipantCountSelector :: Selector
setActiveParticipantCountSelector = mkSelector "setActiveParticipantCount:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @headerTitle@
headerTitleSelector :: Selector
headerTitleSelector = mkSelector "headerTitle"

-- | @Selector@ for @setHeaderTitle:@
setHeaderTitleSelector :: Selector
setHeaderTitleSelector = mkSelector "setHeaderTitle:"

-- | @Selector@ for @headerSubtitle@
headerSubtitleSelector :: Selector
headerSubtitleSelector = mkSelector "headerSubtitle"

-- | @Selector@ for @setHeaderSubtitle:@
setHeaderSubtitleSelector :: Selector
setHeaderSubtitleSelector = mkSelector "setHeaderSubtitle:"

-- | @Selector@ for @headerImage@
headerImageSelector :: Selector
headerImageSelector = mkSelector "headerImage"

-- | @Selector@ for @setHeaderImage:@
setHeaderImageSelector :: Selector
setHeaderImageSelector = mkSelector "setHeaderImage:"

-- | @Selector@ for @menuFormRepresentation@
menuFormRepresentationSelector :: Selector
menuFormRepresentationSelector = mkSelector "menuFormRepresentation"

-- | @Selector@ for @cloudSharingServiceDelegate@
cloudSharingServiceDelegateSelector :: Selector
cloudSharingServiceDelegateSelector = mkSelector "cloudSharingServiceDelegate"

-- | @Selector@ for @setCloudSharingServiceDelegate:@
setCloudSharingServiceDelegateSelector :: Selector
setCloudSharingServiceDelegateSelector = mkSelector "setCloudSharingServiceDelegate:"

-- | @Selector@ for @manageButtonTitle@
manageButtonTitleSelector :: Selector
manageButtonTitleSelector = mkSelector "manageButtonTitle"

-- | @Selector@ for @setManageButtonTitle:@
setManageButtonTitleSelector :: Selector
setManageButtonTitleSelector = mkSelector "setManageButtonTitle:"

