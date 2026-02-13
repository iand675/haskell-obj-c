{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SWHighlightCenter
--
-- Provides the application with a priority-ordered list of universal links which have been shared with the current user.
--
-- The system decides which links should be surfaced. The app is responsible for updating its UI to reflect the latest provided list.
--
-- Generated bindings for @SWHighlightCenter@.
module ObjC.SharedWithYou.SWHighlightCenter
  ( SWHighlightCenter
  , IsSWHighlightCenter(..)
  , getHighlightForURL_completionHandler
  , collaborationHighlightForIdentifier_error
  , getCollaborationHighlightForURL_completionHandler
  , postNoticeForHighlightEvent
  , clearNoticesForHighlight
  , getSignedIdentityProofForCollaborationHighlight_usingData_completionHandler
  , delegate
  , setDelegate
  , highlights
  , highlightCollectionTitle
  , systemCollaborationSupportAvailable
  , clearNoticesForHighlightSelector
  , collaborationHighlightForIdentifier_errorSelector
  , delegateSelector
  , getCollaborationHighlightForURL_completionHandlerSelector
  , getHighlightForURL_completionHandlerSelector
  , getSignedIdentityProofForCollaborationHighlight_usingData_completionHandlerSelector
  , highlightCollectionTitleSelector
  , highlightsSelector
  , postNoticeForHighlightEventSelector
  , setDelegateSelector
  , systemCollaborationSupportAvailableSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SharedWithYou.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A convenience method to get a SWHighlight for a given URL
--
-- @URL@ — The URL used to find the SWHighlight
--
-- @completionHandler@ — an SWHighlight if it  was fetched. The completion handler will always be invoked on the main queue
--
-- ObjC selector: @- getHighlightForURL:completionHandler:@
getHighlightForURL_completionHandler :: (IsSWHighlightCenter swHighlightCenter, IsNSURL url) => swHighlightCenter -> url -> Ptr () -> IO ()
getHighlightForURL_completionHandler swHighlightCenter url completionHandler =
  sendMessage swHighlightCenter getHighlightForURL_completionHandlerSelector (toNSURL url) completionHandler

-- | @- collaborationHighlightForIdentifier:error:@
collaborationHighlightForIdentifier_error :: (IsSWHighlightCenter swHighlightCenter, IsNSString collaborationIdentifier, IsNSError error_) => swHighlightCenter -> collaborationIdentifier -> error_ -> IO (Id SWCollaborationHighlight)
collaborationHighlightForIdentifier_error swHighlightCenter collaborationIdentifier error_ =
  sendMessage swHighlightCenter collaborationHighlightForIdentifier_errorSelector (toNSString collaborationIdentifier) (toNSError error_)

-- | A convenience method to get an SWCollaborationHighlight for a given URL
--
-- @URL@ — The URL used to find the SWCollaborationHighlight
--
-- @completionHandler@ — an SWCollaborationHighlight if it was fetched. The completion handler will always be invoked on the main queue
--
-- ObjC selector: @- getCollaborationHighlightForURL:completionHandler:@
getCollaborationHighlightForURL_completionHandler :: (IsSWHighlightCenter swHighlightCenter, IsNSURL url) => swHighlightCenter -> url -> Ptr () -> IO ()
getCollaborationHighlightForURL_completionHandler swHighlightCenter url completionHandler =
  sendMessage swHighlightCenter getCollaborationHighlightForURL_completionHandlerSelector (toNSURL url) completionHandler

-- | Post a given event to the highlight center for display in Messages.
--
-- @event@ — The event to add for a specific highlight
--
-- ObjC selector: @- postNoticeForHighlightEvent:@
postNoticeForHighlightEvent :: IsSWHighlightCenter swHighlightCenter => swHighlightCenter -> RawId -> IO ()
postNoticeForHighlightEvent swHighlightCenter event =
  sendMessage swHighlightCenter postNoticeForHighlightEventSelector event

-- | Clear notices for a given collaboration highlight in Messages.
--
-- @highlight@ — The highlight to clear notices from.
--
-- ObjC selector: @- clearNoticesForHighlight:@
clearNoticesForHighlight :: (IsSWHighlightCenter swHighlightCenter, IsSWCollaborationHighlight highlight) => swHighlightCenter -> highlight -> IO ()
clearNoticesForHighlight swHighlightCenter highlight =
  sendMessage swHighlightCenter clearNoticesForHighlightSelector (toSWCollaborationHighlight highlight)

-- | Method to sign passed in data with local device's private key
--
-- @data@ — NSData that needs to be signed
--
-- @collaborationHighlight@ — The corresponding collaboration highlight.
--
-- @completionHandler@ — Signed data along with proof of inclusion for merkle if signing succeeded, otherwise an error. The completion handler will always be invoked on main queue
--
-- ObjC selector: @- getSignedIdentityProofForCollaborationHighlight:usingData:completionHandler:@
getSignedIdentityProofForCollaborationHighlight_usingData_completionHandler :: (IsSWHighlightCenter swHighlightCenter, IsSWCollaborationHighlight collaborationHighlight, IsNSData data_) => swHighlightCenter -> collaborationHighlight -> data_ -> Ptr () -> IO ()
getSignedIdentityProofForCollaborationHighlight_usingData_completionHandler swHighlightCenter collaborationHighlight data_ completionHandler =
  sendMessage swHighlightCenter getSignedIdentityProofForCollaborationHighlight_usingData_completionHandlerSelector (toSWCollaborationHighlight collaborationHighlight) (toNSData data_) completionHandler

-- | The highlight center's delegate
--
-- ObjC selector: @- delegate@
delegate :: IsSWHighlightCenter swHighlightCenter => swHighlightCenter -> IO RawId
delegate swHighlightCenter =
  sendMessage swHighlightCenter delegateSelector

-- | The highlight center's delegate
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsSWHighlightCenter swHighlightCenter => swHighlightCenter -> RawId -> IO ()
setDelegate swHighlightCenter value =
  sendMessage swHighlightCenter setDelegateSelector value

-- | @- highlights@
highlights :: IsSWHighlightCenter swHighlightCenter => swHighlightCenter -> IO (Id NSArray)
highlights swHighlightCenter =
  sendMessage swHighlightCenter highlightsSelector

-- | Localized title to display with a collection of highlights
--
-- Use this string as the title for a collection of shared highlight links displayed to the user.
--
-- ObjC selector: @+ highlightCollectionTitle@
highlightCollectionTitle :: IO (Id NSString)
highlightCollectionTitle  =
  do
    cls' <- getRequiredClass "SWHighlightCenter"
    sendClassMessage cls' highlightCollectionTitleSelector

-- | Whether the current software version has full support for Messages collaboration features.
--
-- Use this property at runtime to conditionally enable Messages collaboration features in your app. This property will be permantently set to YES on a software version with full support for these features.
--
-- ObjC selector: @+ systemCollaborationSupportAvailable@
systemCollaborationSupportAvailable :: IO Bool
systemCollaborationSupportAvailable  =
  do
    cls' <- getRequiredClass "SWHighlightCenter"
    sendClassMessage cls' systemCollaborationSupportAvailableSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getHighlightForURL:completionHandler:@
getHighlightForURL_completionHandlerSelector :: Selector '[Id NSURL, Ptr ()] ()
getHighlightForURL_completionHandlerSelector = mkSelector "getHighlightForURL:completionHandler:"

-- | @Selector@ for @collaborationHighlightForIdentifier:error:@
collaborationHighlightForIdentifier_errorSelector :: Selector '[Id NSString, Id NSError] (Id SWCollaborationHighlight)
collaborationHighlightForIdentifier_errorSelector = mkSelector "collaborationHighlightForIdentifier:error:"

-- | @Selector@ for @getCollaborationHighlightForURL:completionHandler:@
getCollaborationHighlightForURL_completionHandlerSelector :: Selector '[Id NSURL, Ptr ()] ()
getCollaborationHighlightForURL_completionHandlerSelector = mkSelector "getCollaborationHighlightForURL:completionHandler:"

-- | @Selector@ for @postNoticeForHighlightEvent:@
postNoticeForHighlightEventSelector :: Selector '[RawId] ()
postNoticeForHighlightEventSelector = mkSelector "postNoticeForHighlightEvent:"

-- | @Selector@ for @clearNoticesForHighlight:@
clearNoticesForHighlightSelector :: Selector '[Id SWCollaborationHighlight] ()
clearNoticesForHighlightSelector = mkSelector "clearNoticesForHighlight:"

-- | @Selector@ for @getSignedIdentityProofForCollaborationHighlight:usingData:completionHandler:@
getSignedIdentityProofForCollaborationHighlight_usingData_completionHandlerSelector :: Selector '[Id SWCollaborationHighlight, Id NSData, Ptr ()] ()
getSignedIdentityProofForCollaborationHighlight_usingData_completionHandlerSelector = mkSelector "getSignedIdentityProofForCollaborationHighlight:usingData:completionHandler:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @highlights@
highlightsSelector :: Selector '[] (Id NSArray)
highlightsSelector = mkSelector "highlights"

-- | @Selector@ for @highlightCollectionTitle@
highlightCollectionTitleSelector :: Selector '[] (Id NSString)
highlightCollectionTitleSelector = mkSelector "highlightCollectionTitle"

-- | @Selector@ for @systemCollaborationSupportAvailable@
systemCollaborationSupportAvailableSelector :: Selector '[] Bool
systemCollaborationSupportAvailableSelector = mkSelector "systemCollaborationSupportAvailable"

