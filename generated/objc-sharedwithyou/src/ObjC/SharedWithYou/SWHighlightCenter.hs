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
  , highlights
  , highlightCollectionTitle
  , systemCollaborationSupportAvailable
  , getHighlightForURL_completionHandlerSelector
  , collaborationHighlightForIdentifier_errorSelector
  , getCollaborationHighlightForURL_completionHandlerSelector
  , postNoticeForHighlightEventSelector
  , clearNoticesForHighlightSelector
  , getSignedIdentityProofForCollaborationHighlight_usingData_completionHandlerSelector
  , highlightsSelector
  , highlightCollectionTitleSelector
  , systemCollaborationSupportAvailableSelector


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
import ObjC.Foundation.Internal.Classes

-- | A convenience method to get a SWHighlight for a given URL
--
-- @URL@ — The URL used to find the SWHighlight
--
-- @completionHandler@ — an SWHighlight if it  was fetched. The completion handler will always be invoked on the main queue
--
-- ObjC selector: @- getHighlightForURL:completionHandler:@
getHighlightForURL_completionHandler :: (IsSWHighlightCenter swHighlightCenter, IsNSURL url) => swHighlightCenter -> url -> Ptr () -> IO ()
getHighlightForURL_completionHandler swHighlightCenter  url completionHandler =
withObjCPtr url $ \raw_url ->
    sendMsg swHighlightCenter (mkSelector "getHighlightForURL:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- collaborationHighlightForIdentifier:error:@
collaborationHighlightForIdentifier_error :: (IsSWHighlightCenter swHighlightCenter, IsNSString collaborationIdentifier, IsNSError error_) => swHighlightCenter -> collaborationIdentifier -> error_ -> IO (Id SWCollaborationHighlight)
collaborationHighlightForIdentifier_error swHighlightCenter  collaborationIdentifier error_ =
withObjCPtr collaborationIdentifier $ \raw_collaborationIdentifier ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg swHighlightCenter (mkSelector "collaborationHighlightForIdentifier:error:") (retPtr retVoid) [argPtr (castPtr raw_collaborationIdentifier :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | A convenience method to get an SWCollaborationHighlight for a given URL
--
-- @URL@ — The URL used to find the SWCollaborationHighlight
--
-- @completionHandler@ — an SWCollaborationHighlight if it was fetched. The completion handler will always be invoked on the main queue
--
-- ObjC selector: @- getCollaborationHighlightForURL:completionHandler:@
getCollaborationHighlightForURL_completionHandler :: (IsSWHighlightCenter swHighlightCenter, IsNSURL url) => swHighlightCenter -> url -> Ptr () -> IO ()
getCollaborationHighlightForURL_completionHandler swHighlightCenter  url completionHandler =
withObjCPtr url $ \raw_url ->
    sendMsg swHighlightCenter (mkSelector "getCollaborationHighlightForURL:completionHandler:") retVoid [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Post a given event to the highlight center for display in Messages.
--
-- @event@ — The event to add for a specific highlight
--
-- ObjC selector: @- postNoticeForHighlightEvent:@
postNoticeForHighlightEvent :: IsSWHighlightCenter swHighlightCenter => swHighlightCenter -> RawId -> IO ()
postNoticeForHighlightEvent swHighlightCenter  event =
  sendMsg swHighlightCenter (mkSelector "postNoticeForHighlightEvent:") retVoid [argPtr (castPtr (unRawId event) :: Ptr ())]

-- | Clear notices for a given collaboration highlight in Messages.
--
-- @highlight@ — The highlight to clear notices from.
--
-- ObjC selector: @- clearNoticesForHighlight:@
clearNoticesForHighlight :: (IsSWHighlightCenter swHighlightCenter, IsSWCollaborationHighlight highlight) => swHighlightCenter -> highlight -> IO ()
clearNoticesForHighlight swHighlightCenter  highlight =
withObjCPtr highlight $ \raw_highlight ->
    sendMsg swHighlightCenter (mkSelector "clearNoticesForHighlight:") retVoid [argPtr (castPtr raw_highlight :: Ptr ())]

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
getSignedIdentityProofForCollaborationHighlight_usingData_completionHandler swHighlightCenter  collaborationHighlight data_ completionHandler =
withObjCPtr collaborationHighlight $ \raw_collaborationHighlight ->
  withObjCPtr data_ $ \raw_data_ ->
      sendMsg swHighlightCenter (mkSelector "getSignedIdentityProofForCollaborationHighlight:usingData:completionHandler:") retVoid [argPtr (castPtr raw_collaborationHighlight :: Ptr ()), argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- highlights@
highlights :: IsSWHighlightCenter swHighlightCenter => swHighlightCenter -> IO (Id NSArray)
highlights swHighlightCenter  =
  sendMsg swHighlightCenter (mkSelector "highlights") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Localized title to display with a collection of highlights
--
-- Use this string as the title for a collection of shared highlight links displayed to the user.
--
-- ObjC selector: @+ highlightCollectionTitle@
highlightCollectionTitle :: IO (Id NSString)
highlightCollectionTitle  =
  do
    cls' <- getRequiredClass "SWHighlightCenter"
    sendClassMsg cls' (mkSelector "highlightCollectionTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Whether the current software version has full support for Messages collaboration features.
--
-- Use this property at runtime to conditionally enable Messages collaboration features in your app. This property will be permantently set to YES on a software version with full support for these features.
--
-- ObjC selector: @+ systemCollaborationSupportAvailable@
systemCollaborationSupportAvailable :: IO Bool
systemCollaborationSupportAvailable  =
  do
    cls' <- getRequiredClass "SWHighlightCenter"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "systemCollaborationSupportAvailable") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getHighlightForURL:completionHandler:@
getHighlightForURL_completionHandlerSelector :: Selector
getHighlightForURL_completionHandlerSelector = mkSelector "getHighlightForURL:completionHandler:"

-- | @Selector@ for @collaborationHighlightForIdentifier:error:@
collaborationHighlightForIdentifier_errorSelector :: Selector
collaborationHighlightForIdentifier_errorSelector = mkSelector "collaborationHighlightForIdentifier:error:"

-- | @Selector@ for @getCollaborationHighlightForURL:completionHandler:@
getCollaborationHighlightForURL_completionHandlerSelector :: Selector
getCollaborationHighlightForURL_completionHandlerSelector = mkSelector "getCollaborationHighlightForURL:completionHandler:"

-- | @Selector@ for @postNoticeForHighlightEvent:@
postNoticeForHighlightEventSelector :: Selector
postNoticeForHighlightEventSelector = mkSelector "postNoticeForHighlightEvent:"

-- | @Selector@ for @clearNoticesForHighlight:@
clearNoticesForHighlightSelector :: Selector
clearNoticesForHighlightSelector = mkSelector "clearNoticesForHighlight:"

-- | @Selector@ for @getSignedIdentityProofForCollaborationHighlight:usingData:completionHandler:@
getSignedIdentityProofForCollaborationHighlight_usingData_completionHandlerSelector :: Selector
getSignedIdentityProofForCollaborationHighlight_usingData_completionHandlerSelector = mkSelector "getSignedIdentityProofForCollaborationHighlight:usingData:completionHandler:"

-- | @Selector@ for @highlights@
highlightsSelector :: Selector
highlightsSelector = mkSelector "highlights"

-- | @Selector@ for @highlightCollectionTitle@
highlightCollectionTitleSelector :: Selector
highlightCollectionTitleSelector = mkSelector "highlightCollectionTitle"

-- | @Selector@ for @systemCollaborationSupportAvailable@
systemCollaborationSupportAvailableSelector :: Selector
systemCollaborationSupportAvailableSelector = mkSelector "systemCollaborationSupportAvailable"

