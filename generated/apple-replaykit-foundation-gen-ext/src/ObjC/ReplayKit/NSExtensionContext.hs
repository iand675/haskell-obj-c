{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSExtensionContext@.
module ObjC.ReplayKit.NSExtensionContext
  ( NSExtensionContext
  , IsNSExtensionContext(..)
  , loadBroadcastingApplicationInfoWithCompletion
  , completeRequestWithBroadcastURL_setupInfo
  , completeRequestWithBroadcastURL_setupInfoSelector
  , loadBroadcastingApplicationInfoWithCompletionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ReplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Load information about the broadcasting app.
--
-- @handler@ — block which will be supplied a bundleID, displayName and an optional appIcon.
--
-- ObjC selector: @- loadBroadcastingApplicationInfoWithCompletion:@
loadBroadcastingApplicationInfoWithCompletion :: IsNSExtensionContext nsExtensionContext => nsExtensionContext -> Ptr () -> IO ()
loadBroadcastingApplicationInfoWithCompletion nsExtensionContext handler =
  sendMessage nsExtensionContext loadBroadcastingApplicationInfoWithCompletionSelector handler

-- | @- completeRequestWithBroadcastURL:setupInfo:@
completeRequestWithBroadcastURL_setupInfo :: (IsNSExtensionContext nsExtensionContext, IsNSURL broadcastURL, IsNSDictionary setupInfo) => nsExtensionContext -> broadcastURL -> setupInfo -> IO ()
completeRequestWithBroadcastURL_setupInfo nsExtensionContext broadcastURL setupInfo =
  sendMessage nsExtensionContext completeRequestWithBroadcastURL_setupInfoSelector (toNSURL broadcastURL) (toNSDictionary setupInfo)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadBroadcastingApplicationInfoWithCompletion:@
loadBroadcastingApplicationInfoWithCompletionSelector :: Selector '[Ptr ()] ()
loadBroadcastingApplicationInfoWithCompletionSelector = mkSelector "loadBroadcastingApplicationInfoWithCompletion:"

-- | @Selector@ for @completeRequestWithBroadcastURL:setupInfo:@
completeRequestWithBroadcastURL_setupInfoSelector :: Selector '[Id NSURL, Id NSDictionary] ()
completeRequestWithBroadcastURL_setupInfoSelector = mkSelector "completeRequestWithBroadcastURL:setupInfo:"

