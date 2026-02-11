{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSExtensionContext@.
module ObjC.ReplayKit.NSExtensionContext
  ( NSExtensionContext
  , IsNSExtensionContext(..)
  , loadBroadcastingApplicationInfoWithCompletion
  , completeRequestWithBroadcastURL_setupInfo
  , loadBroadcastingApplicationInfoWithCompletionSelector
  , completeRequestWithBroadcastURL_setupInfoSelector


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

import ObjC.ReplayKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Load information about the broadcasting app.
--
-- @handler@ — block which will be supplied a bundleID, displayName and an optional appIcon.
--
-- ObjC selector: @- loadBroadcastingApplicationInfoWithCompletion:@
loadBroadcastingApplicationInfoWithCompletion :: IsNSExtensionContext nsExtensionContext => nsExtensionContext -> Ptr () -> IO ()
loadBroadcastingApplicationInfoWithCompletion nsExtensionContext  handler =
  sendMsg nsExtensionContext (mkSelector "loadBroadcastingApplicationInfoWithCompletion:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | @- completeRequestWithBroadcastURL:setupInfo:@
completeRequestWithBroadcastURL_setupInfo :: (IsNSExtensionContext nsExtensionContext, IsNSURL broadcastURL, IsNSDictionary setupInfo) => nsExtensionContext -> broadcastURL -> setupInfo -> IO ()
completeRequestWithBroadcastURL_setupInfo nsExtensionContext  broadcastURL setupInfo =
withObjCPtr broadcastURL $ \raw_broadcastURL ->
  withObjCPtr setupInfo $ \raw_setupInfo ->
      sendMsg nsExtensionContext (mkSelector "completeRequestWithBroadcastURL:setupInfo:") retVoid [argPtr (castPtr raw_broadcastURL :: Ptr ()), argPtr (castPtr raw_setupInfo :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadBroadcastingApplicationInfoWithCompletion:@
loadBroadcastingApplicationInfoWithCompletionSelector :: Selector
loadBroadcastingApplicationInfoWithCompletionSelector = mkSelector "loadBroadcastingApplicationInfoWithCompletion:"

-- | @Selector@ for @completeRequestWithBroadcastURL:setupInfo:@
completeRequestWithBroadcastURL_setupInfoSelector :: Selector
completeRequestWithBroadcastURL_setupInfoSelector = mkSelector "completeRequestWithBroadcastURL:setupInfo:"

