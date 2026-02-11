{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Asynchronously shows a notification banner like the one used for Game Center’s “Welcome Back” message. If a banner is already being displayed, additional banners will be shown in sequence. Use this to notify the user of game events, high scores, completed achievements, etc.
--
-- Generated bindings for @GKNotificationBanner@.
module ObjC.GameKit.GKNotificationBanner
  ( GKNotificationBanner
  , IsGKNotificationBanner(..)
  , showBannerWithTitle_message_completionHandler
  , showBannerWithTitle_message_duration_completionHandler
  , showBannerWithTitle_message_completionHandlerSelector
  , showBannerWithTitle_message_duration_completionHandlerSelector


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

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ showBannerWithTitle:message:completionHandler:@
showBannerWithTitle_message_completionHandler :: (IsNSString title, IsNSString message) => title -> message -> Ptr () -> IO ()
showBannerWithTitle_message_completionHandler title message completionHandler =
  do
    cls' <- getRequiredClass "GKNotificationBanner"
    withObjCPtr title $ \raw_title ->
      withObjCPtr message $ \raw_message ->
        sendClassMsg cls' (mkSelector "showBannerWithTitle:message:completionHandler:") retVoid [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_message :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @+ showBannerWithTitle:message:duration:completionHandler:@
showBannerWithTitle_message_duration_completionHandler :: (IsNSString title, IsNSString message) => title -> message -> CDouble -> Ptr () -> IO ()
showBannerWithTitle_message_duration_completionHandler title message duration completionHandler =
  do
    cls' <- getRequiredClass "GKNotificationBanner"
    withObjCPtr title $ \raw_title ->
      withObjCPtr message $ \raw_message ->
        sendClassMsg cls' (mkSelector "showBannerWithTitle:message:duration:completionHandler:") retVoid [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_message :: Ptr ()), argCDouble (fromIntegral duration), argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @showBannerWithTitle:message:completionHandler:@
showBannerWithTitle_message_completionHandlerSelector :: Selector
showBannerWithTitle_message_completionHandlerSelector = mkSelector "showBannerWithTitle:message:completionHandler:"

-- | @Selector@ for @showBannerWithTitle:message:duration:completionHandler:@
showBannerWithTitle_message_duration_completionHandlerSelector :: Selector
showBannerWithTitle_message_duration_completionHandlerSelector = mkSelector "showBannerWithTitle:message:duration:completionHandler:"

