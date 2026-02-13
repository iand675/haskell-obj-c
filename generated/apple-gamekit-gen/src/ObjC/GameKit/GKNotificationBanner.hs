{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.GameKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ showBannerWithTitle:message:completionHandler:@
showBannerWithTitle_message_completionHandler :: (IsNSString title, IsNSString message) => title -> message -> Ptr () -> IO ()
showBannerWithTitle_message_completionHandler title message completionHandler =
  do
    cls' <- getRequiredClass "GKNotificationBanner"
    sendClassMessage cls' showBannerWithTitle_message_completionHandlerSelector (toNSString title) (toNSString message) completionHandler

-- | @+ showBannerWithTitle:message:duration:completionHandler:@
showBannerWithTitle_message_duration_completionHandler :: (IsNSString title, IsNSString message) => title -> message -> CDouble -> Ptr () -> IO ()
showBannerWithTitle_message_duration_completionHandler title message duration completionHandler =
  do
    cls' <- getRequiredClass "GKNotificationBanner"
    sendClassMessage cls' showBannerWithTitle_message_duration_completionHandlerSelector (toNSString title) (toNSString message) duration completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @showBannerWithTitle:message:completionHandler:@
showBannerWithTitle_message_completionHandlerSelector :: Selector '[Id NSString, Id NSString, Ptr ()] ()
showBannerWithTitle_message_completionHandlerSelector = mkSelector "showBannerWithTitle:message:completionHandler:"

-- | @Selector@ for @showBannerWithTitle:message:duration:completionHandler:@
showBannerWithTitle_message_duration_completionHandlerSelector :: Selector '[Id NSString, Id NSString, CDouble, Ptr ()] ()
showBannerWithTitle_message_duration_completionHandlerSelector = mkSelector "showBannerWithTitle:message:duration:completionHandler:"

