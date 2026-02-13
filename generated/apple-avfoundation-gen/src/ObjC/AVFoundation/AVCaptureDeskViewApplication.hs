{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptureDeskViewApplication
--
-- Allows a client to programmatically present the Desk View application and be informed when it is done being launched.
--
-- Users can launch the Desk View application through the Video Effects button in Control Center when a Desk View capable Continuity Camera is running. Developers may use this interface as a shortcut to launch the Desk View application directly from their application.
--
-- Generated bindings for @AVCaptureDeskViewApplication@.
module ObjC.AVFoundation.AVCaptureDeskViewApplication
  ( AVCaptureDeskViewApplication
  , IsAVCaptureDeskViewApplication(..)
  , presentWithCompletionHandler
  , presentWithLaunchConfiguration_completionHandler
  , presentWithCompletionHandlerSelector
  , presentWithLaunchConfiguration_completionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | presentWithCompletionHandler:
--
-- Presents the Desk View application to the user with no launch configuration.
--
-- @completionHandler@ — A block to be called once the Desk View application has been completely launched (or brought to the foreground).
--
-- This method allows you to programmatically launch the Desk View application from your own application UI. If the Desk View application is already running, this method brings it to the front. If the Desk View application is minimized in the Dock, this method un-minimizes it and brings it to the front.
--
-- ObjC selector: @- presentWithCompletionHandler:@
presentWithCompletionHandler :: IsAVCaptureDeskViewApplication avCaptureDeskViewApplication => avCaptureDeskViewApplication -> Ptr () -> IO ()
presentWithCompletionHandler avCaptureDeskViewApplication completionHandler =
  sendMessage avCaptureDeskViewApplication presentWithCompletionHandlerSelector completionHandler

-- | presentWithLaunchConfiguration:completionHandler:
--
-- Presents the Desk View application to the user.
--
-- @launchConfiguration@ — Launch configuration governing how the Desk View application will be presented.
--
-- @completionHandler@ — A block to be called once the Desk View application has been completely launched (or brought to the foreground). Optionally, this completionHandler may fire later, once the user has completed set up mode (see AVCaptureDeskViewApplicationLaunchConfiguration.requiresSetUpModeCompletion).
--
-- This method allows you to programmatically launch the Desk View application from your own application UI. If the Desk View application is already running, this method brings it to the front. If the Desk View application is minimized in the Dock, this method un-minimizes it and brings it to the front.
--
-- ObjC selector: @- presentWithLaunchConfiguration:completionHandler:@
presentWithLaunchConfiguration_completionHandler :: (IsAVCaptureDeskViewApplication avCaptureDeskViewApplication, IsAVCaptureDeskViewApplicationLaunchConfiguration launchConfiguration) => avCaptureDeskViewApplication -> launchConfiguration -> Ptr () -> IO ()
presentWithLaunchConfiguration_completionHandler avCaptureDeskViewApplication launchConfiguration completionHandler =
  sendMessage avCaptureDeskViewApplication presentWithLaunchConfiguration_completionHandlerSelector (toAVCaptureDeskViewApplicationLaunchConfiguration launchConfiguration) completionHandler

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presentWithCompletionHandler:@
presentWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
presentWithCompletionHandlerSelector = mkSelector "presentWithCompletionHandler:"

-- | @Selector@ for @presentWithLaunchConfiguration:completionHandler:@
presentWithLaunchConfiguration_completionHandlerSelector :: Selector '[Id AVCaptureDeskViewApplicationLaunchConfiguration, Ptr ()] ()
presentWithLaunchConfiguration_completionHandlerSelector = mkSelector "presentWithLaunchConfiguration:completionHandler:"

