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
presentWithCompletionHandler avCaptureDeskViewApplication  completionHandler =
  sendMsg avCaptureDeskViewApplication (mkSelector "presentWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

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
presentWithLaunchConfiguration_completionHandler avCaptureDeskViewApplication  launchConfiguration completionHandler =
withObjCPtr launchConfiguration $ \raw_launchConfiguration ->
    sendMsg avCaptureDeskViewApplication (mkSelector "presentWithLaunchConfiguration:completionHandler:") retVoid [argPtr (castPtr raw_launchConfiguration :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presentWithCompletionHandler:@
presentWithCompletionHandlerSelector :: Selector
presentWithCompletionHandlerSelector = mkSelector "presentWithCompletionHandler:"

-- | @Selector@ for @presentWithLaunchConfiguration:completionHandler:@
presentWithLaunchConfiguration_completionHandlerSelector :: Selector
presentWithLaunchConfiguration_completionHandlerSelector = mkSelector "presentWithLaunchConfiguration:completionHandler:"

