{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKMapSnapshotter@.
module ObjC.MapKit.MKMapSnapshotter
  ( MKMapSnapshotter
  , IsMKMapSnapshotter(..)
  , initWithOptions
  , startWithCompletionHandler
  , startWithQueue_completionHandler
  , cancel
  , loading
  , cancelSelector
  , initWithOptionsSelector
  , loadingSelector
  , startWithCompletionHandlerSelector
  , startWithQueue_completionHandlerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithOptions:@
initWithOptions :: (IsMKMapSnapshotter mkMapSnapshotter, IsMKMapSnapshotOptions options) => mkMapSnapshotter -> options -> IO (Id MKMapSnapshotter)
initWithOptions mkMapSnapshotter options =
  sendOwnedMessage mkMapSnapshotter initWithOptionsSelector (toMKMapSnapshotOptions options)

-- | @- startWithCompletionHandler:@
startWithCompletionHandler :: IsMKMapSnapshotter mkMapSnapshotter => mkMapSnapshotter -> Ptr () -> IO ()
startWithCompletionHandler mkMapSnapshotter completionHandler =
  sendMessage mkMapSnapshotter startWithCompletionHandlerSelector completionHandler

-- | @- startWithQueue:completionHandler:@
startWithQueue_completionHandler :: (IsMKMapSnapshotter mkMapSnapshotter, IsNSObject queue) => mkMapSnapshotter -> queue -> Ptr () -> IO ()
startWithQueue_completionHandler mkMapSnapshotter queue completionHandler =
  sendMessage mkMapSnapshotter startWithQueue_completionHandlerSelector (toNSObject queue) completionHandler

-- | @- cancel@
cancel :: IsMKMapSnapshotter mkMapSnapshotter => mkMapSnapshotter -> IO ()
cancel mkMapSnapshotter =
  sendMessage mkMapSnapshotter cancelSelector

-- | @- loading@
loading :: IsMKMapSnapshotter mkMapSnapshotter => mkMapSnapshotter -> IO Bool
loading mkMapSnapshotter =
  sendMessage mkMapSnapshotter loadingSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithOptions:@
initWithOptionsSelector :: Selector '[Id MKMapSnapshotOptions] (Id MKMapSnapshotter)
initWithOptionsSelector = mkSelector "initWithOptions:"

-- | @Selector@ for @startWithCompletionHandler:@
startWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
startWithCompletionHandlerSelector = mkSelector "startWithCompletionHandler:"

-- | @Selector@ for @startWithQueue:completionHandler:@
startWithQueue_completionHandlerSelector :: Selector '[Id NSObject, Ptr ()] ()
startWithQueue_completionHandlerSelector = mkSelector "startWithQueue:completionHandler:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector '[] ()
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @loading@
loadingSelector :: Selector '[] Bool
loadingSelector = mkSelector "loading"

