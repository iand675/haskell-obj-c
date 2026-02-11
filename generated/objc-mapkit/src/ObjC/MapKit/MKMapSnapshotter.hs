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
  , initWithOptionsSelector
  , startWithCompletionHandlerSelector
  , startWithQueue_completionHandlerSelector
  , cancelSelector
  , loadingSelector


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

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithOptions:@
initWithOptions :: (IsMKMapSnapshotter mkMapSnapshotter, IsMKMapSnapshotOptions options) => mkMapSnapshotter -> options -> IO (Id MKMapSnapshotter)
initWithOptions mkMapSnapshotter  options =
withObjCPtr options $ \raw_options ->
    sendMsg mkMapSnapshotter (mkSelector "initWithOptions:") (retPtr retVoid) [argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | @- startWithCompletionHandler:@
startWithCompletionHandler :: IsMKMapSnapshotter mkMapSnapshotter => mkMapSnapshotter -> Ptr () -> IO ()
startWithCompletionHandler mkMapSnapshotter  completionHandler =
  sendMsg mkMapSnapshotter (mkSelector "startWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- startWithQueue:completionHandler:@
startWithQueue_completionHandler :: (IsMKMapSnapshotter mkMapSnapshotter, IsNSObject queue) => mkMapSnapshotter -> queue -> Ptr () -> IO ()
startWithQueue_completionHandler mkMapSnapshotter  queue completionHandler =
withObjCPtr queue $ \raw_queue ->
    sendMsg mkMapSnapshotter (mkSelector "startWithQueue:completionHandler:") retVoid [argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- cancel@
cancel :: IsMKMapSnapshotter mkMapSnapshotter => mkMapSnapshotter -> IO ()
cancel mkMapSnapshotter  =
  sendMsg mkMapSnapshotter (mkSelector "cancel") retVoid []

-- | @- loading@
loading :: IsMKMapSnapshotter mkMapSnapshotter => mkMapSnapshotter -> IO Bool
loading mkMapSnapshotter  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkMapSnapshotter (mkSelector "loading") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithOptions:@
initWithOptionsSelector :: Selector
initWithOptionsSelector = mkSelector "initWithOptions:"

-- | @Selector@ for @startWithCompletionHandler:@
startWithCompletionHandlerSelector :: Selector
startWithCompletionHandlerSelector = mkSelector "startWithCompletionHandler:"

-- | @Selector@ for @startWithQueue:completionHandler:@
startWithQueue_completionHandlerSelector :: Selector
startWithQueue_completionHandlerSelector = mkSelector "startWithQueue:completionHandler:"

-- | @Selector@ for @cancel@
cancelSelector :: Selector
cancelSelector = mkSelector "cancel"

-- | @Selector@ for @loading@
loadingSelector :: Selector
loadingSelector = mkSelector "loading"

