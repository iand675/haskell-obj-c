{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVAssetResourceLoader@.
module ObjC.AVFoundation.AVAssetResourceLoader
  ( AVAssetResourceLoader
  , IsAVAssetResourceLoader(..)
  , init_
  , new
  , setDelegate_queue
  , delegate
  , delegateQueue
  , sendsCommonMediaClientDataAsHTTPHeaders
  , setSendsCommonMediaClientDataAsHTTPHeaders
  , preloadsEligibleContentKeys
  , setPreloadsEligibleContentKeys
  , delegateQueueSelector
  , delegateSelector
  , initSelector
  , newSelector
  , preloadsEligibleContentKeysSelector
  , sendsCommonMediaClientDataAsHTTPHeadersSelector
  , setDelegate_queueSelector
  , setPreloadsEligibleContentKeysSelector
  , setSendsCommonMediaClientDataAsHTTPHeadersSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVAssetResourceLoader avAssetResourceLoader => avAssetResourceLoader -> IO (Id AVAssetResourceLoader)
init_ avAssetResourceLoader =
  sendOwnedMessage avAssetResourceLoader initSelector

-- | @+ new@
new :: IO (Id AVAssetResourceLoader)
new  =
  do
    cls' <- getRequiredClass "AVAssetResourceLoader"
    sendOwnedClassMessage cls' newSelector

-- | setDelegate:queue:
--
-- Sets the receiver's delegate that will mediate resource loading and the dispatch queue on which delegate methods will be invoked.
--
-- @delegate@ — An object conforming to the AVAssetResourceLoaderDelegate protocol.
--
-- @delegateQueue@ — A dispatch queue on which all delegate methods will be invoked.
--
-- If you employ an AVAssetResourceLoader delegate that loads media data for playback, you should set the value of your AVPlayer’s automaticallyWaitsToMinimizeStalling property to NO. Allowing the value of automaticallyWaitsToMinimizeStalling to remain YES — its default value — when an AVAssetResourceLoader delegate is used for the loading of media data can result in poor start-up times for playback and poor recovery from stalls, because the behaviors provided by AVPlayer when automaticallyWaitsToMinimizeStalling has a value of YES depend on predictions of the future availability of media data that that do not function as expected when data is loaded via a client-controlled means, using the AVAssetResourceLoader delegate interface.
--
-- You can allow the value of automaticallyWaitsToMinimizeStalling to remain YES if you use an AVAssetResourceLoader delegate to manage content keys for FairPlay Streaming, to provide dynamically-generated master playlists for HTTP Live Streaming, or to respond to authentication challenges, but not to load media data for playback.
--
-- ObjC selector: @- setDelegate:queue:@
setDelegate_queue :: (IsAVAssetResourceLoader avAssetResourceLoader, IsNSObject delegateQueue) => avAssetResourceLoader -> RawId -> delegateQueue -> IO ()
setDelegate_queue avAssetResourceLoader delegate delegateQueue =
  sendMessage avAssetResourceLoader setDelegate_queueSelector delegate (toNSObject delegateQueue)

-- | delegate
--
-- The receiver's delegate.
--
-- The value of this property is an object conforming to the AVAssetResourceLoaderDelegate protocol. The delegate is set using the setDelegate:queue: method. The delegate is held using a zeroing-weak reference, so this property will have a value of nil after a delegate that was previously set has been deallocated.
--
-- ObjC selector: @- delegate@
delegate :: IsAVAssetResourceLoader avAssetResourceLoader => avAssetResourceLoader -> IO RawId
delegate avAssetResourceLoader =
  sendMessage avAssetResourceLoader delegateSelector

-- | delegateQueue
--
-- The dispatch queue on which all delegate methods will be invoked.
--
-- The value of this property is a dispatch_queue_t. The queue is set using the setDelegate:queue: method.
--
-- ObjC selector: @- delegateQueue@
delegateQueue :: IsAVAssetResourceLoader avAssetResourceLoader => avAssetResourceLoader -> IO (Id NSObject)
delegateQueue avAssetResourceLoader =
  sendMessage avAssetResourceLoader delegateQueueSelector

-- | @- sendsCommonMediaClientDataAsHTTPHeaders@
sendsCommonMediaClientDataAsHTTPHeaders :: IsAVAssetResourceLoader avAssetResourceLoader => avAssetResourceLoader -> IO Bool
sendsCommonMediaClientDataAsHTTPHeaders avAssetResourceLoader =
  sendMessage avAssetResourceLoader sendsCommonMediaClientDataAsHTTPHeadersSelector

-- | @- setSendsCommonMediaClientDataAsHTTPHeaders:@
setSendsCommonMediaClientDataAsHTTPHeaders :: IsAVAssetResourceLoader avAssetResourceLoader => avAssetResourceLoader -> Bool -> IO ()
setSendsCommonMediaClientDataAsHTTPHeaders avAssetResourceLoader value =
  sendMessage avAssetResourceLoader setSendsCommonMediaClientDataAsHTTPHeadersSelector value

-- | preloadsEligibleContentKeys
--
-- When YES, eligible content keys will be loaded as eagerly as possible, potentially handled by the delegate. Setting to YES may result in network activity.
--
-- Any work done as a result of setting this property will be performed asynchronously.
--
-- ObjC selector: @- preloadsEligibleContentKeys@
preloadsEligibleContentKeys :: IsAVAssetResourceLoader avAssetResourceLoader => avAssetResourceLoader -> IO Bool
preloadsEligibleContentKeys avAssetResourceLoader =
  sendMessage avAssetResourceLoader preloadsEligibleContentKeysSelector

-- | preloadsEligibleContentKeys
--
-- When YES, eligible content keys will be loaded as eagerly as possible, potentially handled by the delegate. Setting to YES may result in network activity.
--
-- Any work done as a result of setting this property will be performed asynchronously.
--
-- ObjC selector: @- setPreloadsEligibleContentKeys:@
setPreloadsEligibleContentKeys :: IsAVAssetResourceLoader avAssetResourceLoader => avAssetResourceLoader -> Bool -> IO ()
setPreloadsEligibleContentKeys avAssetResourceLoader value =
  sendMessage avAssetResourceLoader setPreloadsEligibleContentKeysSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVAssetResourceLoader)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVAssetResourceLoader)
newSelector = mkSelector "new"

-- | @Selector@ for @setDelegate:queue:@
setDelegate_queueSelector :: Selector '[RawId, Id NSObject] ()
setDelegate_queueSelector = mkSelector "setDelegate:queue:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @delegateQueue@
delegateQueueSelector :: Selector '[] (Id NSObject)
delegateQueueSelector = mkSelector "delegateQueue"

-- | @Selector@ for @sendsCommonMediaClientDataAsHTTPHeaders@
sendsCommonMediaClientDataAsHTTPHeadersSelector :: Selector '[] Bool
sendsCommonMediaClientDataAsHTTPHeadersSelector = mkSelector "sendsCommonMediaClientDataAsHTTPHeaders"

-- | @Selector@ for @setSendsCommonMediaClientDataAsHTTPHeaders:@
setSendsCommonMediaClientDataAsHTTPHeadersSelector :: Selector '[Bool] ()
setSendsCommonMediaClientDataAsHTTPHeadersSelector = mkSelector "setSendsCommonMediaClientDataAsHTTPHeaders:"

-- | @Selector@ for @preloadsEligibleContentKeys@
preloadsEligibleContentKeysSelector :: Selector '[] Bool
preloadsEligibleContentKeysSelector = mkSelector "preloadsEligibleContentKeys"

-- | @Selector@ for @setPreloadsEligibleContentKeys:@
setPreloadsEligibleContentKeysSelector :: Selector '[Bool] ()
setPreloadsEligibleContentKeysSelector = mkSelector "setPreloadsEligibleContentKeys:"

