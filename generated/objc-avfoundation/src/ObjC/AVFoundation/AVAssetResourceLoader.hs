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
  , delegateQueue
  , sendsCommonMediaClientDataAsHTTPHeaders
  , setSendsCommonMediaClientDataAsHTTPHeaders
  , preloadsEligibleContentKeys
  , setPreloadsEligibleContentKeys
  , initSelector
  , newSelector
  , setDelegate_queueSelector
  , delegateQueueSelector
  , sendsCommonMediaClientDataAsHTTPHeadersSelector
  , setSendsCommonMediaClientDataAsHTTPHeadersSelector
  , preloadsEligibleContentKeysSelector
  , setPreloadsEligibleContentKeysSelector


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

-- | @- init@
init_ :: IsAVAssetResourceLoader avAssetResourceLoader => avAssetResourceLoader -> IO (Id AVAssetResourceLoader)
init_ avAssetResourceLoader  =
  sendMsg avAssetResourceLoader (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVAssetResourceLoader)
new  =
  do
    cls' <- getRequiredClass "AVAssetResourceLoader"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
setDelegate_queue avAssetResourceLoader  delegate delegateQueue =
withObjCPtr delegateQueue $ \raw_delegateQueue ->
    sendMsg avAssetResourceLoader (mkSelector "setDelegate:queue:") retVoid [argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_delegateQueue :: Ptr ())]

-- | delegateQueue
--
-- The dispatch queue on which all delegate methods will be invoked.
--
-- The value of this property is a dispatch_queue_t. The queue is set using the setDelegate:queue: method.
--
-- ObjC selector: @- delegateQueue@
delegateQueue :: IsAVAssetResourceLoader avAssetResourceLoader => avAssetResourceLoader -> IO (Id NSObject)
delegateQueue avAssetResourceLoader  =
  sendMsg avAssetResourceLoader (mkSelector "delegateQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sendsCommonMediaClientDataAsHTTPHeaders@
sendsCommonMediaClientDataAsHTTPHeaders :: IsAVAssetResourceLoader avAssetResourceLoader => avAssetResourceLoader -> IO Bool
sendsCommonMediaClientDataAsHTTPHeaders avAssetResourceLoader  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetResourceLoader (mkSelector "sendsCommonMediaClientDataAsHTTPHeaders") retCULong []

-- | @- setSendsCommonMediaClientDataAsHTTPHeaders:@
setSendsCommonMediaClientDataAsHTTPHeaders :: IsAVAssetResourceLoader avAssetResourceLoader => avAssetResourceLoader -> Bool -> IO ()
setSendsCommonMediaClientDataAsHTTPHeaders avAssetResourceLoader  value =
  sendMsg avAssetResourceLoader (mkSelector "setSendsCommonMediaClientDataAsHTTPHeaders:") retVoid [argCULong (if value then 1 else 0)]

-- | preloadsEligibleContentKeys
--
-- When YES, eligible content keys will be loaded as eagerly as possible, potentially handled by the delegate. Setting to YES may result in network activity.
--
-- Any work done as a result of setting this property will be performed asynchronously.
--
-- ObjC selector: @- preloadsEligibleContentKeys@
preloadsEligibleContentKeys :: IsAVAssetResourceLoader avAssetResourceLoader => avAssetResourceLoader -> IO Bool
preloadsEligibleContentKeys avAssetResourceLoader  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avAssetResourceLoader (mkSelector "preloadsEligibleContentKeys") retCULong []

-- | preloadsEligibleContentKeys
--
-- When YES, eligible content keys will be loaded as eagerly as possible, potentially handled by the delegate. Setting to YES may result in network activity.
--
-- Any work done as a result of setting this property will be performed asynchronously.
--
-- ObjC selector: @- setPreloadsEligibleContentKeys:@
setPreloadsEligibleContentKeys :: IsAVAssetResourceLoader avAssetResourceLoader => avAssetResourceLoader -> Bool -> IO ()
setPreloadsEligibleContentKeys avAssetResourceLoader  value =
  sendMsg avAssetResourceLoader (mkSelector "setPreloadsEligibleContentKeys:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @setDelegate:queue:@
setDelegate_queueSelector :: Selector
setDelegate_queueSelector = mkSelector "setDelegate:queue:"

-- | @Selector@ for @delegateQueue@
delegateQueueSelector :: Selector
delegateQueueSelector = mkSelector "delegateQueue"

-- | @Selector@ for @sendsCommonMediaClientDataAsHTTPHeaders@
sendsCommonMediaClientDataAsHTTPHeadersSelector :: Selector
sendsCommonMediaClientDataAsHTTPHeadersSelector = mkSelector "sendsCommonMediaClientDataAsHTTPHeaders"

-- | @Selector@ for @setSendsCommonMediaClientDataAsHTTPHeaders:@
setSendsCommonMediaClientDataAsHTTPHeadersSelector :: Selector
setSendsCommonMediaClientDataAsHTTPHeadersSelector = mkSelector "setSendsCommonMediaClientDataAsHTTPHeaders:"

-- | @Selector@ for @preloadsEligibleContentKeys@
preloadsEligibleContentKeysSelector :: Selector
preloadsEligibleContentKeysSelector = mkSelector "preloadsEligibleContentKeys"

-- | @Selector@ for @setPreloadsEligibleContentKeys:@
setPreloadsEligibleContentKeysSelector :: Selector
setPreloadsEligibleContentKeysSelector = mkSelector "setPreloadsEligibleContentKeys:"

