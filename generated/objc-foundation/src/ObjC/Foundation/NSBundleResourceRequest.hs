{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSBundleResourceRequest@.
module ObjC.Foundation.NSBundleResourceRequest
  ( NSBundleResourceRequest
  , IsNSBundleResourceRequest(..)
  , init_
  , initWithTags
  , initWithTags_bundle
  , beginAccessingResourcesWithCompletionHandler
  , conditionallyBeginAccessingResourcesWithCompletionHandler
  , endAccessingResources
  , loadingPriority
  , setLoadingPriority
  , tags
  , bundle
  , progress
  , initSelector
  , initWithTagsSelector
  , initWithTags_bundleSelector
  , beginAccessingResourcesWithCompletionHandlerSelector
  , conditionallyBeginAccessingResourcesWithCompletionHandlerSelector
  , endAccessingResourcesSelector
  , loadingPrioritySelector
  , setLoadingPrioritySelector
  , tagsSelector
  , bundleSelector
  , progressSelector


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

import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSBundleResourceRequest nsBundleResourceRequest => nsBundleResourceRequest -> IO (Id NSBundleResourceRequest)
init_ nsBundleResourceRequest  =
  sendMsg nsBundleResourceRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithTags:@
initWithTags :: (IsNSBundleResourceRequest nsBundleResourceRequest, IsNSSet tags) => nsBundleResourceRequest -> tags -> IO (Id NSBundleResourceRequest)
initWithTags nsBundleResourceRequest  tags =
withObjCPtr tags $ \raw_tags ->
    sendMsg nsBundleResourceRequest (mkSelector "initWithTags:") (retPtr retVoid) [argPtr (castPtr raw_tags :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithTags:bundle:@
initWithTags_bundle :: (IsNSBundleResourceRequest nsBundleResourceRequest, IsNSSet tags, IsNSBundle bundle) => nsBundleResourceRequest -> tags -> bundle -> IO (Id NSBundleResourceRequest)
initWithTags_bundle nsBundleResourceRequest  tags bundle =
withObjCPtr tags $ \raw_tags ->
  withObjCPtr bundle $ \raw_bundle ->
      sendMsg nsBundleResourceRequest (mkSelector "initWithTags:bundle:") (retPtr retVoid) [argPtr (castPtr raw_tags :: Ptr ()), argPtr (castPtr raw_bundle :: Ptr ())] >>= ownedObject . castPtr

-- | @- beginAccessingResourcesWithCompletionHandler:@
beginAccessingResourcesWithCompletionHandler :: IsNSBundleResourceRequest nsBundleResourceRequest => nsBundleResourceRequest -> Ptr () -> IO ()
beginAccessingResourcesWithCompletionHandler nsBundleResourceRequest  completionHandler =
  sendMsg nsBundleResourceRequest (mkSelector "beginAccessingResourcesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- conditionallyBeginAccessingResourcesWithCompletionHandler:@
conditionallyBeginAccessingResourcesWithCompletionHandler :: IsNSBundleResourceRequest nsBundleResourceRequest => nsBundleResourceRequest -> Ptr () -> IO ()
conditionallyBeginAccessingResourcesWithCompletionHandler nsBundleResourceRequest  completionHandler =
  sendMsg nsBundleResourceRequest (mkSelector "conditionallyBeginAccessingResourcesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | @- endAccessingResources@
endAccessingResources :: IsNSBundleResourceRequest nsBundleResourceRequest => nsBundleResourceRequest -> IO ()
endAccessingResources nsBundleResourceRequest  =
  sendMsg nsBundleResourceRequest (mkSelector "endAccessingResources") retVoid []

-- | @- loadingPriority@
loadingPriority :: IsNSBundleResourceRequest nsBundleResourceRequest => nsBundleResourceRequest -> IO CDouble
loadingPriority nsBundleResourceRequest  =
  sendMsg nsBundleResourceRequest (mkSelector "loadingPriority") retCDouble []

-- | @- setLoadingPriority:@
setLoadingPriority :: IsNSBundleResourceRequest nsBundleResourceRequest => nsBundleResourceRequest -> CDouble -> IO ()
setLoadingPriority nsBundleResourceRequest  value =
  sendMsg nsBundleResourceRequest (mkSelector "setLoadingPriority:") retVoid [argCDouble (fromIntegral value)]

-- | @- tags@
tags :: IsNSBundleResourceRequest nsBundleResourceRequest => nsBundleResourceRequest -> IO (Id NSSet)
tags nsBundleResourceRequest  =
  sendMsg nsBundleResourceRequest (mkSelector "tags") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- bundle@
bundle :: IsNSBundleResourceRequest nsBundleResourceRequest => nsBundleResourceRequest -> IO (Id NSBundle)
bundle nsBundleResourceRequest  =
  sendMsg nsBundleResourceRequest (mkSelector "bundle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- progress@
progress :: IsNSBundleResourceRequest nsBundleResourceRequest => nsBundleResourceRequest -> IO (Id NSProgress)
progress nsBundleResourceRequest  =
  sendMsg nsBundleResourceRequest (mkSelector "progress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithTags:@
initWithTagsSelector :: Selector
initWithTagsSelector = mkSelector "initWithTags:"

-- | @Selector@ for @initWithTags:bundle:@
initWithTags_bundleSelector :: Selector
initWithTags_bundleSelector = mkSelector "initWithTags:bundle:"

-- | @Selector@ for @beginAccessingResourcesWithCompletionHandler:@
beginAccessingResourcesWithCompletionHandlerSelector :: Selector
beginAccessingResourcesWithCompletionHandlerSelector = mkSelector "beginAccessingResourcesWithCompletionHandler:"

-- | @Selector@ for @conditionallyBeginAccessingResourcesWithCompletionHandler:@
conditionallyBeginAccessingResourcesWithCompletionHandlerSelector :: Selector
conditionallyBeginAccessingResourcesWithCompletionHandlerSelector = mkSelector "conditionallyBeginAccessingResourcesWithCompletionHandler:"

-- | @Selector@ for @endAccessingResources@
endAccessingResourcesSelector :: Selector
endAccessingResourcesSelector = mkSelector "endAccessingResources"

-- | @Selector@ for @loadingPriority@
loadingPrioritySelector :: Selector
loadingPrioritySelector = mkSelector "loadingPriority"

-- | @Selector@ for @setLoadingPriority:@
setLoadingPrioritySelector :: Selector
setLoadingPrioritySelector = mkSelector "setLoadingPriority:"

-- | @Selector@ for @tags@
tagsSelector :: Selector
tagsSelector = mkSelector "tags"

-- | @Selector@ for @bundle@
bundleSelector :: Selector
bundleSelector = mkSelector "bundle"

-- | @Selector@ for @progress@
progressSelector :: Selector
progressSelector = mkSelector "progress"

