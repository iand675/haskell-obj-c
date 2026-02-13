{-# LANGUAGE DataKinds #-}
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
  , beginAccessingResourcesWithCompletionHandlerSelector
  , bundleSelector
  , conditionallyBeginAccessingResourcesWithCompletionHandlerSelector
  , endAccessingResourcesSelector
  , initSelector
  , initWithTagsSelector
  , initWithTags_bundleSelector
  , loadingPrioritySelector
  , progressSelector
  , setLoadingPrioritySelector
  , tagsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSBundleResourceRequest nsBundleResourceRequest => nsBundleResourceRequest -> IO (Id NSBundleResourceRequest)
init_ nsBundleResourceRequest =
  sendOwnedMessage nsBundleResourceRequest initSelector

-- | @- initWithTags:@
initWithTags :: (IsNSBundleResourceRequest nsBundleResourceRequest, IsNSSet tags) => nsBundleResourceRequest -> tags -> IO (Id NSBundleResourceRequest)
initWithTags nsBundleResourceRequest tags =
  sendOwnedMessage nsBundleResourceRequest initWithTagsSelector (toNSSet tags)

-- | @- initWithTags:bundle:@
initWithTags_bundle :: (IsNSBundleResourceRequest nsBundleResourceRequest, IsNSSet tags, IsNSBundle bundle) => nsBundleResourceRequest -> tags -> bundle -> IO (Id NSBundleResourceRequest)
initWithTags_bundle nsBundleResourceRequest tags bundle =
  sendOwnedMessage nsBundleResourceRequest initWithTags_bundleSelector (toNSSet tags) (toNSBundle bundle)

-- | @- beginAccessingResourcesWithCompletionHandler:@
beginAccessingResourcesWithCompletionHandler :: IsNSBundleResourceRequest nsBundleResourceRequest => nsBundleResourceRequest -> Ptr () -> IO ()
beginAccessingResourcesWithCompletionHandler nsBundleResourceRequest completionHandler =
  sendMessage nsBundleResourceRequest beginAccessingResourcesWithCompletionHandlerSelector completionHandler

-- | @- conditionallyBeginAccessingResourcesWithCompletionHandler:@
conditionallyBeginAccessingResourcesWithCompletionHandler :: IsNSBundleResourceRequest nsBundleResourceRequest => nsBundleResourceRequest -> Ptr () -> IO ()
conditionallyBeginAccessingResourcesWithCompletionHandler nsBundleResourceRequest completionHandler =
  sendMessage nsBundleResourceRequest conditionallyBeginAccessingResourcesWithCompletionHandlerSelector completionHandler

-- | @- endAccessingResources@
endAccessingResources :: IsNSBundleResourceRequest nsBundleResourceRequest => nsBundleResourceRequest -> IO ()
endAccessingResources nsBundleResourceRequest =
  sendMessage nsBundleResourceRequest endAccessingResourcesSelector

-- | @- loadingPriority@
loadingPriority :: IsNSBundleResourceRequest nsBundleResourceRequest => nsBundleResourceRequest -> IO CDouble
loadingPriority nsBundleResourceRequest =
  sendMessage nsBundleResourceRequest loadingPrioritySelector

-- | @- setLoadingPriority:@
setLoadingPriority :: IsNSBundleResourceRequest nsBundleResourceRequest => nsBundleResourceRequest -> CDouble -> IO ()
setLoadingPriority nsBundleResourceRequest value =
  sendMessage nsBundleResourceRequest setLoadingPrioritySelector value

-- | @- tags@
tags :: IsNSBundleResourceRequest nsBundleResourceRequest => nsBundleResourceRequest -> IO (Id NSSet)
tags nsBundleResourceRequest =
  sendMessage nsBundleResourceRequest tagsSelector

-- | @- bundle@
bundle :: IsNSBundleResourceRequest nsBundleResourceRequest => nsBundleResourceRequest -> IO (Id NSBundle)
bundle nsBundleResourceRequest =
  sendMessage nsBundleResourceRequest bundleSelector

-- | @- progress@
progress :: IsNSBundleResourceRequest nsBundleResourceRequest => nsBundleResourceRequest -> IO (Id NSProgress)
progress nsBundleResourceRequest =
  sendMessage nsBundleResourceRequest progressSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSBundleResourceRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithTags:@
initWithTagsSelector :: Selector '[Id NSSet] (Id NSBundleResourceRequest)
initWithTagsSelector = mkSelector "initWithTags:"

-- | @Selector@ for @initWithTags:bundle:@
initWithTags_bundleSelector :: Selector '[Id NSSet, Id NSBundle] (Id NSBundleResourceRequest)
initWithTags_bundleSelector = mkSelector "initWithTags:bundle:"

-- | @Selector@ for @beginAccessingResourcesWithCompletionHandler:@
beginAccessingResourcesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
beginAccessingResourcesWithCompletionHandlerSelector = mkSelector "beginAccessingResourcesWithCompletionHandler:"

-- | @Selector@ for @conditionallyBeginAccessingResourcesWithCompletionHandler:@
conditionallyBeginAccessingResourcesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
conditionallyBeginAccessingResourcesWithCompletionHandlerSelector = mkSelector "conditionallyBeginAccessingResourcesWithCompletionHandler:"

-- | @Selector@ for @endAccessingResources@
endAccessingResourcesSelector :: Selector '[] ()
endAccessingResourcesSelector = mkSelector "endAccessingResources"

-- | @Selector@ for @loadingPriority@
loadingPrioritySelector :: Selector '[] CDouble
loadingPrioritySelector = mkSelector "loadingPriority"

-- | @Selector@ for @setLoadingPriority:@
setLoadingPrioritySelector :: Selector '[CDouble] ()
setLoadingPrioritySelector = mkSelector "setLoadingPriority:"

-- | @Selector@ for @tags@
tagsSelector :: Selector '[] (Id NSSet)
tagsSelector = mkSelector "tags"

-- | @Selector@ for @bundle@
bundleSelector :: Selector '[] (Id NSBundle)
bundleSelector = mkSelector "bundle"

-- | @Selector@ for @progress@
progressSelector :: Selector '[] (Id NSProgress)
progressSelector = mkSelector "progress"

