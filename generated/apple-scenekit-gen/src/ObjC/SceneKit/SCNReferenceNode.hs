{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNReferenceNode
--
-- Node that references an external file.
--
-- Generated bindings for @SCNReferenceNode@.
module ObjC.SceneKit.SCNReferenceNode
  ( SCNReferenceNode
  , IsSCNReferenceNode(..)
  , initWithURL
  , initWithCoder
  , referenceNodeWithURL
  , load
  , unload
  , referenceURL
  , setReferenceURL
  , loadingPolicy
  , setLoadingPolicy
  , loaded
  , initWithCoderSelector
  , initWithURLSelector
  , loadSelector
  , loadedSelector
  , loadingPolicySelector
  , referenceNodeWithURLSelector
  , referenceURLSelector
  , setLoadingPolicySelector
  , setReferenceURLSelector
  , unloadSelector

  -- * Enum types
  , SCNReferenceLoadingPolicy(SCNReferenceLoadingPolicy)
  , pattern SCNReferenceLoadingPolicyImmediate
  , pattern SCNReferenceLoadingPolicyOnDemand

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | initWithURL:
--
-- Creates a reference node with a url.
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsSCNReferenceNode scnReferenceNode, IsNSURL referenceURL) => scnReferenceNode -> referenceURL -> IO (Id SCNReferenceNode)
initWithURL scnReferenceNode referenceURL =
  sendOwnedMessage scnReferenceNode initWithURLSelector (toNSURL referenceURL)

-- | initWithCoder:
--
-- Support coding and decoding via NSKeyedArchiver.
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsSCNReferenceNode scnReferenceNode, IsNSCoder aDecoder) => scnReferenceNode -> aDecoder -> IO (Id SCNReferenceNode)
initWithCoder scnReferenceNode aDecoder =
  sendOwnedMessage scnReferenceNode initWithCoderSelector (toNSCoder aDecoder)

-- | referenceNodeWithURL:
--
-- Creates a reference node with a url.
--
-- ObjC selector: @+ referenceNodeWithURL:@
referenceNodeWithURL :: IsNSURL referenceURL => referenceURL -> IO (Id SCNReferenceNode)
referenceNodeWithURL referenceURL =
  do
    cls' <- getRequiredClass "SCNReferenceNode"
    sendClassMessage cls' referenceNodeWithURLSelector (toNSURL referenceURL)

-- | load
--
-- Force the reference to be loaded if it hasn't been loaded already. The resolved nodes will be added as child nodes of the receiver.
--
-- ObjC selector: @- load@
load :: IsSCNReferenceNode scnReferenceNode => scnReferenceNode -> IO ()
load scnReferenceNode =
  sendMessage scnReferenceNode loadSelector

-- | unload
--
-- Remove the child nodes and mark as unloaded.
--
-- ObjC selector: @- unload@
unload :: IsSCNReferenceNode scnReferenceNode => scnReferenceNode -> IO ()
unload scnReferenceNode =
  sendMessage scnReferenceNode unloadSelector

-- | referenceURL
--
-- Specifies the url to resolve.
--
-- ObjC selector: @- referenceURL@
referenceURL :: IsSCNReferenceNode scnReferenceNode => scnReferenceNode -> IO (Id NSURL)
referenceURL scnReferenceNode =
  sendMessage scnReferenceNode referenceURLSelector

-- | referenceURL
--
-- Specifies the url to resolve.
--
-- ObjC selector: @- setReferenceURL:@
setReferenceURL :: (IsSCNReferenceNode scnReferenceNode, IsNSURL value) => scnReferenceNode -> value -> IO ()
setReferenceURL scnReferenceNode value =
  sendMessage scnReferenceNode setReferenceURLSelector (toNSURL value)

-- | loadingPolicy
--
-- Specifies when to load the reference. see SCNReferenceLoadingPolicy above. Defaults to SCNReferenceLoadingPolicyImmediately.
--
-- ObjC selector: @- loadingPolicy@
loadingPolicy :: IsSCNReferenceNode scnReferenceNode => scnReferenceNode -> IO SCNReferenceLoadingPolicy
loadingPolicy scnReferenceNode =
  sendMessage scnReferenceNode loadingPolicySelector

-- | loadingPolicy
--
-- Specifies when to load the reference. see SCNReferenceLoadingPolicy above. Defaults to SCNReferenceLoadingPolicyImmediately.
--
-- ObjC selector: @- setLoadingPolicy:@
setLoadingPolicy :: IsSCNReferenceNode scnReferenceNode => scnReferenceNode -> SCNReferenceLoadingPolicy -> IO ()
setLoadingPolicy scnReferenceNode value =
  sendMessage scnReferenceNode setLoadingPolicySelector value

-- | loaded
--
-- Indicates whether the referenced URL has been loaded.
--
-- ObjC selector: @- loaded@
loaded :: IsSCNReferenceNode scnReferenceNode => scnReferenceNode -> IO Bool
loaded scnReferenceNode =
  sendMessage scnReferenceNode loadedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector '[Id NSURL] (Id SCNReferenceNode)
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id SCNReferenceNode)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @referenceNodeWithURL:@
referenceNodeWithURLSelector :: Selector '[Id NSURL] (Id SCNReferenceNode)
referenceNodeWithURLSelector = mkSelector "referenceNodeWithURL:"

-- | @Selector@ for @load@
loadSelector :: Selector '[] ()
loadSelector = mkSelector "load"

-- | @Selector@ for @unload@
unloadSelector :: Selector '[] ()
unloadSelector = mkSelector "unload"

-- | @Selector@ for @referenceURL@
referenceURLSelector :: Selector '[] (Id NSURL)
referenceURLSelector = mkSelector "referenceURL"

-- | @Selector@ for @setReferenceURL:@
setReferenceURLSelector :: Selector '[Id NSURL] ()
setReferenceURLSelector = mkSelector "setReferenceURL:"

-- | @Selector@ for @loadingPolicy@
loadingPolicySelector :: Selector '[] SCNReferenceLoadingPolicy
loadingPolicySelector = mkSelector "loadingPolicy"

-- | @Selector@ for @setLoadingPolicy:@
setLoadingPolicySelector :: Selector '[SCNReferenceLoadingPolicy] ()
setLoadingPolicySelector = mkSelector "setLoadingPolicy:"

-- | @Selector@ for @loaded@
loadedSelector :: Selector '[] Bool
loadedSelector = mkSelector "loaded"

