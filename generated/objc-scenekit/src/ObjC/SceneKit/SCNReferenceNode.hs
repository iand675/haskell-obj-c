{-# LANGUAGE PatternSynonyms #-}
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
  , initWithURLSelector
  , initWithCoderSelector
  , referenceNodeWithURLSelector
  , loadSelector
  , unloadSelector
  , referenceURLSelector
  , setReferenceURLSelector
  , loadingPolicySelector
  , setLoadingPolicySelector
  , loadedSelector

  -- * Enum types
  , SCNReferenceLoadingPolicy(SCNReferenceLoadingPolicy)
  , pattern SCNReferenceLoadingPolicyImmediate
  , pattern SCNReferenceLoadingPolicyOnDemand

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

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | initWithURL:
--
-- Creates a reference node with a url.
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsSCNReferenceNode scnReferenceNode, IsNSURL referenceURL) => scnReferenceNode -> referenceURL -> IO (Id SCNReferenceNode)
initWithURL scnReferenceNode  referenceURL =
withObjCPtr referenceURL $ \raw_referenceURL ->
    sendMsg scnReferenceNode (mkSelector "initWithURL:") (retPtr retVoid) [argPtr (castPtr raw_referenceURL :: Ptr ())] >>= ownedObject . castPtr

-- | initWithCoder:
--
-- Support coding and decoding via NSKeyedArchiver.
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsSCNReferenceNode scnReferenceNode, IsNSCoder aDecoder) => scnReferenceNode -> aDecoder -> IO (Id SCNReferenceNode)
initWithCoder scnReferenceNode  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg scnReferenceNode (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | referenceNodeWithURL:
--
-- Creates a reference node with a url.
--
-- ObjC selector: @+ referenceNodeWithURL:@
referenceNodeWithURL :: IsNSURL referenceURL => referenceURL -> IO (Id SCNReferenceNode)
referenceNodeWithURL referenceURL =
  do
    cls' <- getRequiredClass "SCNReferenceNode"
    withObjCPtr referenceURL $ \raw_referenceURL ->
      sendClassMsg cls' (mkSelector "referenceNodeWithURL:") (retPtr retVoid) [argPtr (castPtr raw_referenceURL :: Ptr ())] >>= retainedObject . castPtr

-- | load
--
-- Force the reference to be loaded if it hasn't been loaded already. The resolved nodes will be added as child nodes of the receiver.
--
-- ObjC selector: @- load@
load :: IsSCNReferenceNode scnReferenceNode => scnReferenceNode -> IO ()
load scnReferenceNode  =
  sendMsg scnReferenceNode (mkSelector "load") retVoid []

-- | unload
--
-- Remove the child nodes and mark as unloaded.
--
-- ObjC selector: @- unload@
unload :: IsSCNReferenceNode scnReferenceNode => scnReferenceNode -> IO ()
unload scnReferenceNode  =
  sendMsg scnReferenceNode (mkSelector "unload") retVoid []

-- | referenceURL
--
-- Specifies the url to resolve.
--
-- ObjC selector: @- referenceURL@
referenceURL :: IsSCNReferenceNode scnReferenceNode => scnReferenceNode -> IO (Id NSURL)
referenceURL scnReferenceNode  =
  sendMsg scnReferenceNode (mkSelector "referenceURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | referenceURL
--
-- Specifies the url to resolve.
--
-- ObjC selector: @- setReferenceURL:@
setReferenceURL :: (IsSCNReferenceNode scnReferenceNode, IsNSURL value) => scnReferenceNode -> value -> IO ()
setReferenceURL scnReferenceNode  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnReferenceNode (mkSelector "setReferenceURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | loadingPolicy
--
-- Specifies when to load the reference. see SCNReferenceLoadingPolicy above. Defaults to SCNReferenceLoadingPolicyImmediately.
--
-- ObjC selector: @- loadingPolicy@
loadingPolicy :: IsSCNReferenceNode scnReferenceNode => scnReferenceNode -> IO SCNReferenceLoadingPolicy
loadingPolicy scnReferenceNode  =
  fmap (coerce :: CLong -> SCNReferenceLoadingPolicy) $ sendMsg scnReferenceNode (mkSelector "loadingPolicy") retCLong []

-- | loadingPolicy
--
-- Specifies when to load the reference. see SCNReferenceLoadingPolicy above. Defaults to SCNReferenceLoadingPolicyImmediately.
--
-- ObjC selector: @- setLoadingPolicy:@
setLoadingPolicy :: IsSCNReferenceNode scnReferenceNode => scnReferenceNode -> SCNReferenceLoadingPolicy -> IO ()
setLoadingPolicy scnReferenceNode  value =
  sendMsg scnReferenceNode (mkSelector "setLoadingPolicy:") retVoid [argCLong (coerce value)]

-- | loaded
--
-- Indicates whether the referenced URL has been loaded.
--
-- ObjC selector: @- loaded@
loaded :: IsSCNReferenceNode scnReferenceNode => scnReferenceNode -> IO Bool
loaded scnReferenceNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnReferenceNode (mkSelector "loaded") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @referenceNodeWithURL:@
referenceNodeWithURLSelector :: Selector
referenceNodeWithURLSelector = mkSelector "referenceNodeWithURL:"

-- | @Selector@ for @load@
loadSelector :: Selector
loadSelector = mkSelector "load"

-- | @Selector@ for @unload@
unloadSelector :: Selector
unloadSelector = mkSelector "unload"

-- | @Selector@ for @referenceURL@
referenceURLSelector :: Selector
referenceURLSelector = mkSelector "referenceURL"

-- | @Selector@ for @setReferenceURL:@
setReferenceURLSelector :: Selector
setReferenceURLSelector = mkSelector "setReferenceURL:"

-- | @Selector@ for @loadingPolicy@
loadingPolicySelector :: Selector
loadingPolicySelector = mkSelector "loadingPolicy"

-- | @Selector@ for @setLoadingPolicy:@
setLoadingPolicySelector :: Selector
setLoadingPolicySelector = mkSelector "setLoadingPolicy:"

-- | @Selector@ for @loaded@
loadedSelector :: Selector
loadedSelector = mkSelector "loaded"

