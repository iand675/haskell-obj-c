{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKReferenceNode@.
module ObjC.SpriteKit.SKReferenceNode
  ( SKReferenceNode
  , IsSKReferenceNode(..)
  , initWithURL
  , initWithFileNamed
  , initWithCoder
  , referenceNodeWithFileNamed
  , referenceNodeWithURL
  , didLoadReferenceNode
  , resolveReferenceNode
  , initWithURLSelector
  , initWithFileNamedSelector
  , initWithCoderSelector
  , referenceNodeWithFileNamedSelector
  , referenceNodeWithURLSelector
  , didLoadReferenceNodeSelector
  , resolveReferenceNodeSelector


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

import ObjC.SpriteKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a reference node with a url
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsSKReferenceNode skReferenceNode, IsNSURL url) => skReferenceNode -> url -> IO (Id SKReferenceNode)
initWithURL skReferenceNode  url =
withObjCPtr url $ \raw_url ->
    sendMsg skReferenceNode (mkSelector "initWithURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= ownedObject . castPtr

-- | Create a reference node with a url
--
-- ObjC selector: @- initWithFileNamed:@
initWithFileNamed :: (IsSKReferenceNode skReferenceNode, IsNSString fileName) => skReferenceNode -> fileName -> IO (Id SKReferenceNode)
initWithFileNamed skReferenceNode  fileName =
withObjCPtr fileName $ \raw_fileName ->
    sendMsg skReferenceNode (mkSelector "initWithFileNamed:") (retPtr retVoid) [argPtr (castPtr raw_fileName :: Ptr ())] >>= ownedObject . castPtr

-- | Support coding and decoding via NSKeyedArchiver.
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsSKReferenceNode skReferenceNode, IsNSCoder aDecoder) => skReferenceNode -> aDecoder -> IO (Id SKReferenceNode)
initWithCoder skReferenceNode  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg skReferenceNode (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | Create a reference node with a url
--
-- ObjC selector: @+ referenceNodeWithFileNamed:@
referenceNodeWithFileNamed :: IsNSString fileName => fileName -> IO (Id SKReferenceNode)
referenceNodeWithFileNamed fileName =
  do
    cls' <- getRequiredClass "SKReferenceNode"
    withObjCPtr fileName $ \raw_fileName ->
      sendClassMsg cls' (mkSelector "referenceNodeWithFileNamed:") (retPtr retVoid) [argPtr (castPtr raw_fileName :: Ptr ())] >>= retainedObject . castPtr

-- | Create a reference node with a url
--
-- ObjC selector: @+ referenceNodeWithURL:@
referenceNodeWithURL :: IsNSURL referenceURL => referenceURL -> IO (Id SKReferenceNode)
referenceNodeWithURL referenceURL =
  do
    cls' <- getRequiredClass "SKReferenceNode"
    withObjCPtr referenceURL $ \raw_referenceURL ->
      sendClassMsg cls' (mkSelector "referenceNodeWithURL:") (retPtr retVoid) [argPtr (castPtr raw_referenceURL :: Ptr ())] >>= retainedObject . castPtr

-- | called each time the url is loaded, after it has been added as a child
--
-- ObjC selector: @- didLoadReferenceNode:@
didLoadReferenceNode :: (IsSKReferenceNode skReferenceNode, IsSKNode node) => skReferenceNode -> node -> IO ()
didLoadReferenceNode skReferenceNode  node =
withObjCPtr node $ \raw_node ->
    sendMsg skReferenceNode (mkSelector "didLoadReferenceNode:") retVoid [argPtr (castPtr raw_node :: Ptr ())]

-- | Force the reference to be reloaded. The resolved node will added as a child of this node. If the resolved node has not yet been loaded, it will be automatically loaded when the resolved node is queryed or the refenece node is rendered.
--
-- ObjC selector: @- resolveReferenceNode@
resolveReferenceNode :: IsSKReferenceNode skReferenceNode => skReferenceNode -> IO ()
resolveReferenceNode skReferenceNode  =
  sendMsg skReferenceNode (mkSelector "resolveReferenceNode") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @initWithFileNamed:@
initWithFileNamedSelector :: Selector
initWithFileNamedSelector = mkSelector "initWithFileNamed:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @referenceNodeWithFileNamed:@
referenceNodeWithFileNamedSelector :: Selector
referenceNodeWithFileNamedSelector = mkSelector "referenceNodeWithFileNamed:"

-- | @Selector@ for @referenceNodeWithURL:@
referenceNodeWithURLSelector :: Selector
referenceNodeWithURLSelector = mkSelector "referenceNodeWithURL:"

-- | @Selector@ for @didLoadReferenceNode:@
didLoadReferenceNodeSelector :: Selector
didLoadReferenceNodeSelector = mkSelector "didLoadReferenceNode:"

-- | @Selector@ for @resolveReferenceNode@
resolveReferenceNodeSelector :: Selector
resolveReferenceNodeSelector = mkSelector "resolveReferenceNode"

