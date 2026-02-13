{-# LANGUAGE DataKinds #-}
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
  , didLoadReferenceNodeSelector
  , initWithCoderSelector
  , initWithFileNamedSelector
  , initWithURLSelector
  , referenceNodeWithFileNamedSelector
  , referenceNodeWithURLSelector
  , resolveReferenceNodeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SpriteKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a reference node with a url
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsSKReferenceNode skReferenceNode, IsNSURL url) => skReferenceNode -> url -> IO (Id SKReferenceNode)
initWithURL skReferenceNode url =
  sendOwnedMessage skReferenceNode initWithURLSelector (toNSURL url)

-- | Create a reference node with a url
--
-- ObjC selector: @- initWithFileNamed:@
initWithFileNamed :: (IsSKReferenceNode skReferenceNode, IsNSString fileName) => skReferenceNode -> fileName -> IO (Id SKReferenceNode)
initWithFileNamed skReferenceNode fileName =
  sendOwnedMessage skReferenceNode initWithFileNamedSelector (toNSString fileName)

-- | Support coding and decoding via NSKeyedArchiver.
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsSKReferenceNode skReferenceNode, IsNSCoder aDecoder) => skReferenceNode -> aDecoder -> IO (Id SKReferenceNode)
initWithCoder skReferenceNode aDecoder =
  sendOwnedMessage skReferenceNode initWithCoderSelector (toNSCoder aDecoder)

-- | Create a reference node with a url
--
-- ObjC selector: @+ referenceNodeWithFileNamed:@
referenceNodeWithFileNamed :: IsNSString fileName => fileName -> IO (Id SKReferenceNode)
referenceNodeWithFileNamed fileName =
  do
    cls' <- getRequiredClass "SKReferenceNode"
    sendClassMessage cls' referenceNodeWithFileNamedSelector (toNSString fileName)

-- | Create a reference node with a url
--
-- ObjC selector: @+ referenceNodeWithURL:@
referenceNodeWithURL :: IsNSURL referenceURL => referenceURL -> IO (Id SKReferenceNode)
referenceNodeWithURL referenceURL =
  do
    cls' <- getRequiredClass "SKReferenceNode"
    sendClassMessage cls' referenceNodeWithURLSelector (toNSURL referenceURL)

-- | called each time the url is loaded, after it has been added as a child
--
-- ObjC selector: @- didLoadReferenceNode:@
didLoadReferenceNode :: (IsSKReferenceNode skReferenceNode, IsSKNode node) => skReferenceNode -> node -> IO ()
didLoadReferenceNode skReferenceNode node =
  sendMessage skReferenceNode didLoadReferenceNodeSelector (toSKNode node)

-- | Force the reference to be reloaded. The resolved node will added as a child of this node. If the resolved node has not yet been loaded, it will be automatically loaded when the resolved node is queryed or the refenece node is rendered.
--
-- ObjC selector: @- resolveReferenceNode@
resolveReferenceNode :: IsSKReferenceNode skReferenceNode => skReferenceNode -> IO ()
resolveReferenceNode skReferenceNode =
  sendMessage skReferenceNode resolveReferenceNodeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector '[Id NSURL] (Id SKReferenceNode)
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @initWithFileNamed:@
initWithFileNamedSelector :: Selector '[Id NSString] (Id SKReferenceNode)
initWithFileNamedSelector = mkSelector "initWithFileNamed:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id SKReferenceNode)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @referenceNodeWithFileNamed:@
referenceNodeWithFileNamedSelector :: Selector '[Id NSString] (Id SKReferenceNode)
referenceNodeWithFileNamedSelector = mkSelector "referenceNodeWithFileNamed:"

-- | @Selector@ for @referenceNodeWithURL:@
referenceNodeWithURLSelector :: Selector '[Id NSURL] (Id SKReferenceNode)
referenceNodeWithURLSelector = mkSelector "referenceNodeWithURL:"

-- | @Selector@ for @didLoadReferenceNode:@
didLoadReferenceNodeSelector :: Selector '[Id SKNode] ()
didLoadReferenceNodeSelector = mkSelector "didLoadReferenceNode:"

-- | @Selector@ for @resolveReferenceNode@
resolveReferenceNodeSelector :: Selector '[] ()
resolveReferenceNodeSelector = mkSelector "resolveReferenceNode"

