{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMNamedNodeMap@.
module ObjC.WebKit.DOMNamedNodeMap
  ( DOMNamedNodeMap
  , IsDOMNamedNodeMap(..)
  , getNamedItem
  , setNamedItem
  , removeNamedItem
  , item
  , getNamedItemNS_localName
  , setNamedItemNS
  , removeNamedItemNS_localName
  , getNamedItemNS
  , removeNamedItemNS
  , length_
  , getNamedItemNSSelector
  , getNamedItemNS_localNameSelector
  , getNamedItemSelector
  , itemSelector
  , lengthSelector
  , removeNamedItemNSSelector
  , removeNamedItemNS_localNameSelector
  , removeNamedItemSelector
  , setNamedItemNSSelector
  , setNamedItemSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- getNamedItem:@
getNamedItem :: (IsDOMNamedNodeMap domNamedNodeMap, IsNSString name) => domNamedNodeMap -> name -> IO (Id DOMNode)
getNamedItem domNamedNodeMap name =
  sendMessage domNamedNodeMap getNamedItemSelector (toNSString name)

-- | @- setNamedItem:@
setNamedItem :: (IsDOMNamedNodeMap domNamedNodeMap, IsDOMNode node) => domNamedNodeMap -> node -> IO (Id DOMNode)
setNamedItem domNamedNodeMap node =
  sendMessage domNamedNodeMap setNamedItemSelector (toDOMNode node)

-- | @- removeNamedItem:@
removeNamedItem :: (IsDOMNamedNodeMap domNamedNodeMap, IsNSString name) => domNamedNodeMap -> name -> IO (Id DOMNode)
removeNamedItem domNamedNodeMap name =
  sendMessage domNamedNodeMap removeNamedItemSelector (toNSString name)

-- | @- item:@
item :: IsDOMNamedNodeMap domNamedNodeMap => domNamedNodeMap -> CUInt -> IO (Id DOMNode)
item domNamedNodeMap index =
  sendMessage domNamedNodeMap itemSelector index

-- | @- getNamedItemNS:localName:@
getNamedItemNS_localName :: (IsDOMNamedNodeMap domNamedNodeMap, IsNSString namespaceURI, IsNSString localName) => domNamedNodeMap -> namespaceURI -> localName -> IO (Id DOMNode)
getNamedItemNS_localName domNamedNodeMap namespaceURI localName =
  sendMessage domNamedNodeMap getNamedItemNS_localNameSelector (toNSString namespaceURI) (toNSString localName)

-- | @- setNamedItemNS:@
setNamedItemNS :: (IsDOMNamedNodeMap domNamedNodeMap, IsDOMNode node) => domNamedNodeMap -> node -> IO (Id DOMNode)
setNamedItemNS domNamedNodeMap node =
  sendMessage domNamedNodeMap setNamedItemNSSelector (toDOMNode node)

-- | @- removeNamedItemNS:localName:@
removeNamedItemNS_localName :: (IsDOMNamedNodeMap domNamedNodeMap, IsNSString namespaceURI, IsNSString localName) => domNamedNodeMap -> namespaceURI -> localName -> IO (Id DOMNode)
removeNamedItemNS_localName domNamedNodeMap namespaceURI localName =
  sendMessage domNamedNodeMap removeNamedItemNS_localNameSelector (toNSString namespaceURI) (toNSString localName)

-- | @- getNamedItemNS::@
getNamedItemNS :: (IsDOMNamedNodeMap domNamedNodeMap, IsNSString namespaceURI, IsNSString localName) => domNamedNodeMap -> namespaceURI -> localName -> IO (Id DOMNode)
getNamedItemNS domNamedNodeMap namespaceURI localName =
  sendMessage domNamedNodeMap getNamedItemNSSelector (toNSString namespaceURI) (toNSString localName)

-- | @- removeNamedItemNS::@
removeNamedItemNS :: (IsDOMNamedNodeMap domNamedNodeMap, IsNSString namespaceURI, IsNSString localName) => domNamedNodeMap -> namespaceURI -> localName -> IO (Id DOMNode)
removeNamedItemNS domNamedNodeMap namespaceURI localName =
  sendMessage domNamedNodeMap removeNamedItemNSSelector (toNSString namespaceURI) (toNSString localName)

-- | @- length@
length_ :: IsDOMNamedNodeMap domNamedNodeMap => domNamedNodeMap -> IO CUInt
length_ domNamedNodeMap =
  sendMessage domNamedNodeMap lengthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getNamedItem:@
getNamedItemSelector :: Selector '[Id NSString] (Id DOMNode)
getNamedItemSelector = mkSelector "getNamedItem:"

-- | @Selector@ for @setNamedItem:@
setNamedItemSelector :: Selector '[Id DOMNode] (Id DOMNode)
setNamedItemSelector = mkSelector "setNamedItem:"

-- | @Selector@ for @removeNamedItem:@
removeNamedItemSelector :: Selector '[Id NSString] (Id DOMNode)
removeNamedItemSelector = mkSelector "removeNamedItem:"

-- | @Selector@ for @item:@
itemSelector :: Selector '[CUInt] (Id DOMNode)
itemSelector = mkSelector "item:"

-- | @Selector@ for @getNamedItemNS:localName:@
getNamedItemNS_localNameSelector :: Selector '[Id NSString, Id NSString] (Id DOMNode)
getNamedItemNS_localNameSelector = mkSelector "getNamedItemNS:localName:"

-- | @Selector@ for @setNamedItemNS:@
setNamedItemNSSelector :: Selector '[Id DOMNode] (Id DOMNode)
setNamedItemNSSelector = mkSelector "setNamedItemNS:"

-- | @Selector@ for @removeNamedItemNS:localName:@
removeNamedItemNS_localNameSelector :: Selector '[Id NSString, Id NSString] (Id DOMNode)
removeNamedItemNS_localNameSelector = mkSelector "removeNamedItemNS:localName:"

-- | @Selector@ for @getNamedItemNS::@
getNamedItemNSSelector :: Selector '[Id NSString, Id NSString] (Id DOMNode)
getNamedItemNSSelector = mkSelector "getNamedItemNS::"

-- | @Selector@ for @removeNamedItemNS::@
removeNamedItemNSSelector :: Selector '[Id NSString, Id NSString] (Id DOMNode)
removeNamedItemNSSelector = mkSelector "removeNamedItemNS::"

-- | @Selector@ for @length@
lengthSelector :: Selector '[] CUInt
lengthSelector = mkSelector "length"

