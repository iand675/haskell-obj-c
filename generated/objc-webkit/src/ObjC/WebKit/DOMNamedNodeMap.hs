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
  , getNamedItemSelector
  , setNamedItemSelector
  , removeNamedItemSelector
  , itemSelector
  , getNamedItemNS_localNameSelector
  , setNamedItemNSSelector
  , removeNamedItemNS_localNameSelector
  , getNamedItemNSSelector
  , removeNamedItemNSSelector
  , lengthSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- getNamedItem:@
getNamedItem :: (IsDOMNamedNodeMap domNamedNodeMap, IsNSString name) => domNamedNodeMap -> name -> IO (Id DOMNode)
getNamedItem domNamedNodeMap  name =
withObjCPtr name $ \raw_name ->
    sendMsg domNamedNodeMap (mkSelector "getNamedItem:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- setNamedItem:@
setNamedItem :: (IsDOMNamedNodeMap domNamedNodeMap, IsDOMNode node) => domNamedNodeMap -> node -> IO (Id DOMNode)
setNamedItem domNamedNodeMap  node =
withObjCPtr node $ \raw_node ->
    sendMsg domNamedNodeMap (mkSelector "setNamedItem:") (retPtr retVoid) [argPtr (castPtr raw_node :: Ptr ())] >>= retainedObject . castPtr

-- | @- removeNamedItem:@
removeNamedItem :: (IsDOMNamedNodeMap domNamedNodeMap, IsNSString name) => domNamedNodeMap -> name -> IO (Id DOMNode)
removeNamedItem domNamedNodeMap  name =
withObjCPtr name $ \raw_name ->
    sendMsg domNamedNodeMap (mkSelector "removeNamedItem:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- item:@
item :: IsDOMNamedNodeMap domNamedNodeMap => domNamedNodeMap -> CUInt -> IO (Id DOMNode)
item domNamedNodeMap  index =
  sendMsg domNamedNodeMap (mkSelector "item:") (retPtr retVoid) [argCUInt (fromIntegral index)] >>= retainedObject . castPtr

-- | @- getNamedItemNS:localName:@
getNamedItemNS_localName :: (IsDOMNamedNodeMap domNamedNodeMap, IsNSString namespaceURI, IsNSString localName) => domNamedNodeMap -> namespaceURI -> localName -> IO (Id DOMNode)
getNamedItemNS_localName domNamedNodeMap  namespaceURI localName =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr localName $ \raw_localName ->
      sendMsg domNamedNodeMap (mkSelector "getNamedItemNS:localName:") (retPtr retVoid) [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_localName :: Ptr ())] >>= retainedObject . castPtr

-- | @- setNamedItemNS:@
setNamedItemNS :: (IsDOMNamedNodeMap domNamedNodeMap, IsDOMNode node) => domNamedNodeMap -> node -> IO (Id DOMNode)
setNamedItemNS domNamedNodeMap  node =
withObjCPtr node $ \raw_node ->
    sendMsg domNamedNodeMap (mkSelector "setNamedItemNS:") (retPtr retVoid) [argPtr (castPtr raw_node :: Ptr ())] >>= retainedObject . castPtr

-- | @- removeNamedItemNS:localName:@
removeNamedItemNS_localName :: (IsDOMNamedNodeMap domNamedNodeMap, IsNSString namespaceURI, IsNSString localName) => domNamedNodeMap -> namespaceURI -> localName -> IO (Id DOMNode)
removeNamedItemNS_localName domNamedNodeMap  namespaceURI localName =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr localName $ \raw_localName ->
      sendMsg domNamedNodeMap (mkSelector "removeNamedItemNS:localName:") (retPtr retVoid) [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_localName :: Ptr ())] >>= retainedObject . castPtr

-- | @- getNamedItemNS::@
getNamedItemNS :: (IsDOMNamedNodeMap domNamedNodeMap, IsNSString namespaceURI, IsNSString localName) => domNamedNodeMap -> namespaceURI -> localName -> IO (Id DOMNode)
getNamedItemNS domNamedNodeMap  namespaceURI localName =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr localName $ \raw_localName ->
      sendMsg domNamedNodeMap (mkSelector "getNamedItemNS::") (retPtr retVoid) [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_localName :: Ptr ())] >>= retainedObject . castPtr

-- | @- removeNamedItemNS::@
removeNamedItemNS :: (IsDOMNamedNodeMap domNamedNodeMap, IsNSString namespaceURI, IsNSString localName) => domNamedNodeMap -> namespaceURI -> localName -> IO (Id DOMNode)
removeNamedItemNS domNamedNodeMap  namespaceURI localName =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr localName $ \raw_localName ->
      sendMsg domNamedNodeMap (mkSelector "removeNamedItemNS::") (retPtr retVoid) [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_localName :: Ptr ())] >>= retainedObject . castPtr

-- | @- length@
length_ :: IsDOMNamedNodeMap domNamedNodeMap => domNamedNodeMap -> IO CUInt
length_ domNamedNodeMap  =
  sendMsg domNamedNodeMap (mkSelector "length") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getNamedItem:@
getNamedItemSelector :: Selector
getNamedItemSelector = mkSelector "getNamedItem:"

-- | @Selector@ for @setNamedItem:@
setNamedItemSelector :: Selector
setNamedItemSelector = mkSelector "setNamedItem:"

-- | @Selector@ for @removeNamedItem:@
removeNamedItemSelector :: Selector
removeNamedItemSelector = mkSelector "removeNamedItem:"

-- | @Selector@ for @item:@
itemSelector :: Selector
itemSelector = mkSelector "item:"

-- | @Selector@ for @getNamedItemNS:localName:@
getNamedItemNS_localNameSelector :: Selector
getNamedItemNS_localNameSelector = mkSelector "getNamedItemNS:localName:"

-- | @Selector@ for @setNamedItemNS:@
setNamedItemNSSelector :: Selector
setNamedItemNSSelector = mkSelector "setNamedItemNS:"

-- | @Selector@ for @removeNamedItemNS:localName:@
removeNamedItemNS_localNameSelector :: Selector
removeNamedItemNS_localNameSelector = mkSelector "removeNamedItemNS:localName:"

-- | @Selector@ for @getNamedItemNS::@
getNamedItemNSSelector :: Selector
getNamedItemNSSelector = mkSelector "getNamedItemNS::"

-- | @Selector@ for @removeNamedItemNS::@
removeNamedItemNSSelector :: Selector
removeNamedItemNSSelector = mkSelector "removeNamedItemNS::"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

