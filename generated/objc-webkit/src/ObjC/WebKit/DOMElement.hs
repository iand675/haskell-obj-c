{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMElement@.
module ObjC.WebKit.DOMElement
  ( DOMElement
  , IsDOMElement(..)
  , getAttribute
  , setAttribute_value
  , removeAttribute
  , getAttributeNode
  , setAttributeNode
  , removeAttributeNode
  , getElementsByTagName
  , getAttributeNS_localName
  , setAttributeNS_qualifiedName_value
  , removeAttributeNS_localName
  , getElementsByTagNameNS_localName
  , getAttributeNodeNS_localName
  , setAttributeNodeNS
  , hasAttribute
  , hasAttributeNS_localName
  , focus
  , blur
  , scrollIntoView
  , scrollIntoViewIfNeeded
  , getElementsByClassName
  , webkitRequestFullScreen
  , querySelector
  , querySelectorAll
  , image
  , setAttribute
  , getAttributeNS
  , setAttributeNS
  , removeAttributeNS
  , getElementsByTagNameNS
  , getAttributeNodeNS
  , hasAttributeNS
  , scrollByLines
  , scrollByPages
  , tagName
  , style
  , offsetLeft
  , offsetTop
  , offsetWidth
  , offsetHeight
  , clientLeft
  , clientTop
  , clientWidth
  , clientHeight
  , scrollLeft
  , setScrollLeft
  , scrollTop
  , setScrollTop
  , scrollWidth
  , scrollHeight
  , offsetParent
  , innerHTML
  , setInnerHTML
  , outerHTML
  , setOuterHTML
  , className
  , setClassName
  , innerText
  , previousElementSibling
  , nextElementSibling
  , firstElementChild
  , lastElementChild
  , childElementCount
  , getAttributeSelector
  , setAttribute_valueSelector
  , removeAttributeSelector
  , getAttributeNodeSelector
  , setAttributeNodeSelector
  , removeAttributeNodeSelector
  , getElementsByTagNameSelector
  , getAttributeNS_localNameSelector
  , setAttributeNS_qualifiedName_valueSelector
  , removeAttributeNS_localNameSelector
  , getElementsByTagNameNS_localNameSelector
  , getAttributeNodeNS_localNameSelector
  , setAttributeNodeNSSelector
  , hasAttributeSelector
  , hasAttributeNS_localNameSelector
  , focusSelector
  , blurSelector
  , scrollIntoViewSelector
  , scrollIntoViewIfNeededSelector
  , getElementsByClassNameSelector
  , webkitRequestFullScreenSelector
  , querySelectorSelector
  , querySelectorAllSelector
  , imageSelector
  , setAttributeSelector
  , getAttributeNSSelector
  , setAttributeNSSelector
  , removeAttributeNSSelector
  , getElementsByTagNameNSSelector
  , getAttributeNodeNSSelector
  , hasAttributeNSSelector
  , scrollByLinesSelector
  , scrollByPagesSelector
  , tagNameSelector
  , styleSelector
  , offsetLeftSelector
  , offsetTopSelector
  , offsetWidthSelector
  , offsetHeightSelector
  , clientLeftSelector
  , clientTopSelector
  , clientWidthSelector
  , clientHeightSelector
  , scrollLeftSelector
  , setScrollLeftSelector
  , scrollTopSelector
  , setScrollTopSelector
  , scrollWidthSelector
  , scrollHeightSelector
  , offsetParentSelector
  , innerHTMLSelector
  , setInnerHTMLSelector
  , outerHTMLSelector
  , setOuterHTMLSelector
  , classNameSelector
  , setClassNameSelector
  , innerTextSelector
  , previousElementSiblingSelector
  , nextElementSiblingSelector
  , firstElementChildSelector
  , lastElementChildSelector
  , childElementCountSelector


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
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- getAttribute:@
getAttribute :: (IsDOMElement domElement, IsNSString name) => domElement -> name -> IO (Id NSString)
getAttribute domElement  name =
withObjCPtr name $ \raw_name ->
    sendMsg domElement (mkSelector "getAttribute:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- setAttribute:value:@
setAttribute_value :: (IsDOMElement domElement, IsNSString name, IsNSString value) => domElement -> name -> value -> IO ()
setAttribute_value domElement  name value =
withObjCPtr name $ \raw_name ->
  withObjCPtr value $ \raw_value ->
      sendMsg domElement (mkSelector "setAttribute:value:") retVoid [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_value :: Ptr ())]

-- | @- removeAttribute:@
removeAttribute :: (IsDOMElement domElement, IsNSString name) => domElement -> name -> IO ()
removeAttribute domElement  name =
withObjCPtr name $ \raw_name ->
    sendMsg domElement (mkSelector "removeAttribute:") retVoid [argPtr (castPtr raw_name :: Ptr ())]

-- | @- getAttributeNode:@
getAttributeNode :: (IsDOMElement domElement, IsNSString name) => domElement -> name -> IO (Id DOMAttr)
getAttributeNode domElement  name =
withObjCPtr name $ \raw_name ->
    sendMsg domElement (mkSelector "getAttributeNode:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- setAttributeNode:@
setAttributeNode :: (IsDOMElement domElement, IsDOMAttr newAttr) => domElement -> newAttr -> IO (Id DOMAttr)
setAttributeNode domElement  newAttr =
withObjCPtr newAttr $ \raw_newAttr ->
    sendMsg domElement (mkSelector "setAttributeNode:") (retPtr retVoid) [argPtr (castPtr raw_newAttr :: Ptr ())] >>= retainedObject . castPtr

-- | @- removeAttributeNode:@
removeAttributeNode :: (IsDOMElement domElement, IsDOMAttr oldAttr) => domElement -> oldAttr -> IO (Id DOMAttr)
removeAttributeNode domElement  oldAttr =
withObjCPtr oldAttr $ \raw_oldAttr ->
    sendMsg domElement (mkSelector "removeAttributeNode:") (retPtr retVoid) [argPtr (castPtr raw_oldAttr :: Ptr ())] >>= retainedObject . castPtr

-- | @- getElementsByTagName:@
getElementsByTagName :: (IsDOMElement domElement, IsNSString name) => domElement -> name -> IO (Id DOMNodeList)
getElementsByTagName domElement  name =
withObjCPtr name $ \raw_name ->
    sendMsg domElement (mkSelector "getElementsByTagName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- getAttributeNS:localName:@
getAttributeNS_localName :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString localName) => domElement -> namespaceURI -> localName -> IO (Id NSString)
getAttributeNS_localName domElement  namespaceURI localName =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr localName $ \raw_localName ->
      sendMsg domElement (mkSelector "getAttributeNS:localName:") (retPtr retVoid) [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_localName :: Ptr ())] >>= retainedObject . castPtr

-- | @- setAttributeNS:qualifiedName:value:@
setAttributeNS_qualifiedName_value :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString qualifiedName, IsNSString value) => domElement -> namespaceURI -> qualifiedName -> value -> IO ()
setAttributeNS_qualifiedName_value domElement  namespaceURI qualifiedName value =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr qualifiedName $ \raw_qualifiedName ->
    withObjCPtr value $ \raw_value ->
        sendMsg domElement (mkSelector "setAttributeNS:qualifiedName:value:") retVoid [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_qualifiedName :: Ptr ()), argPtr (castPtr raw_value :: Ptr ())]

-- | @- removeAttributeNS:localName:@
removeAttributeNS_localName :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString localName) => domElement -> namespaceURI -> localName -> IO ()
removeAttributeNS_localName domElement  namespaceURI localName =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr localName $ \raw_localName ->
      sendMsg domElement (mkSelector "removeAttributeNS:localName:") retVoid [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_localName :: Ptr ())]

-- | @- getElementsByTagNameNS:localName:@
getElementsByTagNameNS_localName :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString localName) => domElement -> namespaceURI -> localName -> IO (Id DOMNodeList)
getElementsByTagNameNS_localName domElement  namespaceURI localName =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr localName $ \raw_localName ->
      sendMsg domElement (mkSelector "getElementsByTagNameNS:localName:") (retPtr retVoid) [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_localName :: Ptr ())] >>= retainedObject . castPtr

-- | @- getAttributeNodeNS:localName:@
getAttributeNodeNS_localName :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString localName) => domElement -> namespaceURI -> localName -> IO (Id DOMAttr)
getAttributeNodeNS_localName domElement  namespaceURI localName =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr localName $ \raw_localName ->
      sendMsg domElement (mkSelector "getAttributeNodeNS:localName:") (retPtr retVoid) [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_localName :: Ptr ())] >>= retainedObject . castPtr

-- | @- setAttributeNodeNS:@
setAttributeNodeNS :: (IsDOMElement domElement, IsDOMAttr newAttr) => domElement -> newAttr -> IO (Id DOMAttr)
setAttributeNodeNS domElement  newAttr =
withObjCPtr newAttr $ \raw_newAttr ->
    sendMsg domElement (mkSelector "setAttributeNodeNS:") (retPtr retVoid) [argPtr (castPtr raw_newAttr :: Ptr ())] >>= retainedObject . castPtr

-- | @- hasAttribute:@
hasAttribute :: (IsDOMElement domElement, IsNSString name) => domElement -> name -> IO Bool
hasAttribute domElement  name =
withObjCPtr name $ \raw_name ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg domElement (mkSelector "hasAttribute:") retCULong [argPtr (castPtr raw_name :: Ptr ())]

-- | @- hasAttributeNS:localName:@
hasAttributeNS_localName :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString localName) => domElement -> namespaceURI -> localName -> IO Bool
hasAttributeNS_localName domElement  namespaceURI localName =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr localName $ \raw_localName ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg domElement (mkSelector "hasAttributeNS:localName:") retCULong [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_localName :: Ptr ())]

-- | @- focus@
focus :: IsDOMElement domElement => domElement -> IO ()
focus domElement  =
  sendMsg domElement (mkSelector "focus") retVoid []

-- | @- blur@
blur :: IsDOMElement domElement => domElement -> IO ()
blur domElement  =
  sendMsg domElement (mkSelector "blur") retVoid []

-- | @- scrollIntoView:@
scrollIntoView :: IsDOMElement domElement => domElement -> Bool -> IO ()
scrollIntoView domElement  alignWithTop =
  sendMsg domElement (mkSelector "scrollIntoView:") retVoid [argCULong (if alignWithTop then 1 else 0)]

-- | @- scrollIntoViewIfNeeded:@
scrollIntoViewIfNeeded :: IsDOMElement domElement => domElement -> Bool -> IO ()
scrollIntoViewIfNeeded domElement  centerIfNeeded =
  sendMsg domElement (mkSelector "scrollIntoViewIfNeeded:") retVoid [argCULong (if centerIfNeeded then 1 else 0)]

-- | @- getElementsByClassName:@
getElementsByClassName :: (IsDOMElement domElement, IsNSString name) => domElement -> name -> IO (Id DOMNodeList)
getElementsByClassName domElement  name =
withObjCPtr name $ \raw_name ->
    sendMsg domElement (mkSelector "getElementsByClassName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- webkitRequestFullScreen:@
webkitRequestFullScreen :: IsDOMElement domElement => domElement -> CUShort -> IO ()
webkitRequestFullScreen domElement  flags =
  sendMsg domElement (mkSelector "webkitRequestFullScreen:") retVoid [argCUInt (fromIntegral flags)]

-- | @- querySelector:@
querySelector :: (IsDOMElement domElement, IsNSString selectors) => domElement -> selectors -> IO (Id DOMElement)
querySelector domElement  selectors =
withObjCPtr selectors $ \raw_selectors ->
    sendMsg domElement (mkSelector "querySelector:") (retPtr retVoid) [argPtr (castPtr raw_selectors :: Ptr ())] >>= retainedObject . castPtr

-- | @- querySelectorAll:@
querySelectorAll :: (IsDOMElement domElement, IsNSString selectors) => domElement -> selectors -> IO (Id DOMNodeList)
querySelectorAll domElement  selectors =
withObjCPtr selectors $ \raw_selectors ->
    sendMsg domElement (mkSelector "querySelectorAll:") (retPtr retVoid) [argPtr (castPtr raw_selectors :: Ptr ())] >>= retainedObject . castPtr

-- | @- image@
image :: IsDOMElement domElement => domElement -> IO (Id NSImage)
image domElement  =
  sendMsg domElement (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttribute::@
setAttribute :: (IsDOMElement domElement, IsNSString name, IsNSString value) => domElement -> name -> value -> IO ()
setAttribute domElement  name value =
withObjCPtr name $ \raw_name ->
  withObjCPtr value $ \raw_value ->
      sendMsg domElement (mkSelector "setAttribute::") retVoid [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_value :: Ptr ())]

-- | @- getAttributeNS::@
getAttributeNS :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString localName) => domElement -> namespaceURI -> localName -> IO (Id NSString)
getAttributeNS domElement  namespaceURI localName =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr localName $ \raw_localName ->
      sendMsg domElement (mkSelector "getAttributeNS::") (retPtr retVoid) [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_localName :: Ptr ())] >>= retainedObject . castPtr

-- | @- setAttributeNS:::@
setAttributeNS :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString qualifiedName, IsNSString value) => domElement -> namespaceURI -> qualifiedName -> value -> IO ()
setAttributeNS domElement  namespaceURI qualifiedName value =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr qualifiedName $ \raw_qualifiedName ->
    withObjCPtr value $ \raw_value ->
        sendMsg domElement (mkSelector "setAttributeNS:::") retVoid [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_qualifiedName :: Ptr ()), argPtr (castPtr raw_value :: Ptr ())]

-- | @- removeAttributeNS::@
removeAttributeNS :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString localName) => domElement -> namespaceURI -> localName -> IO ()
removeAttributeNS domElement  namespaceURI localName =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr localName $ \raw_localName ->
      sendMsg domElement (mkSelector "removeAttributeNS::") retVoid [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_localName :: Ptr ())]

-- | @- getElementsByTagNameNS::@
getElementsByTagNameNS :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString localName) => domElement -> namespaceURI -> localName -> IO (Id DOMNodeList)
getElementsByTagNameNS domElement  namespaceURI localName =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr localName $ \raw_localName ->
      sendMsg domElement (mkSelector "getElementsByTagNameNS::") (retPtr retVoid) [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_localName :: Ptr ())] >>= retainedObject . castPtr

-- | @- getAttributeNodeNS::@
getAttributeNodeNS :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString localName) => domElement -> namespaceURI -> localName -> IO (Id DOMAttr)
getAttributeNodeNS domElement  namespaceURI localName =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr localName $ \raw_localName ->
      sendMsg domElement (mkSelector "getAttributeNodeNS::") (retPtr retVoid) [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_localName :: Ptr ())] >>= retainedObject . castPtr

-- | @- hasAttributeNS::@
hasAttributeNS :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString localName) => domElement -> namespaceURI -> localName -> IO Bool
hasAttributeNS domElement  namespaceURI localName =
withObjCPtr namespaceURI $ \raw_namespaceURI ->
  withObjCPtr localName $ \raw_localName ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg domElement (mkSelector "hasAttributeNS::") retCULong [argPtr (castPtr raw_namespaceURI :: Ptr ()), argPtr (castPtr raw_localName :: Ptr ())]

-- | @- scrollByLines:@
scrollByLines :: IsDOMElement domElement => domElement -> CInt -> IO ()
scrollByLines domElement  lines_ =
  sendMsg domElement (mkSelector "scrollByLines:") retVoid [argCInt (fromIntegral lines_)]

-- | @- scrollByPages:@
scrollByPages :: IsDOMElement domElement => domElement -> CInt -> IO ()
scrollByPages domElement  pages =
  sendMsg domElement (mkSelector "scrollByPages:") retVoid [argCInt (fromIntegral pages)]

-- | @- tagName@
tagName :: IsDOMElement domElement => domElement -> IO (Id NSString)
tagName domElement  =
  sendMsg domElement (mkSelector "tagName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- style@
style :: IsDOMElement domElement => domElement -> IO (Id DOMCSSStyleDeclaration)
style domElement  =
  sendMsg domElement (mkSelector "style") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- offsetLeft@
offsetLeft :: IsDOMElement domElement => domElement -> IO CInt
offsetLeft domElement  =
  sendMsg domElement (mkSelector "offsetLeft") retCInt []

-- | @- offsetTop@
offsetTop :: IsDOMElement domElement => domElement -> IO CInt
offsetTop domElement  =
  sendMsg domElement (mkSelector "offsetTop") retCInt []

-- | @- offsetWidth@
offsetWidth :: IsDOMElement domElement => domElement -> IO CInt
offsetWidth domElement  =
  sendMsg domElement (mkSelector "offsetWidth") retCInt []

-- | @- offsetHeight@
offsetHeight :: IsDOMElement domElement => domElement -> IO CInt
offsetHeight domElement  =
  sendMsg domElement (mkSelector "offsetHeight") retCInt []

-- | @- clientLeft@
clientLeft :: IsDOMElement domElement => domElement -> IO CInt
clientLeft domElement  =
  sendMsg domElement (mkSelector "clientLeft") retCInt []

-- | @- clientTop@
clientTop :: IsDOMElement domElement => domElement -> IO CInt
clientTop domElement  =
  sendMsg domElement (mkSelector "clientTop") retCInt []

-- | @- clientWidth@
clientWidth :: IsDOMElement domElement => domElement -> IO CInt
clientWidth domElement  =
  sendMsg domElement (mkSelector "clientWidth") retCInt []

-- | @- clientHeight@
clientHeight :: IsDOMElement domElement => domElement -> IO CInt
clientHeight domElement  =
  sendMsg domElement (mkSelector "clientHeight") retCInt []

-- | @- scrollLeft@
scrollLeft :: IsDOMElement domElement => domElement -> IO CInt
scrollLeft domElement  =
  sendMsg domElement (mkSelector "scrollLeft") retCInt []

-- | @- setScrollLeft:@
setScrollLeft :: IsDOMElement domElement => domElement -> CInt -> IO ()
setScrollLeft domElement  value =
  sendMsg domElement (mkSelector "setScrollLeft:") retVoid [argCInt (fromIntegral value)]

-- | @- scrollTop@
scrollTop :: IsDOMElement domElement => domElement -> IO CInt
scrollTop domElement  =
  sendMsg domElement (mkSelector "scrollTop") retCInt []

-- | @- setScrollTop:@
setScrollTop :: IsDOMElement domElement => domElement -> CInt -> IO ()
setScrollTop domElement  value =
  sendMsg domElement (mkSelector "setScrollTop:") retVoid [argCInt (fromIntegral value)]

-- | @- scrollWidth@
scrollWidth :: IsDOMElement domElement => domElement -> IO CInt
scrollWidth domElement  =
  sendMsg domElement (mkSelector "scrollWidth") retCInt []

-- | @- scrollHeight@
scrollHeight :: IsDOMElement domElement => domElement -> IO CInt
scrollHeight domElement  =
  sendMsg domElement (mkSelector "scrollHeight") retCInt []

-- | @- offsetParent@
offsetParent :: IsDOMElement domElement => domElement -> IO (Id DOMElement)
offsetParent domElement  =
  sendMsg domElement (mkSelector "offsetParent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- innerHTML@
innerHTML :: IsDOMElement domElement => domElement -> IO (Id NSString)
innerHTML domElement  =
  sendMsg domElement (mkSelector "innerHTML") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInnerHTML:@
setInnerHTML :: (IsDOMElement domElement, IsNSString value) => domElement -> value -> IO ()
setInnerHTML domElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domElement (mkSelector "setInnerHTML:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- outerHTML@
outerHTML :: IsDOMElement domElement => domElement -> IO (Id NSString)
outerHTML domElement  =
  sendMsg domElement (mkSelector "outerHTML") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOuterHTML:@
setOuterHTML :: (IsDOMElement domElement, IsNSString value) => domElement -> value -> IO ()
setOuterHTML domElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domElement (mkSelector "setOuterHTML:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- className@
className :: IsDOMElement domElement => domElement -> IO (Id NSString)
className domElement  =
  sendMsg domElement (mkSelector "className") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setClassName:@
setClassName :: (IsDOMElement domElement, IsNSString value) => domElement -> value -> IO ()
setClassName domElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domElement (mkSelector "setClassName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- innerText@
innerText :: IsDOMElement domElement => domElement -> IO (Id NSString)
innerText domElement  =
  sendMsg domElement (mkSelector "innerText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- previousElementSibling@
previousElementSibling :: IsDOMElement domElement => domElement -> IO (Id DOMElement)
previousElementSibling domElement  =
  sendMsg domElement (mkSelector "previousElementSibling") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- nextElementSibling@
nextElementSibling :: IsDOMElement domElement => domElement -> IO (Id DOMElement)
nextElementSibling domElement  =
  sendMsg domElement (mkSelector "nextElementSibling") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- firstElementChild@
firstElementChild :: IsDOMElement domElement => domElement -> IO (Id DOMElement)
firstElementChild domElement  =
  sendMsg domElement (mkSelector "firstElementChild") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- lastElementChild@
lastElementChild :: IsDOMElement domElement => domElement -> IO (Id DOMElement)
lastElementChild domElement  =
  sendMsg domElement (mkSelector "lastElementChild") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- childElementCount@
childElementCount :: IsDOMElement domElement => domElement -> IO CUInt
childElementCount domElement  =
  sendMsg domElement (mkSelector "childElementCount") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getAttribute:@
getAttributeSelector :: Selector
getAttributeSelector = mkSelector "getAttribute:"

-- | @Selector@ for @setAttribute:value:@
setAttribute_valueSelector :: Selector
setAttribute_valueSelector = mkSelector "setAttribute:value:"

-- | @Selector@ for @removeAttribute:@
removeAttributeSelector :: Selector
removeAttributeSelector = mkSelector "removeAttribute:"

-- | @Selector@ for @getAttributeNode:@
getAttributeNodeSelector :: Selector
getAttributeNodeSelector = mkSelector "getAttributeNode:"

-- | @Selector@ for @setAttributeNode:@
setAttributeNodeSelector :: Selector
setAttributeNodeSelector = mkSelector "setAttributeNode:"

-- | @Selector@ for @removeAttributeNode:@
removeAttributeNodeSelector :: Selector
removeAttributeNodeSelector = mkSelector "removeAttributeNode:"

-- | @Selector@ for @getElementsByTagName:@
getElementsByTagNameSelector :: Selector
getElementsByTagNameSelector = mkSelector "getElementsByTagName:"

-- | @Selector@ for @getAttributeNS:localName:@
getAttributeNS_localNameSelector :: Selector
getAttributeNS_localNameSelector = mkSelector "getAttributeNS:localName:"

-- | @Selector@ for @setAttributeNS:qualifiedName:value:@
setAttributeNS_qualifiedName_valueSelector :: Selector
setAttributeNS_qualifiedName_valueSelector = mkSelector "setAttributeNS:qualifiedName:value:"

-- | @Selector@ for @removeAttributeNS:localName:@
removeAttributeNS_localNameSelector :: Selector
removeAttributeNS_localNameSelector = mkSelector "removeAttributeNS:localName:"

-- | @Selector@ for @getElementsByTagNameNS:localName:@
getElementsByTagNameNS_localNameSelector :: Selector
getElementsByTagNameNS_localNameSelector = mkSelector "getElementsByTagNameNS:localName:"

-- | @Selector@ for @getAttributeNodeNS:localName:@
getAttributeNodeNS_localNameSelector :: Selector
getAttributeNodeNS_localNameSelector = mkSelector "getAttributeNodeNS:localName:"

-- | @Selector@ for @setAttributeNodeNS:@
setAttributeNodeNSSelector :: Selector
setAttributeNodeNSSelector = mkSelector "setAttributeNodeNS:"

-- | @Selector@ for @hasAttribute:@
hasAttributeSelector :: Selector
hasAttributeSelector = mkSelector "hasAttribute:"

-- | @Selector@ for @hasAttributeNS:localName:@
hasAttributeNS_localNameSelector :: Selector
hasAttributeNS_localNameSelector = mkSelector "hasAttributeNS:localName:"

-- | @Selector@ for @focus@
focusSelector :: Selector
focusSelector = mkSelector "focus"

-- | @Selector@ for @blur@
blurSelector :: Selector
blurSelector = mkSelector "blur"

-- | @Selector@ for @scrollIntoView:@
scrollIntoViewSelector :: Selector
scrollIntoViewSelector = mkSelector "scrollIntoView:"

-- | @Selector@ for @scrollIntoViewIfNeeded:@
scrollIntoViewIfNeededSelector :: Selector
scrollIntoViewIfNeededSelector = mkSelector "scrollIntoViewIfNeeded:"

-- | @Selector@ for @getElementsByClassName:@
getElementsByClassNameSelector :: Selector
getElementsByClassNameSelector = mkSelector "getElementsByClassName:"

-- | @Selector@ for @webkitRequestFullScreen:@
webkitRequestFullScreenSelector :: Selector
webkitRequestFullScreenSelector = mkSelector "webkitRequestFullScreen:"

-- | @Selector@ for @querySelector:@
querySelectorSelector :: Selector
querySelectorSelector = mkSelector "querySelector:"

-- | @Selector@ for @querySelectorAll:@
querySelectorAllSelector :: Selector
querySelectorAllSelector = mkSelector "querySelectorAll:"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @setAttribute::@
setAttributeSelector :: Selector
setAttributeSelector = mkSelector "setAttribute::"

-- | @Selector@ for @getAttributeNS::@
getAttributeNSSelector :: Selector
getAttributeNSSelector = mkSelector "getAttributeNS::"

-- | @Selector@ for @setAttributeNS:::@
setAttributeNSSelector :: Selector
setAttributeNSSelector = mkSelector "setAttributeNS:::"

-- | @Selector@ for @removeAttributeNS::@
removeAttributeNSSelector :: Selector
removeAttributeNSSelector = mkSelector "removeAttributeNS::"

-- | @Selector@ for @getElementsByTagNameNS::@
getElementsByTagNameNSSelector :: Selector
getElementsByTagNameNSSelector = mkSelector "getElementsByTagNameNS::"

-- | @Selector@ for @getAttributeNodeNS::@
getAttributeNodeNSSelector :: Selector
getAttributeNodeNSSelector = mkSelector "getAttributeNodeNS::"

-- | @Selector@ for @hasAttributeNS::@
hasAttributeNSSelector :: Selector
hasAttributeNSSelector = mkSelector "hasAttributeNS::"

-- | @Selector@ for @scrollByLines:@
scrollByLinesSelector :: Selector
scrollByLinesSelector = mkSelector "scrollByLines:"

-- | @Selector@ for @scrollByPages:@
scrollByPagesSelector :: Selector
scrollByPagesSelector = mkSelector "scrollByPages:"

-- | @Selector@ for @tagName@
tagNameSelector :: Selector
tagNameSelector = mkSelector "tagName"

-- | @Selector@ for @style@
styleSelector :: Selector
styleSelector = mkSelector "style"

-- | @Selector@ for @offsetLeft@
offsetLeftSelector :: Selector
offsetLeftSelector = mkSelector "offsetLeft"

-- | @Selector@ for @offsetTop@
offsetTopSelector :: Selector
offsetTopSelector = mkSelector "offsetTop"

-- | @Selector@ for @offsetWidth@
offsetWidthSelector :: Selector
offsetWidthSelector = mkSelector "offsetWidth"

-- | @Selector@ for @offsetHeight@
offsetHeightSelector :: Selector
offsetHeightSelector = mkSelector "offsetHeight"

-- | @Selector@ for @clientLeft@
clientLeftSelector :: Selector
clientLeftSelector = mkSelector "clientLeft"

-- | @Selector@ for @clientTop@
clientTopSelector :: Selector
clientTopSelector = mkSelector "clientTop"

-- | @Selector@ for @clientWidth@
clientWidthSelector :: Selector
clientWidthSelector = mkSelector "clientWidth"

-- | @Selector@ for @clientHeight@
clientHeightSelector :: Selector
clientHeightSelector = mkSelector "clientHeight"

-- | @Selector@ for @scrollLeft@
scrollLeftSelector :: Selector
scrollLeftSelector = mkSelector "scrollLeft"

-- | @Selector@ for @setScrollLeft:@
setScrollLeftSelector :: Selector
setScrollLeftSelector = mkSelector "setScrollLeft:"

-- | @Selector@ for @scrollTop@
scrollTopSelector :: Selector
scrollTopSelector = mkSelector "scrollTop"

-- | @Selector@ for @setScrollTop:@
setScrollTopSelector :: Selector
setScrollTopSelector = mkSelector "setScrollTop:"

-- | @Selector@ for @scrollWidth@
scrollWidthSelector :: Selector
scrollWidthSelector = mkSelector "scrollWidth"

-- | @Selector@ for @scrollHeight@
scrollHeightSelector :: Selector
scrollHeightSelector = mkSelector "scrollHeight"

-- | @Selector@ for @offsetParent@
offsetParentSelector :: Selector
offsetParentSelector = mkSelector "offsetParent"

-- | @Selector@ for @innerHTML@
innerHTMLSelector :: Selector
innerHTMLSelector = mkSelector "innerHTML"

-- | @Selector@ for @setInnerHTML:@
setInnerHTMLSelector :: Selector
setInnerHTMLSelector = mkSelector "setInnerHTML:"

-- | @Selector@ for @outerHTML@
outerHTMLSelector :: Selector
outerHTMLSelector = mkSelector "outerHTML"

-- | @Selector@ for @setOuterHTML:@
setOuterHTMLSelector :: Selector
setOuterHTMLSelector = mkSelector "setOuterHTML:"

-- | @Selector@ for @className@
classNameSelector :: Selector
classNameSelector = mkSelector "className"

-- | @Selector@ for @setClassName:@
setClassNameSelector :: Selector
setClassNameSelector = mkSelector "setClassName:"

-- | @Selector@ for @innerText@
innerTextSelector :: Selector
innerTextSelector = mkSelector "innerText"

-- | @Selector@ for @previousElementSibling@
previousElementSiblingSelector :: Selector
previousElementSiblingSelector = mkSelector "previousElementSibling"

-- | @Selector@ for @nextElementSibling@
nextElementSiblingSelector :: Selector
nextElementSiblingSelector = mkSelector "nextElementSibling"

-- | @Selector@ for @firstElementChild@
firstElementChildSelector :: Selector
firstElementChildSelector = mkSelector "firstElementChild"

-- | @Selector@ for @lastElementChild@
lastElementChildSelector :: Selector
lastElementChildSelector = mkSelector "lastElementChild"

-- | @Selector@ for @childElementCount@
childElementCountSelector :: Selector
childElementCountSelector = mkSelector "childElementCount"

