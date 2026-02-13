{-# LANGUAGE DataKinds #-}
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
  , blurSelector
  , childElementCountSelector
  , classNameSelector
  , clientHeightSelector
  , clientLeftSelector
  , clientTopSelector
  , clientWidthSelector
  , firstElementChildSelector
  , focusSelector
  , getAttributeNSSelector
  , getAttributeNS_localNameSelector
  , getAttributeNodeNSSelector
  , getAttributeNodeNS_localNameSelector
  , getAttributeNodeSelector
  , getAttributeSelector
  , getElementsByClassNameSelector
  , getElementsByTagNameNSSelector
  , getElementsByTagNameNS_localNameSelector
  , getElementsByTagNameSelector
  , hasAttributeNSSelector
  , hasAttributeNS_localNameSelector
  , hasAttributeSelector
  , imageSelector
  , innerHTMLSelector
  , innerTextSelector
  , lastElementChildSelector
  , nextElementSiblingSelector
  , offsetHeightSelector
  , offsetLeftSelector
  , offsetParentSelector
  , offsetTopSelector
  , offsetWidthSelector
  , outerHTMLSelector
  , previousElementSiblingSelector
  , querySelectorAllSelector
  , querySelectorSelector
  , removeAttributeNSSelector
  , removeAttributeNS_localNameSelector
  , removeAttributeNodeSelector
  , removeAttributeSelector
  , scrollByLinesSelector
  , scrollByPagesSelector
  , scrollHeightSelector
  , scrollIntoViewIfNeededSelector
  , scrollIntoViewSelector
  , scrollLeftSelector
  , scrollTopSelector
  , scrollWidthSelector
  , setAttributeNSSelector
  , setAttributeNS_qualifiedName_valueSelector
  , setAttributeNodeNSSelector
  , setAttributeNodeSelector
  , setAttributeSelector
  , setAttribute_valueSelector
  , setClassNameSelector
  , setInnerHTMLSelector
  , setOuterHTMLSelector
  , setScrollLeftSelector
  , setScrollTopSelector
  , styleSelector
  , tagNameSelector
  , webkitRequestFullScreenSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- getAttribute:@
getAttribute :: (IsDOMElement domElement, IsNSString name) => domElement -> name -> IO (Id NSString)
getAttribute domElement name =
  sendMessage domElement getAttributeSelector (toNSString name)

-- | @- setAttribute:value:@
setAttribute_value :: (IsDOMElement domElement, IsNSString name, IsNSString value) => domElement -> name -> value -> IO ()
setAttribute_value domElement name value =
  sendMessage domElement setAttribute_valueSelector (toNSString name) (toNSString value)

-- | @- removeAttribute:@
removeAttribute :: (IsDOMElement domElement, IsNSString name) => domElement -> name -> IO ()
removeAttribute domElement name =
  sendMessage domElement removeAttributeSelector (toNSString name)

-- | @- getAttributeNode:@
getAttributeNode :: (IsDOMElement domElement, IsNSString name) => domElement -> name -> IO (Id DOMAttr)
getAttributeNode domElement name =
  sendMessage domElement getAttributeNodeSelector (toNSString name)

-- | @- setAttributeNode:@
setAttributeNode :: (IsDOMElement domElement, IsDOMAttr newAttr) => domElement -> newAttr -> IO (Id DOMAttr)
setAttributeNode domElement newAttr =
  sendMessage domElement setAttributeNodeSelector (toDOMAttr newAttr)

-- | @- removeAttributeNode:@
removeAttributeNode :: (IsDOMElement domElement, IsDOMAttr oldAttr) => domElement -> oldAttr -> IO (Id DOMAttr)
removeAttributeNode domElement oldAttr =
  sendMessage domElement removeAttributeNodeSelector (toDOMAttr oldAttr)

-- | @- getElementsByTagName:@
getElementsByTagName :: (IsDOMElement domElement, IsNSString name) => domElement -> name -> IO (Id DOMNodeList)
getElementsByTagName domElement name =
  sendMessage domElement getElementsByTagNameSelector (toNSString name)

-- | @- getAttributeNS:localName:@
getAttributeNS_localName :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString localName) => domElement -> namespaceURI -> localName -> IO (Id NSString)
getAttributeNS_localName domElement namespaceURI localName =
  sendMessage domElement getAttributeNS_localNameSelector (toNSString namespaceURI) (toNSString localName)

-- | @- setAttributeNS:qualifiedName:value:@
setAttributeNS_qualifiedName_value :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString qualifiedName, IsNSString value) => domElement -> namespaceURI -> qualifiedName -> value -> IO ()
setAttributeNS_qualifiedName_value domElement namespaceURI qualifiedName value =
  sendMessage domElement setAttributeNS_qualifiedName_valueSelector (toNSString namespaceURI) (toNSString qualifiedName) (toNSString value)

-- | @- removeAttributeNS:localName:@
removeAttributeNS_localName :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString localName) => domElement -> namespaceURI -> localName -> IO ()
removeAttributeNS_localName domElement namespaceURI localName =
  sendMessage domElement removeAttributeNS_localNameSelector (toNSString namespaceURI) (toNSString localName)

-- | @- getElementsByTagNameNS:localName:@
getElementsByTagNameNS_localName :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString localName) => domElement -> namespaceURI -> localName -> IO (Id DOMNodeList)
getElementsByTagNameNS_localName domElement namespaceURI localName =
  sendMessage domElement getElementsByTagNameNS_localNameSelector (toNSString namespaceURI) (toNSString localName)

-- | @- getAttributeNodeNS:localName:@
getAttributeNodeNS_localName :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString localName) => domElement -> namespaceURI -> localName -> IO (Id DOMAttr)
getAttributeNodeNS_localName domElement namespaceURI localName =
  sendMessage domElement getAttributeNodeNS_localNameSelector (toNSString namespaceURI) (toNSString localName)

-- | @- setAttributeNodeNS:@
setAttributeNodeNS :: (IsDOMElement domElement, IsDOMAttr newAttr) => domElement -> newAttr -> IO (Id DOMAttr)
setAttributeNodeNS domElement newAttr =
  sendMessage domElement setAttributeNodeNSSelector (toDOMAttr newAttr)

-- | @- hasAttribute:@
hasAttribute :: (IsDOMElement domElement, IsNSString name) => domElement -> name -> IO Bool
hasAttribute domElement name =
  sendMessage domElement hasAttributeSelector (toNSString name)

-- | @- hasAttributeNS:localName:@
hasAttributeNS_localName :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString localName) => domElement -> namespaceURI -> localName -> IO Bool
hasAttributeNS_localName domElement namespaceURI localName =
  sendMessage domElement hasAttributeNS_localNameSelector (toNSString namespaceURI) (toNSString localName)

-- | @- focus@
focus :: IsDOMElement domElement => domElement -> IO ()
focus domElement =
  sendMessage domElement focusSelector

-- | @- blur@
blur :: IsDOMElement domElement => domElement -> IO ()
blur domElement =
  sendMessage domElement blurSelector

-- | @- scrollIntoView:@
scrollIntoView :: IsDOMElement domElement => domElement -> Bool -> IO ()
scrollIntoView domElement alignWithTop =
  sendMessage domElement scrollIntoViewSelector alignWithTop

-- | @- scrollIntoViewIfNeeded:@
scrollIntoViewIfNeeded :: IsDOMElement domElement => domElement -> Bool -> IO ()
scrollIntoViewIfNeeded domElement centerIfNeeded =
  sendMessage domElement scrollIntoViewIfNeededSelector centerIfNeeded

-- | @- getElementsByClassName:@
getElementsByClassName :: (IsDOMElement domElement, IsNSString name) => domElement -> name -> IO (Id DOMNodeList)
getElementsByClassName domElement name =
  sendMessage domElement getElementsByClassNameSelector (toNSString name)

-- | @- webkitRequestFullScreen:@
webkitRequestFullScreen :: IsDOMElement domElement => domElement -> CUShort -> IO ()
webkitRequestFullScreen domElement flags =
  sendMessage domElement webkitRequestFullScreenSelector flags

-- | @- querySelector:@
querySelector :: (IsDOMElement domElement, IsNSString selectors) => domElement -> selectors -> IO (Id DOMElement)
querySelector domElement selectors =
  sendMessage domElement querySelectorSelector (toNSString selectors)

-- | @- querySelectorAll:@
querySelectorAll :: (IsDOMElement domElement, IsNSString selectors) => domElement -> selectors -> IO (Id DOMNodeList)
querySelectorAll domElement selectors =
  sendMessage domElement querySelectorAllSelector (toNSString selectors)

-- | @- image@
image :: IsDOMElement domElement => domElement -> IO (Id NSImage)
image domElement =
  sendMessage domElement imageSelector

-- | @- setAttribute::@
setAttribute :: (IsDOMElement domElement, IsNSString name, IsNSString value) => domElement -> name -> value -> IO ()
setAttribute domElement name value =
  sendMessage domElement setAttributeSelector (toNSString name) (toNSString value)

-- | @- getAttributeNS::@
getAttributeNS :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString localName) => domElement -> namespaceURI -> localName -> IO (Id NSString)
getAttributeNS domElement namespaceURI localName =
  sendMessage domElement getAttributeNSSelector (toNSString namespaceURI) (toNSString localName)

-- | @- setAttributeNS:::@
setAttributeNS :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString qualifiedName, IsNSString value) => domElement -> namespaceURI -> qualifiedName -> value -> IO ()
setAttributeNS domElement namespaceURI qualifiedName value =
  sendMessage domElement setAttributeNSSelector (toNSString namespaceURI) (toNSString qualifiedName) (toNSString value)

-- | @- removeAttributeNS::@
removeAttributeNS :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString localName) => domElement -> namespaceURI -> localName -> IO ()
removeAttributeNS domElement namespaceURI localName =
  sendMessage domElement removeAttributeNSSelector (toNSString namespaceURI) (toNSString localName)

-- | @- getElementsByTagNameNS::@
getElementsByTagNameNS :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString localName) => domElement -> namespaceURI -> localName -> IO (Id DOMNodeList)
getElementsByTagNameNS domElement namespaceURI localName =
  sendMessage domElement getElementsByTagNameNSSelector (toNSString namespaceURI) (toNSString localName)

-- | @- getAttributeNodeNS::@
getAttributeNodeNS :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString localName) => domElement -> namespaceURI -> localName -> IO (Id DOMAttr)
getAttributeNodeNS domElement namespaceURI localName =
  sendMessage domElement getAttributeNodeNSSelector (toNSString namespaceURI) (toNSString localName)

-- | @- hasAttributeNS::@
hasAttributeNS :: (IsDOMElement domElement, IsNSString namespaceURI, IsNSString localName) => domElement -> namespaceURI -> localName -> IO Bool
hasAttributeNS domElement namespaceURI localName =
  sendMessage domElement hasAttributeNSSelector (toNSString namespaceURI) (toNSString localName)

-- | @- scrollByLines:@
scrollByLines :: IsDOMElement domElement => domElement -> CInt -> IO ()
scrollByLines domElement lines_ =
  sendMessage domElement scrollByLinesSelector lines_

-- | @- scrollByPages:@
scrollByPages :: IsDOMElement domElement => domElement -> CInt -> IO ()
scrollByPages domElement pages =
  sendMessage domElement scrollByPagesSelector pages

-- | @- tagName@
tagName :: IsDOMElement domElement => domElement -> IO (Id NSString)
tagName domElement =
  sendMessage domElement tagNameSelector

-- | @- style@
style :: IsDOMElement domElement => domElement -> IO (Id DOMCSSStyleDeclaration)
style domElement =
  sendMessage domElement styleSelector

-- | @- offsetLeft@
offsetLeft :: IsDOMElement domElement => domElement -> IO CInt
offsetLeft domElement =
  sendMessage domElement offsetLeftSelector

-- | @- offsetTop@
offsetTop :: IsDOMElement domElement => domElement -> IO CInt
offsetTop domElement =
  sendMessage domElement offsetTopSelector

-- | @- offsetWidth@
offsetWidth :: IsDOMElement domElement => domElement -> IO CInt
offsetWidth domElement =
  sendMessage domElement offsetWidthSelector

-- | @- offsetHeight@
offsetHeight :: IsDOMElement domElement => domElement -> IO CInt
offsetHeight domElement =
  sendMessage domElement offsetHeightSelector

-- | @- clientLeft@
clientLeft :: IsDOMElement domElement => domElement -> IO CInt
clientLeft domElement =
  sendMessage domElement clientLeftSelector

-- | @- clientTop@
clientTop :: IsDOMElement domElement => domElement -> IO CInt
clientTop domElement =
  sendMessage domElement clientTopSelector

-- | @- clientWidth@
clientWidth :: IsDOMElement domElement => domElement -> IO CInt
clientWidth domElement =
  sendMessage domElement clientWidthSelector

-- | @- clientHeight@
clientHeight :: IsDOMElement domElement => domElement -> IO CInt
clientHeight domElement =
  sendMessage domElement clientHeightSelector

-- | @- scrollLeft@
scrollLeft :: IsDOMElement domElement => domElement -> IO CInt
scrollLeft domElement =
  sendMessage domElement scrollLeftSelector

-- | @- setScrollLeft:@
setScrollLeft :: IsDOMElement domElement => domElement -> CInt -> IO ()
setScrollLeft domElement value =
  sendMessage domElement setScrollLeftSelector value

-- | @- scrollTop@
scrollTop :: IsDOMElement domElement => domElement -> IO CInt
scrollTop domElement =
  sendMessage domElement scrollTopSelector

-- | @- setScrollTop:@
setScrollTop :: IsDOMElement domElement => domElement -> CInt -> IO ()
setScrollTop domElement value =
  sendMessage domElement setScrollTopSelector value

-- | @- scrollWidth@
scrollWidth :: IsDOMElement domElement => domElement -> IO CInt
scrollWidth domElement =
  sendMessage domElement scrollWidthSelector

-- | @- scrollHeight@
scrollHeight :: IsDOMElement domElement => domElement -> IO CInt
scrollHeight domElement =
  sendMessage domElement scrollHeightSelector

-- | @- offsetParent@
offsetParent :: IsDOMElement domElement => domElement -> IO (Id DOMElement)
offsetParent domElement =
  sendMessage domElement offsetParentSelector

-- | @- innerHTML@
innerHTML :: IsDOMElement domElement => domElement -> IO (Id NSString)
innerHTML domElement =
  sendMessage domElement innerHTMLSelector

-- | @- setInnerHTML:@
setInnerHTML :: (IsDOMElement domElement, IsNSString value) => domElement -> value -> IO ()
setInnerHTML domElement value =
  sendMessage domElement setInnerHTMLSelector (toNSString value)

-- | @- outerHTML@
outerHTML :: IsDOMElement domElement => domElement -> IO (Id NSString)
outerHTML domElement =
  sendMessage domElement outerHTMLSelector

-- | @- setOuterHTML:@
setOuterHTML :: (IsDOMElement domElement, IsNSString value) => domElement -> value -> IO ()
setOuterHTML domElement value =
  sendMessage domElement setOuterHTMLSelector (toNSString value)

-- | @- className@
className :: IsDOMElement domElement => domElement -> IO (Id NSString)
className domElement =
  sendMessage domElement classNameSelector

-- | @- setClassName:@
setClassName :: (IsDOMElement domElement, IsNSString value) => domElement -> value -> IO ()
setClassName domElement value =
  sendMessage domElement setClassNameSelector (toNSString value)

-- | @- innerText@
innerText :: IsDOMElement domElement => domElement -> IO (Id NSString)
innerText domElement =
  sendMessage domElement innerTextSelector

-- | @- previousElementSibling@
previousElementSibling :: IsDOMElement domElement => domElement -> IO (Id DOMElement)
previousElementSibling domElement =
  sendMessage domElement previousElementSiblingSelector

-- | @- nextElementSibling@
nextElementSibling :: IsDOMElement domElement => domElement -> IO (Id DOMElement)
nextElementSibling domElement =
  sendMessage domElement nextElementSiblingSelector

-- | @- firstElementChild@
firstElementChild :: IsDOMElement domElement => domElement -> IO (Id DOMElement)
firstElementChild domElement =
  sendMessage domElement firstElementChildSelector

-- | @- lastElementChild@
lastElementChild :: IsDOMElement domElement => domElement -> IO (Id DOMElement)
lastElementChild domElement =
  sendMessage domElement lastElementChildSelector

-- | @- childElementCount@
childElementCount :: IsDOMElement domElement => domElement -> IO CUInt
childElementCount domElement =
  sendMessage domElement childElementCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @getAttribute:@
getAttributeSelector :: Selector '[Id NSString] (Id NSString)
getAttributeSelector = mkSelector "getAttribute:"

-- | @Selector@ for @setAttribute:value:@
setAttribute_valueSelector :: Selector '[Id NSString, Id NSString] ()
setAttribute_valueSelector = mkSelector "setAttribute:value:"

-- | @Selector@ for @removeAttribute:@
removeAttributeSelector :: Selector '[Id NSString] ()
removeAttributeSelector = mkSelector "removeAttribute:"

-- | @Selector@ for @getAttributeNode:@
getAttributeNodeSelector :: Selector '[Id NSString] (Id DOMAttr)
getAttributeNodeSelector = mkSelector "getAttributeNode:"

-- | @Selector@ for @setAttributeNode:@
setAttributeNodeSelector :: Selector '[Id DOMAttr] (Id DOMAttr)
setAttributeNodeSelector = mkSelector "setAttributeNode:"

-- | @Selector@ for @removeAttributeNode:@
removeAttributeNodeSelector :: Selector '[Id DOMAttr] (Id DOMAttr)
removeAttributeNodeSelector = mkSelector "removeAttributeNode:"

-- | @Selector@ for @getElementsByTagName:@
getElementsByTagNameSelector :: Selector '[Id NSString] (Id DOMNodeList)
getElementsByTagNameSelector = mkSelector "getElementsByTagName:"

-- | @Selector@ for @getAttributeNS:localName:@
getAttributeNS_localNameSelector :: Selector '[Id NSString, Id NSString] (Id NSString)
getAttributeNS_localNameSelector = mkSelector "getAttributeNS:localName:"

-- | @Selector@ for @setAttributeNS:qualifiedName:value:@
setAttributeNS_qualifiedName_valueSelector :: Selector '[Id NSString, Id NSString, Id NSString] ()
setAttributeNS_qualifiedName_valueSelector = mkSelector "setAttributeNS:qualifiedName:value:"

-- | @Selector@ for @removeAttributeNS:localName:@
removeAttributeNS_localNameSelector :: Selector '[Id NSString, Id NSString] ()
removeAttributeNS_localNameSelector = mkSelector "removeAttributeNS:localName:"

-- | @Selector@ for @getElementsByTagNameNS:localName:@
getElementsByTagNameNS_localNameSelector :: Selector '[Id NSString, Id NSString] (Id DOMNodeList)
getElementsByTagNameNS_localNameSelector = mkSelector "getElementsByTagNameNS:localName:"

-- | @Selector@ for @getAttributeNodeNS:localName:@
getAttributeNodeNS_localNameSelector :: Selector '[Id NSString, Id NSString] (Id DOMAttr)
getAttributeNodeNS_localNameSelector = mkSelector "getAttributeNodeNS:localName:"

-- | @Selector@ for @setAttributeNodeNS:@
setAttributeNodeNSSelector :: Selector '[Id DOMAttr] (Id DOMAttr)
setAttributeNodeNSSelector = mkSelector "setAttributeNodeNS:"

-- | @Selector@ for @hasAttribute:@
hasAttributeSelector :: Selector '[Id NSString] Bool
hasAttributeSelector = mkSelector "hasAttribute:"

-- | @Selector@ for @hasAttributeNS:localName:@
hasAttributeNS_localNameSelector :: Selector '[Id NSString, Id NSString] Bool
hasAttributeNS_localNameSelector = mkSelector "hasAttributeNS:localName:"

-- | @Selector@ for @focus@
focusSelector :: Selector '[] ()
focusSelector = mkSelector "focus"

-- | @Selector@ for @blur@
blurSelector :: Selector '[] ()
blurSelector = mkSelector "blur"

-- | @Selector@ for @scrollIntoView:@
scrollIntoViewSelector :: Selector '[Bool] ()
scrollIntoViewSelector = mkSelector "scrollIntoView:"

-- | @Selector@ for @scrollIntoViewIfNeeded:@
scrollIntoViewIfNeededSelector :: Selector '[Bool] ()
scrollIntoViewIfNeededSelector = mkSelector "scrollIntoViewIfNeeded:"

-- | @Selector@ for @getElementsByClassName:@
getElementsByClassNameSelector :: Selector '[Id NSString] (Id DOMNodeList)
getElementsByClassNameSelector = mkSelector "getElementsByClassName:"

-- | @Selector@ for @webkitRequestFullScreen:@
webkitRequestFullScreenSelector :: Selector '[CUShort] ()
webkitRequestFullScreenSelector = mkSelector "webkitRequestFullScreen:"

-- | @Selector@ for @querySelector:@
querySelectorSelector :: Selector '[Id NSString] (Id DOMElement)
querySelectorSelector = mkSelector "querySelector:"

-- | @Selector@ for @querySelectorAll:@
querySelectorAllSelector :: Selector '[Id NSString] (Id DOMNodeList)
querySelectorAllSelector = mkSelector "querySelectorAll:"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @setAttribute::@
setAttributeSelector :: Selector '[Id NSString, Id NSString] ()
setAttributeSelector = mkSelector "setAttribute::"

-- | @Selector@ for @getAttributeNS::@
getAttributeNSSelector :: Selector '[Id NSString, Id NSString] (Id NSString)
getAttributeNSSelector = mkSelector "getAttributeNS::"

-- | @Selector@ for @setAttributeNS:::@
setAttributeNSSelector :: Selector '[Id NSString, Id NSString, Id NSString] ()
setAttributeNSSelector = mkSelector "setAttributeNS:::"

-- | @Selector@ for @removeAttributeNS::@
removeAttributeNSSelector :: Selector '[Id NSString, Id NSString] ()
removeAttributeNSSelector = mkSelector "removeAttributeNS::"

-- | @Selector@ for @getElementsByTagNameNS::@
getElementsByTagNameNSSelector :: Selector '[Id NSString, Id NSString] (Id DOMNodeList)
getElementsByTagNameNSSelector = mkSelector "getElementsByTagNameNS::"

-- | @Selector@ for @getAttributeNodeNS::@
getAttributeNodeNSSelector :: Selector '[Id NSString, Id NSString] (Id DOMAttr)
getAttributeNodeNSSelector = mkSelector "getAttributeNodeNS::"

-- | @Selector@ for @hasAttributeNS::@
hasAttributeNSSelector :: Selector '[Id NSString, Id NSString] Bool
hasAttributeNSSelector = mkSelector "hasAttributeNS::"

-- | @Selector@ for @scrollByLines:@
scrollByLinesSelector :: Selector '[CInt] ()
scrollByLinesSelector = mkSelector "scrollByLines:"

-- | @Selector@ for @scrollByPages:@
scrollByPagesSelector :: Selector '[CInt] ()
scrollByPagesSelector = mkSelector "scrollByPages:"

-- | @Selector@ for @tagName@
tagNameSelector :: Selector '[] (Id NSString)
tagNameSelector = mkSelector "tagName"

-- | @Selector@ for @style@
styleSelector :: Selector '[] (Id DOMCSSStyleDeclaration)
styleSelector = mkSelector "style"

-- | @Selector@ for @offsetLeft@
offsetLeftSelector :: Selector '[] CInt
offsetLeftSelector = mkSelector "offsetLeft"

-- | @Selector@ for @offsetTop@
offsetTopSelector :: Selector '[] CInt
offsetTopSelector = mkSelector "offsetTop"

-- | @Selector@ for @offsetWidth@
offsetWidthSelector :: Selector '[] CInt
offsetWidthSelector = mkSelector "offsetWidth"

-- | @Selector@ for @offsetHeight@
offsetHeightSelector :: Selector '[] CInt
offsetHeightSelector = mkSelector "offsetHeight"

-- | @Selector@ for @clientLeft@
clientLeftSelector :: Selector '[] CInt
clientLeftSelector = mkSelector "clientLeft"

-- | @Selector@ for @clientTop@
clientTopSelector :: Selector '[] CInt
clientTopSelector = mkSelector "clientTop"

-- | @Selector@ for @clientWidth@
clientWidthSelector :: Selector '[] CInt
clientWidthSelector = mkSelector "clientWidth"

-- | @Selector@ for @clientHeight@
clientHeightSelector :: Selector '[] CInt
clientHeightSelector = mkSelector "clientHeight"

-- | @Selector@ for @scrollLeft@
scrollLeftSelector :: Selector '[] CInt
scrollLeftSelector = mkSelector "scrollLeft"

-- | @Selector@ for @setScrollLeft:@
setScrollLeftSelector :: Selector '[CInt] ()
setScrollLeftSelector = mkSelector "setScrollLeft:"

-- | @Selector@ for @scrollTop@
scrollTopSelector :: Selector '[] CInt
scrollTopSelector = mkSelector "scrollTop"

-- | @Selector@ for @setScrollTop:@
setScrollTopSelector :: Selector '[CInt] ()
setScrollTopSelector = mkSelector "setScrollTop:"

-- | @Selector@ for @scrollWidth@
scrollWidthSelector :: Selector '[] CInt
scrollWidthSelector = mkSelector "scrollWidth"

-- | @Selector@ for @scrollHeight@
scrollHeightSelector :: Selector '[] CInt
scrollHeightSelector = mkSelector "scrollHeight"

-- | @Selector@ for @offsetParent@
offsetParentSelector :: Selector '[] (Id DOMElement)
offsetParentSelector = mkSelector "offsetParent"

-- | @Selector@ for @innerHTML@
innerHTMLSelector :: Selector '[] (Id NSString)
innerHTMLSelector = mkSelector "innerHTML"

-- | @Selector@ for @setInnerHTML:@
setInnerHTMLSelector :: Selector '[Id NSString] ()
setInnerHTMLSelector = mkSelector "setInnerHTML:"

-- | @Selector@ for @outerHTML@
outerHTMLSelector :: Selector '[] (Id NSString)
outerHTMLSelector = mkSelector "outerHTML"

-- | @Selector@ for @setOuterHTML:@
setOuterHTMLSelector :: Selector '[Id NSString] ()
setOuterHTMLSelector = mkSelector "setOuterHTML:"

-- | @Selector@ for @className@
classNameSelector :: Selector '[] (Id NSString)
classNameSelector = mkSelector "className"

-- | @Selector@ for @setClassName:@
setClassNameSelector :: Selector '[Id NSString] ()
setClassNameSelector = mkSelector "setClassName:"

-- | @Selector@ for @innerText@
innerTextSelector :: Selector '[] (Id NSString)
innerTextSelector = mkSelector "innerText"

-- | @Selector@ for @previousElementSibling@
previousElementSiblingSelector :: Selector '[] (Id DOMElement)
previousElementSiblingSelector = mkSelector "previousElementSibling"

-- | @Selector@ for @nextElementSibling@
nextElementSiblingSelector :: Selector '[] (Id DOMElement)
nextElementSiblingSelector = mkSelector "nextElementSibling"

-- | @Selector@ for @firstElementChild@
firstElementChildSelector :: Selector '[] (Id DOMElement)
firstElementChildSelector = mkSelector "firstElementChild"

-- | @Selector@ for @lastElementChild@
lastElementChildSelector :: Selector '[] (Id DOMElement)
lastElementChildSelector = mkSelector "lastElementChild"

-- | @Selector@ for @childElementCount@
childElementCountSelector :: Selector '[] CUInt
childElementCountSelector = mkSelector "childElementCount"

