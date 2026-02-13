{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLElement@.
module ObjC.WebKit.DOMHTMLElement
  ( DOMHTMLElement
  , IsDOMHTMLElement(..)
  , click
  , title
  , setTitle
  , lang
  , setLang
  , dir
  , setDir
  , tabIndex
  , setTabIndex
  , accessKey
  , setAccessKey
  , innerText
  , setInnerText
  , outerText
  , setOuterText
  , contentEditable
  , setContentEditable
  , isContentEditable
  , idName
  , setIdName
  , children
  , titleDisplayString
  , accessKeySelector
  , childrenSelector
  , clickSelector
  , contentEditableSelector
  , dirSelector
  , idNameSelector
  , innerTextSelector
  , isContentEditableSelector
  , langSelector
  , outerTextSelector
  , setAccessKeySelector
  , setContentEditableSelector
  , setDirSelector
  , setIdNameSelector
  , setInnerTextSelector
  , setLangSelector
  , setOuterTextSelector
  , setTabIndexSelector
  , setTitleSelector
  , tabIndexSelector
  , titleDisplayStringSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- click@
click :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO ()
click domhtmlElement =
  sendMessage domhtmlElement clickSelector

-- | @- title@
title :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO (Id NSString)
title domhtmlElement =
  sendMessage domhtmlElement titleSelector

-- | @- setTitle:@
setTitle :: (IsDOMHTMLElement domhtmlElement, IsNSString value) => domhtmlElement -> value -> IO ()
setTitle domhtmlElement value =
  sendMessage domhtmlElement setTitleSelector (toNSString value)

-- | @- lang@
lang :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO (Id NSString)
lang domhtmlElement =
  sendMessage domhtmlElement langSelector

-- | @- setLang:@
setLang :: (IsDOMHTMLElement domhtmlElement, IsNSString value) => domhtmlElement -> value -> IO ()
setLang domhtmlElement value =
  sendMessage domhtmlElement setLangSelector (toNSString value)

-- | @- dir@
dir :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO (Id NSString)
dir domhtmlElement =
  sendMessage domhtmlElement dirSelector

-- | @- setDir:@
setDir :: (IsDOMHTMLElement domhtmlElement, IsNSString value) => domhtmlElement -> value -> IO ()
setDir domhtmlElement value =
  sendMessage domhtmlElement setDirSelector (toNSString value)

-- | @- tabIndex@
tabIndex :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO CInt
tabIndex domhtmlElement =
  sendMessage domhtmlElement tabIndexSelector

-- | @- setTabIndex:@
setTabIndex :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> CInt -> IO ()
setTabIndex domhtmlElement value =
  sendMessage domhtmlElement setTabIndexSelector value

-- | @- accessKey@
accessKey :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO (Id NSString)
accessKey domhtmlElement =
  sendMessage domhtmlElement accessKeySelector

-- | @- setAccessKey:@
setAccessKey :: (IsDOMHTMLElement domhtmlElement, IsNSString value) => domhtmlElement -> value -> IO ()
setAccessKey domhtmlElement value =
  sendMessage domhtmlElement setAccessKeySelector (toNSString value)

-- | @- innerText@
innerText :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO (Id NSString)
innerText domhtmlElement =
  sendMessage domhtmlElement innerTextSelector

-- | @- setInnerText:@
setInnerText :: (IsDOMHTMLElement domhtmlElement, IsNSString value) => domhtmlElement -> value -> IO ()
setInnerText domhtmlElement value =
  sendMessage domhtmlElement setInnerTextSelector (toNSString value)

-- | @- outerText@
outerText :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO (Id NSString)
outerText domhtmlElement =
  sendMessage domhtmlElement outerTextSelector

-- | @- setOuterText:@
setOuterText :: (IsDOMHTMLElement domhtmlElement, IsNSString value) => domhtmlElement -> value -> IO ()
setOuterText domhtmlElement value =
  sendMessage domhtmlElement setOuterTextSelector (toNSString value)

-- | @- contentEditable@
contentEditable :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO (Id NSString)
contentEditable domhtmlElement =
  sendMessage domhtmlElement contentEditableSelector

-- | @- setContentEditable:@
setContentEditable :: (IsDOMHTMLElement domhtmlElement, IsNSString value) => domhtmlElement -> value -> IO ()
setContentEditable domhtmlElement value =
  sendMessage domhtmlElement setContentEditableSelector (toNSString value)

-- | @- isContentEditable@
isContentEditable :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO Bool
isContentEditable domhtmlElement =
  sendMessage domhtmlElement isContentEditableSelector

-- | @- idName@
idName :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO (Id NSString)
idName domhtmlElement =
  sendMessage domhtmlElement idNameSelector

-- | @- setIdName:@
setIdName :: (IsDOMHTMLElement domhtmlElement, IsNSString value) => domhtmlElement -> value -> IO ()
setIdName domhtmlElement value =
  sendMessage domhtmlElement setIdNameSelector (toNSString value)

-- | @- children@
children :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO (Id DOMHTMLCollection)
children domhtmlElement =
  sendMessage domhtmlElement childrenSelector

-- | @- titleDisplayString@
titleDisplayString :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO (Id NSString)
titleDisplayString domhtmlElement =
  sendMessage domhtmlElement titleDisplayStringSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @click@
clickSelector :: Selector '[] ()
clickSelector = mkSelector "click"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @lang@
langSelector :: Selector '[] (Id NSString)
langSelector = mkSelector "lang"

-- | @Selector@ for @setLang:@
setLangSelector :: Selector '[Id NSString] ()
setLangSelector = mkSelector "setLang:"

-- | @Selector@ for @dir@
dirSelector :: Selector '[] (Id NSString)
dirSelector = mkSelector "dir"

-- | @Selector@ for @setDir:@
setDirSelector :: Selector '[Id NSString] ()
setDirSelector = mkSelector "setDir:"

-- | @Selector@ for @tabIndex@
tabIndexSelector :: Selector '[] CInt
tabIndexSelector = mkSelector "tabIndex"

-- | @Selector@ for @setTabIndex:@
setTabIndexSelector :: Selector '[CInt] ()
setTabIndexSelector = mkSelector "setTabIndex:"

-- | @Selector@ for @accessKey@
accessKeySelector :: Selector '[] (Id NSString)
accessKeySelector = mkSelector "accessKey"

-- | @Selector@ for @setAccessKey:@
setAccessKeySelector :: Selector '[Id NSString] ()
setAccessKeySelector = mkSelector "setAccessKey:"

-- | @Selector@ for @innerText@
innerTextSelector :: Selector '[] (Id NSString)
innerTextSelector = mkSelector "innerText"

-- | @Selector@ for @setInnerText:@
setInnerTextSelector :: Selector '[Id NSString] ()
setInnerTextSelector = mkSelector "setInnerText:"

-- | @Selector@ for @outerText@
outerTextSelector :: Selector '[] (Id NSString)
outerTextSelector = mkSelector "outerText"

-- | @Selector@ for @setOuterText:@
setOuterTextSelector :: Selector '[Id NSString] ()
setOuterTextSelector = mkSelector "setOuterText:"

-- | @Selector@ for @contentEditable@
contentEditableSelector :: Selector '[] (Id NSString)
contentEditableSelector = mkSelector "contentEditable"

-- | @Selector@ for @setContentEditable:@
setContentEditableSelector :: Selector '[Id NSString] ()
setContentEditableSelector = mkSelector "setContentEditable:"

-- | @Selector@ for @isContentEditable@
isContentEditableSelector :: Selector '[] Bool
isContentEditableSelector = mkSelector "isContentEditable"

-- | @Selector@ for @idName@
idNameSelector :: Selector '[] (Id NSString)
idNameSelector = mkSelector "idName"

-- | @Selector@ for @setIdName:@
setIdNameSelector :: Selector '[Id NSString] ()
setIdNameSelector = mkSelector "setIdName:"

-- | @Selector@ for @children@
childrenSelector :: Selector '[] (Id DOMHTMLCollection)
childrenSelector = mkSelector "children"

-- | @Selector@ for @titleDisplayString@
titleDisplayStringSelector :: Selector '[] (Id NSString)
titleDisplayStringSelector = mkSelector "titleDisplayString"

