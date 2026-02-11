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
  , clickSelector
  , titleSelector
  , setTitleSelector
  , langSelector
  , setLangSelector
  , dirSelector
  , setDirSelector
  , tabIndexSelector
  , setTabIndexSelector
  , accessKeySelector
  , setAccessKeySelector
  , innerTextSelector
  , setInnerTextSelector
  , outerTextSelector
  , setOuterTextSelector
  , contentEditableSelector
  , setContentEditableSelector
  , isContentEditableSelector
  , idNameSelector
  , setIdNameSelector
  , childrenSelector
  , titleDisplayStringSelector


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

-- | @- click@
click :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO ()
click domhtmlElement  =
  sendMsg domhtmlElement (mkSelector "click") retVoid []

-- | @- title@
title :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO (Id NSString)
title domhtmlElement  =
  sendMsg domhtmlElement (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsDOMHTMLElement domhtmlElement, IsNSString value) => domhtmlElement -> value -> IO ()
setTitle domhtmlElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlElement (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- lang@
lang :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO (Id NSString)
lang domhtmlElement  =
  sendMsg domhtmlElement (mkSelector "lang") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLang:@
setLang :: (IsDOMHTMLElement domhtmlElement, IsNSString value) => domhtmlElement -> value -> IO ()
setLang domhtmlElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlElement (mkSelector "setLang:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- dir@
dir :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO (Id NSString)
dir domhtmlElement  =
  sendMsg domhtmlElement (mkSelector "dir") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDir:@
setDir :: (IsDOMHTMLElement domhtmlElement, IsNSString value) => domhtmlElement -> value -> IO ()
setDir domhtmlElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlElement (mkSelector "setDir:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tabIndex@
tabIndex :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO CInt
tabIndex domhtmlElement  =
  sendMsg domhtmlElement (mkSelector "tabIndex") retCInt []

-- | @- setTabIndex:@
setTabIndex :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> CInt -> IO ()
setTabIndex domhtmlElement  value =
  sendMsg domhtmlElement (mkSelector "setTabIndex:") retVoid [argCInt (fromIntegral value)]

-- | @- accessKey@
accessKey :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO (Id NSString)
accessKey domhtmlElement  =
  sendMsg domhtmlElement (mkSelector "accessKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccessKey:@
setAccessKey :: (IsDOMHTMLElement domhtmlElement, IsNSString value) => domhtmlElement -> value -> IO ()
setAccessKey domhtmlElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlElement (mkSelector "setAccessKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- innerText@
innerText :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO (Id NSString)
innerText domhtmlElement  =
  sendMsg domhtmlElement (mkSelector "innerText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setInnerText:@
setInnerText :: (IsDOMHTMLElement domhtmlElement, IsNSString value) => domhtmlElement -> value -> IO ()
setInnerText domhtmlElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlElement (mkSelector "setInnerText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- outerText@
outerText :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO (Id NSString)
outerText domhtmlElement  =
  sendMsg domhtmlElement (mkSelector "outerText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOuterText:@
setOuterText :: (IsDOMHTMLElement domhtmlElement, IsNSString value) => domhtmlElement -> value -> IO ()
setOuterText domhtmlElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlElement (mkSelector "setOuterText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contentEditable@
contentEditable :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO (Id NSString)
contentEditable domhtmlElement  =
  sendMsg domhtmlElement (mkSelector "contentEditable") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentEditable:@
setContentEditable :: (IsDOMHTMLElement domhtmlElement, IsNSString value) => domhtmlElement -> value -> IO ()
setContentEditable domhtmlElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlElement (mkSelector "setContentEditable:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- isContentEditable@
isContentEditable :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO Bool
isContentEditable domhtmlElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlElement (mkSelector "isContentEditable") retCULong []

-- | @- idName@
idName :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO (Id NSString)
idName domhtmlElement  =
  sendMsg domhtmlElement (mkSelector "idName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIdName:@
setIdName :: (IsDOMHTMLElement domhtmlElement, IsNSString value) => domhtmlElement -> value -> IO ()
setIdName domhtmlElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlElement (mkSelector "setIdName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- children@
children :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO (Id DOMHTMLCollection)
children domhtmlElement  =
  sendMsg domhtmlElement (mkSelector "children") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- titleDisplayString@
titleDisplayString :: IsDOMHTMLElement domhtmlElement => domhtmlElement -> IO (Id NSString)
titleDisplayString domhtmlElement  =
  sendMsg domhtmlElement (mkSelector "titleDisplayString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @click@
clickSelector :: Selector
clickSelector = mkSelector "click"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @lang@
langSelector :: Selector
langSelector = mkSelector "lang"

-- | @Selector@ for @setLang:@
setLangSelector :: Selector
setLangSelector = mkSelector "setLang:"

-- | @Selector@ for @dir@
dirSelector :: Selector
dirSelector = mkSelector "dir"

-- | @Selector@ for @setDir:@
setDirSelector :: Selector
setDirSelector = mkSelector "setDir:"

-- | @Selector@ for @tabIndex@
tabIndexSelector :: Selector
tabIndexSelector = mkSelector "tabIndex"

-- | @Selector@ for @setTabIndex:@
setTabIndexSelector :: Selector
setTabIndexSelector = mkSelector "setTabIndex:"

-- | @Selector@ for @accessKey@
accessKeySelector :: Selector
accessKeySelector = mkSelector "accessKey"

-- | @Selector@ for @setAccessKey:@
setAccessKeySelector :: Selector
setAccessKeySelector = mkSelector "setAccessKey:"

-- | @Selector@ for @innerText@
innerTextSelector :: Selector
innerTextSelector = mkSelector "innerText"

-- | @Selector@ for @setInnerText:@
setInnerTextSelector :: Selector
setInnerTextSelector = mkSelector "setInnerText:"

-- | @Selector@ for @outerText@
outerTextSelector :: Selector
outerTextSelector = mkSelector "outerText"

-- | @Selector@ for @setOuterText:@
setOuterTextSelector :: Selector
setOuterTextSelector = mkSelector "setOuterText:"

-- | @Selector@ for @contentEditable@
contentEditableSelector :: Selector
contentEditableSelector = mkSelector "contentEditable"

-- | @Selector@ for @setContentEditable:@
setContentEditableSelector :: Selector
setContentEditableSelector = mkSelector "setContentEditable:"

-- | @Selector@ for @isContentEditable@
isContentEditableSelector :: Selector
isContentEditableSelector = mkSelector "isContentEditable"

-- | @Selector@ for @idName@
idNameSelector :: Selector
idNameSelector = mkSelector "idName"

-- | @Selector@ for @setIdName:@
setIdNameSelector :: Selector
setIdNameSelector = mkSelector "setIdName:"

-- | @Selector@ for @children@
childrenSelector :: Selector
childrenSelector = mkSelector "children"

-- | @Selector@ for @titleDisplayString@
titleDisplayStringSelector :: Selector
titleDisplayStringSelector = mkSelector "titleDisplayString"

