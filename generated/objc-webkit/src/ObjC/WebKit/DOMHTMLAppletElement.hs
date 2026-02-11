{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLAppletElement@.
module ObjC.WebKit.DOMHTMLAppletElement
  ( DOMHTMLAppletElement
  , IsDOMHTMLAppletElement(..)
  , align
  , setAlign
  , alt
  , setAlt
  , archive
  , setArchive
  , code
  , setCode
  , codeBase
  , setCodeBase
  , height
  , setHeight
  , hspace
  , setHspace
  , name
  , setName
  , object
  , setObject
  , vspace
  , setVspace
  , width
  , setWidth
  , alignSelector
  , setAlignSelector
  , altSelector
  , setAltSelector
  , archiveSelector
  , setArchiveSelector
  , codeSelector
  , setCodeSelector
  , codeBaseSelector
  , setCodeBaseSelector
  , heightSelector
  , setHeightSelector
  , hspaceSelector
  , setHspaceSelector
  , nameSelector
  , setNameSelector
  , objectSelector
  , setObjectSelector
  , vspaceSelector
  , setVspaceSelector
  , widthSelector
  , setWidthSelector


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

-- | @- align@
align :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> IO (Id NSString)
align domhtmlAppletElement  =
  sendMsg domhtmlAppletElement (mkSelector "align") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlign:@
setAlign :: (IsDOMHTMLAppletElement domhtmlAppletElement, IsNSString value) => domhtmlAppletElement -> value -> IO ()
setAlign domhtmlAppletElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAppletElement (mkSelector "setAlign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- alt@
alt :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> IO (Id NSString)
alt domhtmlAppletElement  =
  sendMsg domhtmlAppletElement (mkSelector "alt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlt:@
setAlt :: (IsDOMHTMLAppletElement domhtmlAppletElement, IsNSString value) => domhtmlAppletElement -> value -> IO ()
setAlt domhtmlAppletElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAppletElement (mkSelector "setAlt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- archive@
archive :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> IO (Id NSString)
archive domhtmlAppletElement  =
  sendMsg domhtmlAppletElement (mkSelector "archive") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArchive:@
setArchive :: (IsDOMHTMLAppletElement domhtmlAppletElement, IsNSString value) => domhtmlAppletElement -> value -> IO ()
setArchive domhtmlAppletElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAppletElement (mkSelector "setArchive:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- code@
code :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> IO (Id NSString)
code domhtmlAppletElement  =
  sendMsg domhtmlAppletElement (mkSelector "code") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCode:@
setCode :: (IsDOMHTMLAppletElement domhtmlAppletElement, IsNSString value) => domhtmlAppletElement -> value -> IO ()
setCode domhtmlAppletElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAppletElement (mkSelector "setCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- codeBase@
codeBase :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> IO (Id NSString)
codeBase domhtmlAppletElement  =
  sendMsg domhtmlAppletElement (mkSelector "codeBase") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCodeBase:@
setCodeBase :: (IsDOMHTMLAppletElement domhtmlAppletElement, IsNSString value) => domhtmlAppletElement -> value -> IO ()
setCodeBase domhtmlAppletElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAppletElement (mkSelector "setCodeBase:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- height@
height :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> IO (Id NSString)
height domhtmlAppletElement  =
  sendMsg domhtmlAppletElement (mkSelector "height") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHeight:@
setHeight :: (IsDOMHTMLAppletElement domhtmlAppletElement, IsNSString value) => domhtmlAppletElement -> value -> IO ()
setHeight domhtmlAppletElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAppletElement (mkSelector "setHeight:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- hspace@
hspace :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> IO CInt
hspace domhtmlAppletElement  =
  sendMsg domhtmlAppletElement (mkSelector "hspace") retCInt []

-- | @- setHspace:@
setHspace :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> CInt -> IO ()
setHspace domhtmlAppletElement  value =
  sendMsg domhtmlAppletElement (mkSelector "setHspace:") retVoid [argCInt (fromIntegral value)]

-- | @- name@
name :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> IO (Id NSString)
name domhtmlAppletElement  =
  sendMsg domhtmlAppletElement (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsDOMHTMLAppletElement domhtmlAppletElement, IsNSString value) => domhtmlAppletElement -> value -> IO ()
setName domhtmlAppletElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAppletElement (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- object@
object :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> IO (Id NSString)
object domhtmlAppletElement  =
  sendMsg domhtmlAppletElement (mkSelector "object") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setObject:@
setObject :: (IsDOMHTMLAppletElement domhtmlAppletElement, IsNSString value) => domhtmlAppletElement -> value -> IO ()
setObject domhtmlAppletElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAppletElement (mkSelector "setObject:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- vspace@
vspace :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> IO CInt
vspace domhtmlAppletElement  =
  sendMsg domhtmlAppletElement (mkSelector "vspace") retCInt []

-- | @- setVspace:@
setVspace :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> CInt -> IO ()
setVspace domhtmlAppletElement  value =
  sendMsg domhtmlAppletElement (mkSelector "setVspace:") retVoid [argCInt (fromIntegral value)]

-- | @- width@
width :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> IO (Id NSString)
width domhtmlAppletElement  =
  sendMsg domhtmlAppletElement (mkSelector "width") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWidth:@
setWidth :: (IsDOMHTMLAppletElement domhtmlAppletElement, IsNSString value) => domhtmlAppletElement -> value -> IO ()
setWidth domhtmlAppletElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlAppletElement (mkSelector "setWidth:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @align@
alignSelector :: Selector
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector
setAlignSelector = mkSelector "setAlign:"

-- | @Selector@ for @alt@
altSelector :: Selector
altSelector = mkSelector "alt"

-- | @Selector@ for @setAlt:@
setAltSelector :: Selector
setAltSelector = mkSelector "setAlt:"

-- | @Selector@ for @archive@
archiveSelector :: Selector
archiveSelector = mkSelector "archive"

-- | @Selector@ for @setArchive:@
setArchiveSelector :: Selector
setArchiveSelector = mkSelector "setArchive:"

-- | @Selector@ for @code@
codeSelector :: Selector
codeSelector = mkSelector "code"

-- | @Selector@ for @setCode:@
setCodeSelector :: Selector
setCodeSelector = mkSelector "setCode:"

-- | @Selector@ for @codeBase@
codeBaseSelector :: Selector
codeBaseSelector = mkSelector "codeBase"

-- | @Selector@ for @setCodeBase:@
setCodeBaseSelector :: Selector
setCodeBaseSelector = mkSelector "setCodeBase:"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @hspace@
hspaceSelector :: Selector
hspaceSelector = mkSelector "hspace"

-- | @Selector@ for @setHspace:@
setHspaceSelector :: Selector
setHspaceSelector = mkSelector "setHspace:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @object@
objectSelector :: Selector
objectSelector = mkSelector "object"

-- | @Selector@ for @setObject:@
setObjectSelector :: Selector
setObjectSelector = mkSelector "setObject:"

-- | @Selector@ for @vspace@
vspaceSelector :: Selector
vspaceSelector = mkSelector "vspace"

-- | @Selector@ for @setVspace:@
setVspaceSelector :: Selector
setVspaceSelector = mkSelector "setVspace:"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector
setWidthSelector = mkSelector "setWidth:"

