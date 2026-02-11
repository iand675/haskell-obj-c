{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLObjectElement@.
module ObjC.WebKit.DOMHTMLObjectElement
  ( DOMHTMLObjectElement
  , IsDOMHTMLObjectElement(..)
  , form
  , code
  , setCode
  , align
  , setAlign
  , archive
  , setArchive
  , border
  , setBorder
  , codeBase
  , setCodeBase
  , codeType
  , setCodeType
  , data_
  , setData
  , declare
  , setDeclare
  , height
  , setHeight
  , hspace
  , setHspace
  , name
  , setName
  , standby
  , setStandby
  , type_
  , setType
  , useMap
  , setUseMap
  , vspace
  , setVspace
  , width
  , setWidth
  , contentDocument
  , absoluteImageURL
  , contentFrame
  , formSelector
  , codeSelector
  , setCodeSelector
  , alignSelector
  , setAlignSelector
  , archiveSelector
  , setArchiveSelector
  , borderSelector
  , setBorderSelector
  , codeBaseSelector
  , setCodeBaseSelector
  , codeTypeSelector
  , setCodeTypeSelector
  , dataSelector
  , setDataSelector
  , declareSelector
  , setDeclareSelector
  , heightSelector
  , setHeightSelector
  , hspaceSelector
  , setHspaceSelector
  , nameSelector
  , setNameSelector
  , standbySelector
  , setStandbySelector
  , typeSelector
  , setTypeSelector
  , useMapSelector
  , setUseMapSelector
  , vspaceSelector
  , setVspaceSelector
  , widthSelector
  , setWidthSelector
  , contentDocumentSelector
  , absoluteImageURLSelector
  , contentFrameSelector


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

-- | @- form@
form :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id DOMHTMLFormElement)
form domhtmlObjectElement  =
  sendMsg domhtmlObjectElement (mkSelector "form") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- code@
code :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
code domhtmlObjectElement  =
  sendMsg domhtmlObjectElement (mkSelector "code") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCode:@
setCode :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setCode domhtmlObjectElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlObjectElement (mkSelector "setCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- align@
align :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
align domhtmlObjectElement  =
  sendMsg domhtmlObjectElement (mkSelector "align") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlign:@
setAlign :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setAlign domhtmlObjectElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlObjectElement (mkSelector "setAlign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- archive@
archive :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
archive domhtmlObjectElement  =
  sendMsg domhtmlObjectElement (mkSelector "archive") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setArchive:@
setArchive :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setArchive domhtmlObjectElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlObjectElement (mkSelector "setArchive:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- border@
border :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
border domhtmlObjectElement  =
  sendMsg domhtmlObjectElement (mkSelector "border") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorder:@
setBorder :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setBorder domhtmlObjectElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlObjectElement (mkSelector "setBorder:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- codeBase@
codeBase :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
codeBase domhtmlObjectElement  =
  sendMsg domhtmlObjectElement (mkSelector "codeBase") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCodeBase:@
setCodeBase :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setCodeBase domhtmlObjectElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlObjectElement (mkSelector "setCodeBase:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- codeType@
codeType :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
codeType domhtmlObjectElement  =
  sendMsg domhtmlObjectElement (mkSelector "codeType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCodeType:@
setCodeType :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setCodeType domhtmlObjectElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlObjectElement (mkSelector "setCodeType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- data@
data_ :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
data_ domhtmlObjectElement  =
  sendMsg domhtmlObjectElement (mkSelector "data") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setData:@
setData :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setData domhtmlObjectElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlObjectElement (mkSelector "setData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- declare@
declare :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO Bool
declare domhtmlObjectElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlObjectElement (mkSelector "declare") retCULong []

-- | @- setDeclare:@
setDeclare :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> Bool -> IO ()
setDeclare domhtmlObjectElement  value =
  sendMsg domhtmlObjectElement (mkSelector "setDeclare:") retVoid [argCULong (if value then 1 else 0)]

-- | @- height@
height :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
height domhtmlObjectElement  =
  sendMsg domhtmlObjectElement (mkSelector "height") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHeight:@
setHeight :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setHeight domhtmlObjectElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlObjectElement (mkSelector "setHeight:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- hspace@
hspace :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO CInt
hspace domhtmlObjectElement  =
  sendMsg domhtmlObjectElement (mkSelector "hspace") retCInt []

-- | @- setHspace:@
setHspace :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> CInt -> IO ()
setHspace domhtmlObjectElement  value =
  sendMsg domhtmlObjectElement (mkSelector "setHspace:") retVoid [argCInt (fromIntegral value)]

-- | @- name@
name :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
name domhtmlObjectElement  =
  sendMsg domhtmlObjectElement (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setName domhtmlObjectElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlObjectElement (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- standby@
standby :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
standby domhtmlObjectElement  =
  sendMsg domhtmlObjectElement (mkSelector "standby") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStandby:@
setStandby :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setStandby domhtmlObjectElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlObjectElement (mkSelector "setStandby:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- type@
type_ :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
type_ domhtmlObjectElement  =
  sendMsg domhtmlObjectElement (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setType domhtmlObjectElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlObjectElement (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- useMap@
useMap :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
useMap domhtmlObjectElement  =
  sendMsg domhtmlObjectElement (mkSelector "useMap") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUseMap:@
setUseMap :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setUseMap domhtmlObjectElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlObjectElement (mkSelector "setUseMap:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- vspace@
vspace :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO CInt
vspace domhtmlObjectElement  =
  sendMsg domhtmlObjectElement (mkSelector "vspace") retCInt []

-- | @- setVspace:@
setVspace :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> CInt -> IO ()
setVspace domhtmlObjectElement  value =
  sendMsg domhtmlObjectElement (mkSelector "setVspace:") retVoid [argCInt (fromIntegral value)]

-- | @- width@
width :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
width domhtmlObjectElement  =
  sendMsg domhtmlObjectElement (mkSelector "width") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setWidth:@
setWidth :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setWidth domhtmlObjectElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlObjectElement (mkSelector "setWidth:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- contentDocument@
contentDocument :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id DOMDocument)
contentDocument domhtmlObjectElement  =
  sendMsg domhtmlObjectElement (mkSelector "contentDocument") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- absoluteImageURL@
absoluteImageURL :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSURL)
absoluteImageURL domhtmlObjectElement  =
  sendMsg domhtmlObjectElement (mkSelector "absoluteImageURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | contentFrame
--
-- The content frame of the element.
--
-- Returns non-nil only if the object represents a child frame    such as if the data of the object is HTML content.
--
-- ObjC selector: @- contentFrame@
contentFrame :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id WebFrame)
contentFrame domhtmlObjectElement  =
  sendMsg domhtmlObjectElement (mkSelector "contentFrame") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @form@
formSelector :: Selector
formSelector = mkSelector "form"

-- | @Selector@ for @code@
codeSelector :: Selector
codeSelector = mkSelector "code"

-- | @Selector@ for @setCode:@
setCodeSelector :: Selector
setCodeSelector = mkSelector "setCode:"

-- | @Selector@ for @align@
alignSelector :: Selector
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector
setAlignSelector = mkSelector "setAlign:"

-- | @Selector@ for @archive@
archiveSelector :: Selector
archiveSelector = mkSelector "archive"

-- | @Selector@ for @setArchive:@
setArchiveSelector :: Selector
setArchiveSelector = mkSelector "setArchive:"

-- | @Selector@ for @border@
borderSelector :: Selector
borderSelector = mkSelector "border"

-- | @Selector@ for @setBorder:@
setBorderSelector :: Selector
setBorderSelector = mkSelector "setBorder:"

-- | @Selector@ for @codeBase@
codeBaseSelector :: Selector
codeBaseSelector = mkSelector "codeBase"

-- | @Selector@ for @setCodeBase:@
setCodeBaseSelector :: Selector
setCodeBaseSelector = mkSelector "setCodeBase:"

-- | @Selector@ for @codeType@
codeTypeSelector :: Selector
codeTypeSelector = mkSelector "codeType"

-- | @Selector@ for @setCodeType:@
setCodeTypeSelector :: Selector
setCodeTypeSelector = mkSelector "setCodeType:"

-- | @Selector@ for @data@
dataSelector :: Selector
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector
setDataSelector = mkSelector "setData:"

-- | @Selector@ for @declare@
declareSelector :: Selector
declareSelector = mkSelector "declare"

-- | @Selector@ for @setDeclare:@
setDeclareSelector :: Selector
setDeclareSelector = mkSelector "setDeclare:"

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

-- | @Selector@ for @standby@
standbySelector :: Selector
standbySelector = mkSelector "standby"

-- | @Selector@ for @setStandby:@
setStandbySelector :: Selector
setStandbySelector = mkSelector "setStandby:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @useMap@
useMapSelector :: Selector
useMapSelector = mkSelector "useMap"

-- | @Selector@ for @setUseMap:@
setUseMapSelector :: Selector
setUseMapSelector = mkSelector "setUseMap:"

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

-- | @Selector@ for @contentDocument@
contentDocumentSelector :: Selector
contentDocumentSelector = mkSelector "contentDocument"

-- | @Selector@ for @absoluteImageURL@
absoluteImageURLSelector :: Selector
absoluteImageURLSelector = mkSelector "absoluteImageURL"

-- | @Selector@ for @contentFrame@
contentFrameSelector :: Selector
contentFrameSelector = mkSelector "contentFrame"

