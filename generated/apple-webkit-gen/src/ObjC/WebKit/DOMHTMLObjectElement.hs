{-# LANGUAGE DataKinds #-}
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
  , absoluteImageURLSelector
  , alignSelector
  , archiveSelector
  , borderSelector
  , codeBaseSelector
  , codeSelector
  , codeTypeSelector
  , contentDocumentSelector
  , contentFrameSelector
  , dataSelector
  , declareSelector
  , formSelector
  , heightSelector
  , hspaceSelector
  , nameSelector
  , setAlignSelector
  , setArchiveSelector
  , setBorderSelector
  , setCodeBaseSelector
  , setCodeSelector
  , setCodeTypeSelector
  , setDataSelector
  , setDeclareSelector
  , setHeightSelector
  , setHspaceSelector
  , setNameSelector
  , setStandbySelector
  , setTypeSelector
  , setUseMapSelector
  , setVspaceSelector
  , setWidthSelector
  , standbySelector
  , typeSelector
  , useMapSelector
  , vspaceSelector
  , widthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- form@
form :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id DOMHTMLFormElement)
form domhtmlObjectElement =
  sendMessage domhtmlObjectElement formSelector

-- | @- code@
code :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
code domhtmlObjectElement =
  sendMessage domhtmlObjectElement codeSelector

-- | @- setCode:@
setCode :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setCode domhtmlObjectElement value =
  sendMessage domhtmlObjectElement setCodeSelector (toNSString value)

-- | @- align@
align :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
align domhtmlObjectElement =
  sendMessage domhtmlObjectElement alignSelector

-- | @- setAlign:@
setAlign :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setAlign domhtmlObjectElement value =
  sendMessage domhtmlObjectElement setAlignSelector (toNSString value)

-- | @- archive@
archive :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
archive domhtmlObjectElement =
  sendMessage domhtmlObjectElement archiveSelector

-- | @- setArchive:@
setArchive :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setArchive domhtmlObjectElement value =
  sendMessage domhtmlObjectElement setArchiveSelector (toNSString value)

-- | @- border@
border :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
border domhtmlObjectElement =
  sendMessage domhtmlObjectElement borderSelector

-- | @- setBorder:@
setBorder :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setBorder domhtmlObjectElement value =
  sendMessage domhtmlObjectElement setBorderSelector (toNSString value)

-- | @- codeBase@
codeBase :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
codeBase domhtmlObjectElement =
  sendMessage domhtmlObjectElement codeBaseSelector

-- | @- setCodeBase:@
setCodeBase :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setCodeBase domhtmlObjectElement value =
  sendMessage domhtmlObjectElement setCodeBaseSelector (toNSString value)

-- | @- codeType@
codeType :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
codeType domhtmlObjectElement =
  sendMessage domhtmlObjectElement codeTypeSelector

-- | @- setCodeType:@
setCodeType :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setCodeType domhtmlObjectElement value =
  sendMessage domhtmlObjectElement setCodeTypeSelector (toNSString value)

-- | @- data@
data_ :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
data_ domhtmlObjectElement =
  sendMessage domhtmlObjectElement dataSelector

-- | @- setData:@
setData :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setData domhtmlObjectElement value =
  sendMessage domhtmlObjectElement setDataSelector (toNSString value)

-- | @- declare@
declare :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO Bool
declare domhtmlObjectElement =
  sendMessage domhtmlObjectElement declareSelector

-- | @- setDeclare:@
setDeclare :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> Bool -> IO ()
setDeclare domhtmlObjectElement value =
  sendMessage domhtmlObjectElement setDeclareSelector value

-- | @- height@
height :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
height domhtmlObjectElement =
  sendMessage domhtmlObjectElement heightSelector

-- | @- setHeight:@
setHeight :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setHeight domhtmlObjectElement value =
  sendMessage domhtmlObjectElement setHeightSelector (toNSString value)

-- | @- hspace@
hspace :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO CInt
hspace domhtmlObjectElement =
  sendMessage domhtmlObjectElement hspaceSelector

-- | @- setHspace:@
setHspace :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> CInt -> IO ()
setHspace domhtmlObjectElement value =
  sendMessage domhtmlObjectElement setHspaceSelector value

-- | @- name@
name :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
name domhtmlObjectElement =
  sendMessage domhtmlObjectElement nameSelector

-- | @- setName:@
setName :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setName domhtmlObjectElement value =
  sendMessage domhtmlObjectElement setNameSelector (toNSString value)

-- | @- standby@
standby :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
standby domhtmlObjectElement =
  sendMessage domhtmlObjectElement standbySelector

-- | @- setStandby:@
setStandby :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setStandby domhtmlObjectElement value =
  sendMessage domhtmlObjectElement setStandbySelector (toNSString value)

-- | @- type@
type_ :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
type_ domhtmlObjectElement =
  sendMessage domhtmlObjectElement typeSelector

-- | @- setType:@
setType :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setType domhtmlObjectElement value =
  sendMessage domhtmlObjectElement setTypeSelector (toNSString value)

-- | @- useMap@
useMap :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
useMap domhtmlObjectElement =
  sendMessage domhtmlObjectElement useMapSelector

-- | @- setUseMap:@
setUseMap :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setUseMap domhtmlObjectElement value =
  sendMessage domhtmlObjectElement setUseMapSelector (toNSString value)

-- | @- vspace@
vspace :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO CInt
vspace domhtmlObjectElement =
  sendMessage domhtmlObjectElement vspaceSelector

-- | @- setVspace:@
setVspace :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> CInt -> IO ()
setVspace domhtmlObjectElement value =
  sendMessage domhtmlObjectElement setVspaceSelector value

-- | @- width@
width :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSString)
width domhtmlObjectElement =
  sendMessage domhtmlObjectElement widthSelector

-- | @- setWidth:@
setWidth :: (IsDOMHTMLObjectElement domhtmlObjectElement, IsNSString value) => domhtmlObjectElement -> value -> IO ()
setWidth domhtmlObjectElement value =
  sendMessage domhtmlObjectElement setWidthSelector (toNSString value)

-- | @- contentDocument@
contentDocument :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id DOMDocument)
contentDocument domhtmlObjectElement =
  sendMessage domhtmlObjectElement contentDocumentSelector

-- | @- absoluteImageURL@
absoluteImageURL :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id NSURL)
absoluteImageURL domhtmlObjectElement =
  sendMessage domhtmlObjectElement absoluteImageURLSelector

-- | contentFrame
--
-- The content frame of the element.
--
-- Returns non-nil only if the object represents a child frame    such as if the data of the object is HTML content.
--
-- ObjC selector: @- contentFrame@
contentFrame :: IsDOMHTMLObjectElement domhtmlObjectElement => domhtmlObjectElement -> IO (Id WebFrame)
contentFrame domhtmlObjectElement =
  sendMessage domhtmlObjectElement contentFrameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @form@
formSelector :: Selector '[] (Id DOMHTMLFormElement)
formSelector = mkSelector "form"

-- | @Selector@ for @code@
codeSelector :: Selector '[] (Id NSString)
codeSelector = mkSelector "code"

-- | @Selector@ for @setCode:@
setCodeSelector :: Selector '[Id NSString] ()
setCodeSelector = mkSelector "setCode:"

-- | @Selector@ for @align@
alignSelector :: Selector '[] (Id NSString)
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector '[Id NSString] ()
setAlignSelector = mkSelector "setAlign:"

-- | @Selector@ for @archive@
archiveSelector :: Selector '[] (Id NSString)
archiveSelector = mkSelector "archive"

-- | @Selector@ for @setArchive:@
setArchiveSelector :: Selector '[Id NSString] ()
setArchiveSelector = mkSelector "setArchive:"

-- | @Selector@ for @border@
borderSelector :: Selector '[] (Id NSString)
borderSelector = mkSelector "border"

-- | @Selector@ for @setBorder:@
setBorderSelector :: Selector '[Id NSString] ()
setBorderSelector = mkSelector "setBorder:"

-- | @Selector@ for @codeBase@
codeBaseSelector :: Selector '[] (Id NSString)
codeBaseSelector = mkSelector "codeBase"

-- | @Selector@ for @setCodeBase:@
setCodeBaseSelector :: Selector '[Id NSString] ()
setCodeBaseSelector = mkSelector "setCodeBase:"

-- | @Selector@ for @codeType@
codeTypeSelector :: Selector '[] (Id NSString)
codeTypeSelector = mkSelector "codeType"

-- | @Selector@ for @setCodeType:@
setCodeTypeSelector :: Selector '[Id NSString] ()
setCodeTypeSelector = mkSelector "setCodeType:"

-- | @Selector@ for @data@
dataSelector :: Selector '[] (Id NSString)
dataSelector = mkSelector "data"

-- | @Selector@ for @setData:@
setDataSelector :: Selector '[Id NSString] ()
setDataSelector = mkSelector "setData:"

-- | @Selector@ for @declare@
declareSelector :: Selector '[] Bool
declareSelector = mkSelector "declare"

-- | @Selector@ for @setDeclare:@
setDeclareSelector :: Selector '[Bool] ()
setDeclareSelector = mkSelector "setDeclare:"

-- | @Selector@ for @height@
heightSelector :: Selector '[] (Id NSString)
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector '[Id NSString] ()
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @hspace@
hspaceSelector :: Selector '[] CInt
hspaceSelector = mkSelector "hspace"

-- | @Selector@ for @setHspace:@
setHspaceSelector :: Selector '[CInt] ()
setHspaceSelector = mkSelector "setHspace:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @standby@
standbySelector :: Selector '[] (Id NSString)
standbySelector = mkSelector "standby"

-- | @Selector@ for @setStandby:@
setStandbySelector :: Selector '[Id NSString] ()
setStandbySelector = mkSelector "setStandby:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector '[Id NSString] ()
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @useMap@
useMapSelector :: Selector '[] (Id NSString)
useMapSelector = mkSelector "useMap"

-- | @Selector@ for @setUseMap:@
setUseMapSelector :: Selector '[Id NSString] ()
setUseMapSelector = mkSelector "setUseMap:"

-- | @Selector@ for @vspace@
vspaceSelector :: Selector '[] CInt
vspaceSelector = mkSelector "vspace"

-- | @Selector@ for @setVspace:@
setVspaceSelector :: Selector '[CInt] ()
setVspaceSelector = mkSelector "setVspace:"

-- | @Selector@ for @width@
widthSelector :: Selector '[] (Id NSString)
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector '[Id NSString] ()
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @contentDocument@
contentDocumentSelector :: Selector '[] (Id DOMDocument)
contentDocumentSelector = mkSelector "contentDocument"

-- | @Selector@ for @absoluteImageURL@
absoluteImageURLSelector :: Selector '[] (Id NSURL)
absoluteImageURLSelector = mkSelector "absoluteImageURL"

-- | @Selector@ for @contentFrame@
contentFrameSelector :: Selector '[] (Id WebFrame)
contentFrameSelector = mkSelector "contentFrame"

