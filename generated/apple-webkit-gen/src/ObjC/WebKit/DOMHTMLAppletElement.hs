{-# LANGUAGE DataKinds #-}
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
  , altSelector
  , archiveSelector
  , codeBaseSelector
  , codeSelector
  , heightSelector
  , hspaceSelector
  , nameSelector
  , objectSelector
  , setAlignSelector
  , setAltSelector
  , setArchiveSelector
  , setCodeBaseSelector
  , setCodeSelector
  , setHeightSelector
  , setHspaceSelector
  , setNameSelector
  , setObjectSelector
  , setVspaceSelector
  , setWidthSelector
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

-- | @- align@
align :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> IO (Id NSString)
align domhtmlAppletElement =
  sendMessage domhtmlAppletElement alignSelector

-- | @- setAlign:@
setAlign :: (IsDOMHTMLAppletElement domhtmlAppletElement, IsNSString value) => domhtmlAppletElement -> value -> IO ()
setAlign domhtmlAppletElement value =
  sendMessage domhtmlAppletElement setAlignSelector (toNSString value)

-- | @- alt@
alt :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> IO (Id NSString)
alt domhtmlAppletElement =
  sendMessage domhtmlAppletElement altSelector

-- | @- setAlt:@
setAlt :: (IsDOMHTMLAppletElement domhtmlAppletElement, IsNSString value) => domhtmlAppletElement -> value -> IO ()
setAlt domhtmlAppletElement value =
  sendMessage domhtmlAppletElement setAltSelector (toNSString value)

-- | @- archive@
archive :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> IO (Id NSString)
archive domhtmlAppletElement =
  sendMessage domhtmlAppletElement archiveSelector

-- | @- setArchive:@
setArchive :: (IsDOMHTMLAppletElement domhtmlAppletElement, IsNSString value) => domhtmlAppletElement -> value -> IO ()
setArchive domhtmlAppletElement value =
  sendMessage domhtmlAppletElement setArchiveSelector (toNSString value)

-- | @- code@
code :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> IO (Id NSString)
code domhtmlAppletElement =
  sendMessage domhtmlAppletElement codeSelector

-- | @- setCode:@
setCode :: (IsDOMHTMLAppletElement domhtmlAppletElement, IsNSString value) => domhtmlAppletElement -> value -> IO ()
setCode domhtmlAppletElement value =
  sendMessage domhtmlAppletElement setCodeSelector (toNSString value)

-- | @- codeBase@
codeBase :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> IO (Id NSString)
codeBase domhtmlAppletElement =
  sendMessage domhtmlAppletElement codeBaseSelector

-- | @- setCodeBase:@
setCodeBase :: (IsDOMHTMLAppletElement domhtmlAppletElement, IsNSString value) => domhtmlAppletElement -> value -> IO ()
setCodeBase domhtmlAppletElement value =
  sendMessage domhtmlAppletElement setCodeBaseSelector (toNSString value)

-- | @- height@
height :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> IO (Id NSString)
height domhtmlAppletElement =
  sendMessage domhtmlAppletElement heightSelector

-- | @- setHeight:@
setHeight :: (IsDOMHTMLAppletElement domhtmlAppletElement, IsNSString value) => domhtmlAppletElement -> value -> IO ()
setHeight domhtmlAppletElement value =
  sendMessage domhtmlAppletElement setHeightSelector (toNSString value)

-- | @- hspace@
hspace :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> IO CInt
hspace domhtmlAppletElement =
  sendMessage domhtmlAppletElement hspaceSelector

-- | @- setHspace:@
setHspace :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> CInt -> IO ()
setHspace domhtmlAppletElement value =
  sendMessage domhtmlAppletElement setHspaceSelector value

-- | @- name@
name :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> IO (Id NSString)
name domhtmlAppletElement =
  sendMessage domhtmlAppletElement nameSelector

-- | @- setName:@
setName :: (IsDOMHTMLAppletElement domhtmlAppletElement, IsNSString value) => domhtmlAppletElement -> value -> IO ()
setName domhtmlAppletElement value =
  sendMessage domhtmlAppletElement setNameSelector (toNSString value)

-- | @- object@
object :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> IO (Id NSString)
object domhtmlAppletElement =
  sendMessage domhtmlAppletElement objectSelector

-- | @- setObject:@
setObject :: (IsDOMHTMLAppletElement domhtmlAppletElement, IsNSString value) => domhtmlAppletElement -> value -> IO ()
setObject domhtmlAppletElement value =
  sendMessage domhtmlAppletElement setObjectSelector (toNSString value)

-- | @- vspace@
vspace :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> IO CInt
vspace domhtmlAppletElement =
  sendMessage domhtmlAppletElement vspaceSelector

-- | @- setVspace:@
setVspace :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> CInt -> IO ()
setVspace domhtmlAppletElement value =
  sendMessage domhtmlAppletElement setVspaceSelector value

-- | @- width@
width :: IsDOMHTMLAppletElement domhtmlAppletElement => domhtmlAppletElement -> IO (Id NSString)
width domhtmlAppletElement =
  sendMessage domhtmlAppletElement widthSelector

-- | @- setWidth:@
setWidth :: (IsDOMHTMLAppletElement domhtmlAppletElement, IsNSString value) => domhtmlAppletElement -> value -> IO ()
setWidth domhtmlAppletElement value =
  sendMessage domhtmlAppletElement setWidthSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @align@
alignSelector :: Selector '[] (Id NSString)
alignSelector = mkSelector "align"

-- | @Selector@ for @setAlign:@
setAlignSelector :: Selector '[Id NSString] ()
setAlignSelector = mkSelector "setAlign:"

-- | @Selector@ for @alt@
altSelector :: Selector '[] (Id NSString)
altSelector = mkSelector "alt"

-- | @Selector@ for @setAlt:@
setAltSelector :: Selector '[Id NSString] ()
setAltSelector = mkSelector "setAlt:"

-- | @Selector@ for @archive@
archiveSelector :: Selector '[] (Id NSString)
archiveSelector = mkSelector "archive"

-- | @Selector@ for @setArchive:@
setArchiveSelector :: Selector '[Id NSString] ()
setArchiveSelector = mkSelector "setArchive:"

-- | @Selector@ for @code@
codeSelector :: Selector '[] (Id NSString)
codeSelector = mkSelector "code"

-- | @Selector@ for @setCode:@
setCodeSelector :: Selector '[Id NSString] ()
setCodeSelector = mkSelector "setCode:"

-- | @Selector@ for @codeBase@
codeBaseSelector :: Selector '[] (Id NSString)
codeBaseSelector = mkSelector "codeBase"

-- | @Selector@ for @setCodeBase:@
setCodeBaseSelector :: Selector '[Id NSString] ()
setCodeBaseSelector = mkSelector "setCodeBase:"

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

-- | @Selector@ for @object@
objectSelector :: Selector '[] (Id NSString)
objectSelector = mkSelector "object"

-- | @Selector@ for @setObject:@
setObjectSelector :: Selector '[Id NSString] ()
setObjectSelector = mkSelector "setObject:"

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

