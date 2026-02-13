{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMHTMLImageElement@.
module ObjC.WebKit.DOMHTMLImageElement
  ( DOMHTMLImageElement
  , IsDOMHTMLImageElement(..)
  , name
  , setName
  , align
  , setAlign
  , alt
  , setAlt
  , border
  , setBorder
  , height
  , setHeight
  , hspace
  , setHspace
  , isMap
  , setIsMap
  , longDesc
  , setLongDesc
  , src
  , setSrc
  , useMap
  , setUseMap
  , vspace
  , setVspace
  , width
  , setWidth
  , complete
  , lowsrc
  , setLowsrc
  , naturalHeight
  , naturalWidth
  , x
  , y
  , altDisplayString
  , absoluteImageURL
  , absoluteImageURLSelector
  , alignSelector
  , altDisplayStringSelector
  , altSelector
  , borderSelector
  , completeSelector
  , heightSelector
  , hspaceSelector
  , isMapSelector
  , longDescSelector
  , lowsrcSelector
  , nameSelector
  , naturalHeightSelector
  , naturalWidthSelector
  , setAlignSelector
  , setAltSelector
  , setBorderSelector
  , setHeightSelector
  , setHspaceSelector
  , setIsMapSelector
  , setLongDescSelector
  , setLowsrcSelector
  , setNameSelector
  , setSrcSelector
  , setUseMapSelector
  , setVspaceSelector
  , setWidthSelector
  , srcSelector
  , useMapSelector
  , vspaceSelector
  , widthSelector
  , xSelector
  , ySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- name@
name :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO (Id NSString)
name domhtmlImageElement =
  sendMessage domhtmlImageElement nameSelector

-- | @- setName:@
setName :: (IsDOMHTMLImageElement domhtmlImageElement, IsNSString value) => domhtmlImageElement -> value -> IO ()
setName domhtmlImageElement value =
  sendMessage domhtmlImageElement setNameSelector (toNSString value)

-- | @- align@
align :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO (Id NSString)
align domhtmlImageElement =
  sendMessage domhtmlImageElement alignSelector

-- | @- setAlign:@
setAlign :: (IsDOMHTMLImageElement domhtmlImageElement, IsNSString value) => domhtmlImageElement -> value -> IO ()
setAlign domhtmlImageElement value =
  sendMessage domhtmlImageElement setAlignSelector (toNSString value)

-- | @- alt@
alt :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO (Id NSString)
alt domhtmlImageElement =
  sendMessage domhtmlImageElement altSelector

-- | @- setAlt:@
setAlt :: (IsDOMHTMLImageElement domhtmlImageElement, IsNSString value) => domhtmlImageElement -> value -> IO ()
setAlt domhtmlImageElement value =
  sendMessage domhtmlImageElement setAltSelector (toNSString value)

-- | @- border@
border :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO (Id NSString)
border domhtmlImageElement =
  sendMessage domhtmlImageElement borderSelector

-- | @- setBorder:@
setBorder :: (IsDOMHTMLImageElement domhtmlImageElement, IsNSString value) => domhtmlImageElement -> value -> IO ()
setBorder domhtmlImageElement value =
  sendMessage domhtmlImageElement setBorderSelector (toNSString value)

-- | @- height@
height :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO CInt
height domhtmlImageElement =
  sendMessage domhtmlImageElement heightSelector

-- | @- setHeight:@
setHeight :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> CInt -> IO ()
setHeight domhtmlImageElement value =
  sendMessage domhtmlImageElement setHeightSelector value

-- | @- hspace@
hspace :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO CInt
hspace domhtmlImageElement =
  sendMessage domhtmlImageElement hspaceSelector

-- | @- setHspace:@
setHspace :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> CInt -> IO ()
setHspace domhtmlImageElement value =
  sendMessage domhtmlImageElement setHspaceSelector value

-- | @- isMap@
isMap :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO Bool
isMap domhtmlImageElement =
  sendMessage domhtmlImageElement isMapSelector

-- | @- setIsMap:@
setIsMap :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> Bool -> IO ()
setIsMap domhtmlImageElement value =
  sendMessage domhtmlImageElement setIsMapSelector value

-- | @- longDesc@
longDesc :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO (Id NSString)
longDesc domhtmlImageElement =
  sendMessage domhtmlImageElement longDescSelector

-- | @- setLongDesc:@
setLongDesc :: (IsDOMHTMLImageElement domhtmlImageElement, IsNSString value) => domhtmlImageElement -> value -> IO ()
setLongDesc domhtmlImageElement value =
  sendMessage domhtmlImageElement setLongDescSelector (toNSString value)

-- | @- src@
src :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO (Id NSString)
src domhtmlImageElement =
  sendMessage domhtmlImageElement srcSelector

-- | @- setSrc:@
setSrc :: (IsDOMHTMLImageElement domhtmlImageElement, IsNSString value) => domhtmlImageElement -> value -> IO ()
setSrc domhtmlImageElement value =
  sendMessage domhtmlImageElement setSrcSelector (toNSString value)

-- | @- useMap@
useMap :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO (Id NSString)
useMap domhtmlImageElement =
  sendMessage domhtmlImageElement useMapSelector

-- | @- setUseMap:@
setUseMap :: (IsDOMHTMLImageElement domhtmlImageElement, IsNSString value) => domhtmlImageElement -> value -> IO ()
setUseMap domhtmlImageElement value =
  sendMessage domhtmlImageElement setUseMapSelector (toNSString value)

-- | @- vspace@
vspace :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO CInt
vspace domhtmlImageElement =
  sendMessage domhtmlImageElement vspaceSelector

-- | @- setVspace:@
setVspace :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> CInt -> IO ()
setVspace domhtmlImageElement value =
  sendMessage domhtmlImageElement setVspaceSelector value

-- | @- width@
width :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO CInt
width domhtmlImageElement =
  sendMessage domhtmlImageElement widthSelector

-- | @- setWidth:@
setWidth :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> CInt -> IO ()
setWidth domhtmlImageElement value =
  sendMessage domhtmlImageElement setWidthSelector value

-- | @- complete@
complete :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO Bool
complete domhtmlImageElement =
  sendMessage domhtmlImageElement completeSelector

-- | @- lowsrc@
lowsrc :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO (Id NSString)
lowsrc domhtmlImageElement =
  sendMessage domhtmlImageElement lowsrcSelector

-- | @- setLowsrc:@
setLowsrc :: (IsDOMHTMLImageElement domhtmlImageElement, IsNSString value) => domhtmlImageElement -> value -> IO ()
setLowsrc domhtmlImageElement value =
  sendMessage domhtmlImageElement setLowsrcSelector (toNSString value)

-- | @- naturalHeight@
naturalHeight :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO CInt
naturalHeight domhtmlImageElement =
  sendMessage domhtmlImageElement naturalHeightSelector

-- | @- naturalWidth@
naturalWidth :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO CInt
naturalWidth domhtmlImageElement =
  sendMessage domhtmlImageElement naturalWidthSelector

-- | @- x@
x :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO CInt
x domhtmlImageElement =
  sendMessage domhtmlImageElement xSelector

-- | @- y@
y :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO CInt
y domhtmlImageElement =
  sendMessage domhtmlImageElement ySelector

-- | @- altDisplayString@
altDisplayString :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO (Id NSString)
altDisplayString domhtmlImageElement =
  sendMessage domhtmlImageElement altDisplayStringSelector

-- | @- absoluteImageURL@
absoluteImageURL :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO (Id NSURL)
absoluteImageURL domhtmlImageElement =
  sendMessage domhtmlImageElement absoluteImageURLSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

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

-- | @Selector@ for @border@
borderSelector :: Selector '[] (Id NSString)
borderSelector = mkSelector "border"

-- | @Selector@ for @setBorder:@
setBorderSelector :: Selector '[Id NSString] ()
setBorderSelector = mkSelector "setBorder:"

-- | @Selector@ for @height@
heightSelector :: Selector '[] CInt
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector '[CInt] ()
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @hspace@
hspaceSelector :: Selector '[] CInt
hspaceSelector = mkSelector "hspace"

-- | @Selector@ for @setHspace:@
setHspaceSelector :: Selector '[CInt] ()
setHspaceSelector = mkSelector "setHspace:"

-- | @Selector@ for @isMap@
isMapSelector :: Selector '[] Bool
isMapSelector = mkSelector "isMap"

-- | @Selector@ for @setIsMap:@
setIsMapSelector :: Selector '[Bool] ()
setIsMapSelector = mkSelector "setIsMap:"

-- | @Selector@ for @longDesc@
longDescSelector :: Selector '[] (Id NSString)
longDescSelector = mkSelector "longDesc"

-- | @Selector@ for @setLongDesc:@
setLongDescSelector :: Selector '[Id NSString] ()
setLongDescSelector = mkSelector "setLongDesc:"

-- | @Selector@ for @src@
srcSelector :: Selector '[] (Id NSString)
srcSelector = mkSelector "src"

-- | @Selector@ for @setSrc:@
setSrcSelector :: Selector '[Id NSString] ()
setSrcSelector = mkSelector "setSrc:"

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
widthSelector :: Selector '[] CInt
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector '[CInt] ()
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @complete@
completeSelector :: Selector '[] Bool
completeSelector = mkSelector "complete"

-- | @Selector@ for @lowsrc@
lowsrcSelector :: Selector '[] (Id NSString)
lowsrcSelector = mkSelector "lowsrc"

-- | @Selector@ for @setLowsrc:@
setLowsrcSelector :: Selector '[Id NSString] ()
setLowsrcSelector = mkSelector "setLowsrc:"

-- | @Selector@ for @naturalHeight@
naturalHeightSelector :: Selector '[] CInt
naturalHeightSelector = mkSelector "naturalHeight"

-- | @Selector@ for @naturalWidth@
naturalWidthSelector :: Selector '[] CInt
naturalWidthSelector = mkSelector "naturalWidth"

-- | @Selector@ for @x@
xSelector :: Selector '[] CInt
xSelector = mkSelector "x"

-- | @Selector@ for @y@
ySelector :: Selector '[] CInt
ySelector = mkSelector "y"

-- | @Selector@ for @altDisplayString@
altDisplayStringSelector :: Selector '[] (Id NSString)
altDisplayStringSelector = mkSelector "altDisplayString"

-- | @Selector@ for @absoluteImageURL@
absoluteImageURLSelector :: Selector '[] (Id NSURL)
absoluteImageURLSelector = mkSelector "absoluteImageURL"

