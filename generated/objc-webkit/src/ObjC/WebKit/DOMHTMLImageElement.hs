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
  , nameSelector
  , setNameSelector
  , alignSelector
  , setAlignSelector
  , altSelector
  , setAltSelector
  , borderSelector
  , setBorderSelector
  , heightSelector
  , setHeightSelector
  , hspaceSelector
  , setHspaceSelector
  , isMapSelector
  , setIsMapSelector
  , longDescSelector
  , setLongDescSelector
  , srcSelector
  , setSrcSelector
  , useMapSelector
  , setUseMapSelector
  , vspaceSelector
  , setVspaceSelector
  , widthSelector
  , setWidthSelector
  , completeSelector
  , lowsrcSelector
  , setLowsrcSelector
  , naturalHeightSelector
  , naturalWidthSelector
  , xSelector
  , ySelector
  , altDisplayStringSelector
  , absoluteImageURLSelector


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

-- | @- name@
name :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO (Id NSString)
name domhtmlImageElement  =
  sendMsg domhtmlImageElement (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsDOMHTMLImageElement domhtmlImageElement, IsNSString value) => domhtmlImageElement -> value -> IO ()
setName domhtmlImageElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlImageElement (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- align@
align :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO (Id NSString)
align domhtmlImageElement  =
  sendMsg domhtmlImageElement (mkSelector "align") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlign:@
setAlign :: (IsDOMHTMLImageElement domhtmlImageElement, IsNSString value) => domhtmlImageElement -> value -> IO ()
setAlign domhtmlImageElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlImageElement (mkSelector "setAlign:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- alt@
alt :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO (Id NSString)
alt domhtmlImageElement  =
  sendMsg domhtmlImageElement (mkSelector "alt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlt:@
setAlt :: (IsDOMHTMLImageElement domhtmlImageElement, IsNSString value) => domhtmlImageElement -> value -> IO ()
setAlt domhtmlImageElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlImageElement (mkSelector "setAlt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- border@
border :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO (Id NSString)
border domhtmlImageElement  =
  sendMsg domhtmlImageElement (mkSelector "border") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBorder:@
setBorder :: (IsDOMHTMLImageElement domhtmlImageElement, IsNSString value) => domhtmlImageElement -> value -> IO ()
setBorder domhtmlImageElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlImageElement (mkSelector "setBorder:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- height@
height :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO CInt
height domhtmlImageElement  =
  sendMsg domhtmlImageElement (mkSelector "height") retCInt []

-- | @- setHeight:@
setHeight :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> CInt -> IO ()
setHeight domhtmlImageElement  value =
  sendMsg domhtmlImageElement (mkSelector "setHeight:") retVoid [argCInt (fromIntegral value)]

-- | @- hspace@
hspace :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO CInt
hspace domhtmlImageElement  =
  sendMsg domhtmlImageElement (mkSelector "hspace") retCInt []

-- | @- setHspace:@
setHspace :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> CInt -> IO ()
setHspace domhtmlImageElement  value =
  sendMsg domhtmlImageElement (mkSelector "setHspace:") retVoid [argCInt (fromIntegral value)]

-- | @- isMap@
isMap :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO Bool
isMap domhtmlImageElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlImageElement (mkSelector "isMap") retCULong []

-- | @- setIsMap:@
setIsMap :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> Bool -> IO ()
setIsMap domhtmlImageElement  value =
  sendMsg domhtmlImageElement (mkSelector "setIsMap:") retVoid [argCULong (if value then 1 else 0)]

-- | @- longDesc@
longDesc :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO (Id NSString)
longDesc domhtmlImageElement  =
  sendMsg domhtmlImageElement (mkSelector "longDesc") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLongDesc:@
setLongDesc :: (IsDOMHTMLImageElement domhtmlImageElement, IsNSString value) => domhtmlImageElement -> value -> IO ()
setLongDesc domhtmlImageElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlImageElement (mkSelector "setLongDesc:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- src@
src :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO (Id NSString)
src domhtmlImageElement  =
  sendMsg domhtmlImageElement (mkSelector "src") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSrc:@
setSrc :: (IsDOMHTMLImageElement domhtmlImageElement, IsNSString value) => domhtmlImageElement -> value -> IO ()
setSrc domhtmlImageElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlImageElement (mkSelector "setSrc:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- useMap@
useMap :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO (Id NSString)
useMap domhtmlImageElement  =
  sendMsg domhtmlImageElement (mkSelector "useMap") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUseMap:@
setUseMap :: (IsDOMHTMLImageElement domhtmlImageElement, IsNSString value) => domhtmlImageElement -> value -> IO ()
setUseMap domhtmlImageElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlImageElement (mkSelector "setUseMap:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- vspace@
vspace :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO CInt
vspace domhtmlImageElement  =
  sendMsg domhtmlImageElement (mkSelector "vspace") retCInt []

-- | @- setVspace:@
setVspace :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> CInt -> IO ()
setVspace domhtmlImageElement  value =
  sendMsg domhtmlImageElement (mkSelector "setVspace:") retVoid [argCInt (fromIntegral value)]

-- | @- width@
width :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO CInt
width domhtmlImageElement  =
  sendMsg domhtmlImageElement (mkSelector "width") retCInt []

-- | @- setWidth:@
setWidth :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> CInt -> IO ()
setWidth domhtmlImageElement  value =
  sendMsg domhtmlImageElement (mkSelector "setWidth:") retVoid [argCInt (fromIntegral value)]

-- | @- complete@
complete :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO Bool
complete domhtmlImageElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domhtmlImageElement (mkSelector "complete") retCULong []

-- | @- lowsrc@
lowsrc :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO (Id NSString)
lowsrc domhtmlImageElement  =
  sendMsg domhtmlImageElement (mkSelector "lowsrc") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLowsrc:@
setLowsrc :: (IsDOMHTMLImageElement domhtmlImageElement, IsNSString value) => domhtmlImageElement -> value -> IO ()
setLowsrc domhtmlImageElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg domhtmlImageElement (mkSelector "setLowsrc:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- naturalHeight@
naturalHeight :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO CInt
naturalHeight domhtmlImageElement  =
  sendMsg domhtmlImageElement (mkSelector "naturalHeight") retCInt []

-- | @- naturalWidth@
naturalWidth :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO CInt
naturalWidth domhtmlImageElement  =
  sendMsg domhtmlImageElement (mkSelector "naturalWidth") retCInt []

-- | @- x@
x :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO CInt
x domhtmlImageElement  =
  sendMsg domhtmlImageElement (mkSelector "x") retCInt []

-- | @- y@
y :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO CInt
y domhtmlImageElement  =
  sendMsg domhtmlImageElement (mkSelector "y") retCInt []

-- | @- altDisplayString@
altDisplayString :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO (Id NSString)
altDisplayString domhtmlImageElement  =
  sendMsg domhtmlImageElement (mkSelector "altDisplayString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- absoluteImageURL@
absoluteImageURL :: IsDOMHTMLImageElement domhtmlImageElement => domhtmlImageElement -> IO (Id NSURL)
absoluteImageURL domhtmlImageElement  =
  sendMsg domhtmlImageElement (mkSelector "absoluteImageURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

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

-- | @Selector@ for @border@
borderSelector :: Selector
borderSelector = mkSelector "border"

-- | @Selector@ for @setBorder:@
setBorderSelector :: Selector
setBorderSelector = mkSelector "setBorder:"

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

-- | @Selector@ for @isMap@
isMapSelector :: Selector
isMapSelector = mkSelector "isMap"

-- | @Selector@ for @setIsMap:@
setIsMapSelector :: Selector
setIsMapSelector = mkSelector "setIsMap:"

-- | @Selector@ for @longDesc@
longDescSelector :: Selector
longDescSelector = mkSelector "longDesc"

-- | @Selector@ for @setLongDesc:@
setLongDescSelector :: Selector
setLongDescSelector = mkSelector "setLongDesc:"

-- | @Selector@ for @src@
srcSelector :: Selector
srcSelector = mkSelector "src"

-- | @Selector@ for @setSrc:@
setSrcSelector :: Selector
setSrcSelector = mkSelector "setSrc:"

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

-- | @Selector@ for @complete@
completeSelector :: Selector
completeSelector = mkSelector "complete"

-- | @Selector@ for @lowsrc@
lowsrcSelector :: Selector
lowsrcSelector = mkSelector "lowsrc"

-- | @Selector@ for @setLowsrc:@
setLowsrcSelector :: Selector
setLowsrcSelector = mkSelector "setLowsrc:"

-- | @Selector@ for @naturalHeight@
naturalHeightSelector :: Selector
naturalHeightSelector = mkSelector "naturalHeight"

-- | @Selector@ for @naturalWidth@
naturalWidthSelector :: Selector
naturalWidthSelector = mkSelector "naturalWidth"

-- | @Selector@ for @x@
xSelector :: Selector
xSelector = mkSelector "x"

-- | @Selector@ for @y@
ySelector :: Selector
ySelector = mkSelector "y"

-- | @Selector@ for @altDisplayString@
altDisplayStringSelector :: Selector
altDisplayStringSelector = mkSelector "altDisplayString"

-- | @Selector@ for @absoluteImageURL@
absoluteImageURLSelector :: Selector
absoluteImageURLSelector = mkSelector "absoluteImageURL"

