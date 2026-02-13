{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSOpenGLPixelFormat@.
module ObjC.AppKit.NSOpenGLPixelFormat
  ( NSOpenGLPixelFormat
  , IsNSOpenGLPixelFormat(..)
  , initWithCGLPixelFormatObj
  , initWithAttributes
  , initWithData
  , attributes
  , setAttributes
  , getValues_forAttribute_forVirtualScreen
  , numberOfVirtualScreens
  , cglPixelFormatObj
  , attributesSelector
  , cglPixelFormatObjSelector
  , getValues_forAttribute_forVirtualScreenSelector
  , initWithAttributesSelector
  , initWithCGLPixelFormatObjSelector
  , initWithDataSelector
  , numberOfVirtualScreensSelector
  , setAttributesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCGLPixelFormatObj:@
initWithCGLPixelFormatObj :: IsNSOpenGLPixelFormat nsOpenGLPixelFormat => nsOpenGLPixelFormat -> Ptr () -> IO (Id NSOpenGLPixelFormat)
initWithCGLPixelFormatObj nsOpenGLPixelFormat format =
  sendOwnedMessage nsOpenGLPixelFormat initWithCGLPixelFormatObjSelector format

-- | @- initWithAttributes:@
initWithAttributes :: IsNSOpenGLPixelFormat nsOpenGLPixelFormat => nsOpenGLPixelFormat -> Const RawId -> IO (Id NSOpenGLPixelFormat)
initWithAttributes nsOpenGLPixelFormat attribs =
  sendOwnedMessage nsOpenGLPixelFormat initWithAttributesSelector attribs

-- | @- initWithData:@
initWithData :: (IsNSOpenGLPixelFormat nsOpenGLPixelFormat, IsNSData attribs) => nsOpenGLPixelFormat -> attribs -> IO RawId
initWithData nsOpenGLPixelFormat attribs =
  sendOwnedMessage nsOpenGLPixelFormat initWithDataSelector (toNSData attribs)

-- | @- attributes@
attributes :: IsNSOpenGLPixelFormat nsOpenGLPixelFormat => nsOpenGLPixelFormat -> IO (Id NSData)
attributes nsOpenGLPixelFormat =
  sendMessage nsOpenGLPixelFormat attributesSelector

-- | @- setAttributes:@
setAttributes :: (IsNSOpenGLPixelFormat nsOpenGLPixelFormat, IsNSData attribs) => nsOpenGLPixelFormat -> attribs -> IO ()
setAttributes nsOpenGLPixelFormat attribs =
  sendMessage nsOpenGLPixelFormat setAttributesSelector (toNSData attribs)

-- | @- getValues:forAttribute:forVirtualScreen:@
getValues_forAttribute_forVirtualScreen :: IsNSOpenGLPixelFormat nsOpenGLPixelFormat => nsOpenGLPixelFormat -> RawId -> CUInt -> CInt -> IO ()
getValues_forAttribute_forVirtualScreen nsOpenGLPixelFormat vals attrib screen =
  sendMessage nsOpenGLPixelFormat getValues_forAttribute_forVirtualScreenSelector vals attrib screen

-- | @- numberOfVirtualScreens@
numberOfVirtualScreens :: IsNSOpenGLPixelFormat nsOpenGLPixelFormat => nsOpenGLPixelFormat -> IO CInt
numberOfVirtualScreens nsOpenGLPixelFormat =
  sendMessage nsOpenGLPixelFormat numberOfVirtualScreensSelector

-- | @- CGLPixelFormatObj@
cglPixelFormatObj :: IsNSOpenGLPixelFormat nsOpenGLPixelFormat => nsOpenGLPixelFormat -> IO (Ptr ())
cglPixelFormatObj nsOpenGLPixelFormat =
  sendMessage nsOpenGLPixelFormat cglPixelFormatObjSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCGLPixelFormatObj:@
initWithCGLPixelFormatObjSelector :: Selector '[Ptr ()] (Id NSOpenGLPixelFormat)
initWithCGLPixelFormatObjSelector = mkSelector "initWithCGLPixelFormatObj:"

-- | @Selector@ for @initWithAttributes:@
initWithAttributesSelector :: Selector '[Const RawId] (Id NSOpenGLPixelFormat)
initWithAttributesSelector = mkSelector "initWithAttributes:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector '[Id NSData] RawId
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @attributes@
attributesSelector :: Selector '[] (Id NSData)
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @setAttributes:@
setAttributesSelector :: Selector '[Id NSData] ()
setAttributesSelector = mkSelector "setAttributes:"

-- | @Selector@ for @getValues:forAttribute:forVirtualScreen:@
getValues_forAttribute_forVirtualScreenSelector :: Selector '[RawId, CUInt, CInt] ()
getValues_forAttribute_forVirtualScreenSelector = mkSelector "getValues:forAttribute:forVirtualScreen:"

-- | @Selector@ for @numberOfVirtualScreens@
numberOfVirtualScreensSelector :: Selector '[] CInt
numberOfVirtualScreensSelector = mkSelector "numberOfVirtualScreens"

-- | @Selector@ for @CGLPixelFormatObj@
cglPixelFormatObjSelector :: Selector '[] (Ptr ())
cglPixelFormatObjSelector = mkSelector "CGLPixelFormatObj"

