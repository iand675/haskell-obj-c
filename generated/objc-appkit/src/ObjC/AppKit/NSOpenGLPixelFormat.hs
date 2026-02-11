{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSOpenGLPixelFormat@.
module ObjC.AppKit.NSOpenGLPixelFormat
  ( NSOpenGLPixelFormat
  , IsNSOpenGLPixelFormat(..)
  , initWithCGLPixelFormatObj
  , initWithData
  , attributes
  , setAttributes
  , numberOfVirtualScreens
  , cglPixelFormatObj
  , initWithCGLPixelFormatObjSelector
  , initWithDataSelector
  , attributesSelector
  , setAttributesSelector
  , numberOfVirtualScreensSelector
  , cglPixelFormatObjSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithCGLPixelFormatObj:@
initWithCGLPixelFormatObj :: IsNSOpenGLPixelFormat nsOpenGLPixelFormat => nsOpenGLPixelFormat -> Ptr () -> IO (Id NSOpenGLPixelFormat)
initWithCGLPixelFormatObj nsOpenGLPixelFormat  format =
  sendMsg nsOpenGLPixelFormat (mkSelector "initWithCGLPixelFormatObj:") (retPtr retVoid) [argPtr format] >>= ownedObject . castPtr

-- | @- initWithData:@
initWithData :: (IsNSOpenGLPixelFormat nsOpenGLPixelFormat, IsNSData attribs) => nsOpenGLPixelFormat -> attribs -> IO RawId
initWithData nsOpenGLPixelFormat  attribs =
withObjCPtr attribs $ \raw_attribs ->
    fmap (RawId . castPtr) $ sendMsg nsOpenGLPixelFormat (mkSelector "initWithData:") (retPtr retVoid) [argPtr (castPtr raw_attribs :: Ptr ())]

-- | @- attributes@
attributes :: IsNSOpenGLPixelFormat nsOpenGLPixelFormat => nsOpenGLPixelFormat -> IO (Id NSData)
attributes nsOpenGLPixelFormat  =
  sendMsg nsOpenGLPixelFormat (mkSelector "attributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributes:@
setAttributes :: (IsNSOpenGLPixelFormat nsOpenGLPixelFormat, IsNSData attribs) => nsOpenGLPixelFormat -> attribs -> IO ()
setAttributes nsOpenGLPixelFormat  attribs =
withObjCPtr attribs $ \raw_attribs ->
    sendMsg nsOpenGLPixelFormat (mkSelector "setAttributes:") retVoid [argPtr (castPtr raw_attribs :: Ptr ())]

-- | @- numberOfVirtualScreens@
numberOfVirtualScreens :: IsNSOpenGLPixelFormat nsOpenGLPixelFormat => nsOpenGLPixelFormat -> IO CInt
numberOfVirtualScreens nsOpenGLPixelFormat  =
  sendMsg nsOpenGLPixelFormat (mkSelector "numberOfVirtualScreens") retCInt []

-- | @- CGLPixelFormatObj@
cglPixelFormatObj :: IsNSOpenGLPixelFormat nsOpenGLPixelFormat => nsOpenGLPixelFormat -> IO (Ptr ())
cglPixelFormatObj nsOpenGLPixelFormat  =
  fmap castPtr $ sendMsg nsOpenGLPixelFormat (mkSelector "CGLPixelFormatObj") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithCGLPixelFormatObj:@
initWithCGLPixelFormatObjSelector :: Selector
initWithCGLPixelFormatObjSelector = mkSelector "initWithCGLPixelFormatObj:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @attributes@
attributesSelector :: Selector
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @setAttributes:@
setAttributesSelector :: Selector
setAttributesSelector = mkSelector "setAttributes:"

-- | @Selector@ for @numberOfVirtualScreens@
numberOfVirtualScreensSelector :: Selector
numberOfVirtualScreensSelector = mkSelector "numberOfVirtualScreens"

-- | @Selector@ for @CGLPixelFormatObj@
cglPixelFormatObjSelector :: Selector
cglPixelFormatObjSelector = mkSelector "CGLPixelFormatObj"

