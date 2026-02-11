{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPDFInfo@.
module ObjC.AppKit.NSPDFInfo
  ( NSPDFInfo
  , IsNSPDFInfo(..)
  , url
  , setURL
  , fileExtensionHidden
  , setFileExtensionHidden
  , tagNames
  , setTagNames
  , orientation
  , setOrientation
  , paperSize
  , setPaperSize
  , attributes
  , urlSelector
  , setURLSelector
  , fileExtensionHiddenSelector
  , setFileExtensionHiddenSelector
  , tagNamesSelector
  , setTagNamesSelector
  , orientationSelector
  , setOrientationSelector
  , paperSizeSelector
  , setPaperSizeSelector
  , attributesSelector

  -- * Enum types
  , NSPaperOrientation(NSPaperOrientation)
  , pattern NSPaperOrientationPortrait
  , pattern NSPaperOrientationLandscape

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- URL@
url :: IsNSPDFInfo nspdfInfo => nspdfInfo -> IO (Id NSURL)
url nspdfInfo  =
  sendMsg nspdfInfo (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setURL:@
setURL :: (IsNSPDFInfo nspdfInfo, IsNSURL value) => nspdfInfo -> value -> IO ()
setURL nspdfInfo  value =
withObjCPtr value $ \raw_value ->
    sendMsg nspdfInfo (mkSelector "setURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fileExtensionHidden@
fileExtensionHidden :: IsNSPDFInfo nspdfInfo => nspdfInfo -> IO Bool
fileExtensionHidden nspdfInfo  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nspdfInfo (mkSelector "fileExtensionHidden") retCULong []

-- | @- setFileExtensionHidden:@
setFileExtensionHidden :: IsNSPDFInfo nspdfInfo => nspdfInfo -> Bool -> IO ()
setFileExtensionHidden nspdfInfo  value =
  sendMsg nspdfInfo (mkSelector "setFileExtensionHidden:") retVoid [argCULong (if value then 1 else 0)]

-- | @- tagNames@
tagNames :: IsNSPDFInfo nspdfInfo => nspdfInfo -> IO (Id NSArray)
tagNames nspdfInfo  =
  sendMsg nspdfInfo (mkSelector "tagNames") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTagNames:@
setTagNames :: (IsNSPDFInfo nspdfInfo, IsNSArray value) => nspdfInfo -> value -> IO ()
setTagNames nspdfInfo  value =
withObjCPtr value $ \raw_value ->
    sendMsg nspdfInfo (mkSelector "setTagNames:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- orientation@
orientation :: IsNSPDFInfo nspdfInfo => nspdfInfo -> IO NSPaperOrientation
orientation nspdfInfo  =
  fmap (coerce :: CLong -> NSPaperOrientation) $ sendMsg nspdfInfo (mkSelector "orientation") retCLong []

-- | @- setOrientation:@
setOrientation :: IsNSPDFInfo nspdfInfo => nspdfInfo -> NSPaperOrientation -> IO ()
setOrientation nspdfInfo  value =
  sendMsg nspdfInfo (mkSelector "setOrientation:") retVoid [argCLong (coerce value)]

-- | @- paperSize@
paperSize :: IsNSPDFInfo nspdfInfo => nspdfInfo -> IO NSSize
paperSize nspdfInfo  =
  sendMsgStret nspdfInfo (mkSelector "paperSize") retNSSize []

-- | @- setPaperSize:@
setPaperSize :: IsNSPDFInfo nspdfInfo => nspdfInfo -> NSSize -> IO ()
setPaperSize nspdfInfo  value =
  sendMsg nspdfInfo (mkSelector "setPaperSize:") retVoid [argNSSize value]

-- | @- attributes@
attributes :: IsNSPDFInfo nspdfInfo => nspdfInfo -> IO (Id NSMutableDictionary)
attributes nspdfInfo  =
  sendMsg nspdfInfo (mkSelector "attributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector
setURLSelector = mkSelector "setURL:"

-- | @Selector@ for @fileExtensionHidden@
fileExtensionHiddenSelector :: Selector
fileExtensionHiddenSelector = mkSelector "fileExtensionHidden"

-- | @Selector@ for @setFileExtensionHidden:@
setFileExtensionHiddenSelector :: Selector
setFileExtensionHiddenSelector = mkSelector "setFileExtensionHidden:"

-- | @Selector@ for @tagNames@
tagNamesSelector :: Selector
tagNamesSelector = mkSelector "tagNames"

-- | @Selector@ for @setTagNames:@
setTagNamesSelector :: Selector
setTagNamesSelector = mkSelector "setTagNames:"

-- | @Selector@ for @orientation@
orientationSelector :: Selector
orientationSelector = mkSelector "orientation"

-- | @Selector@ for @setOrientation:@
setOrientationSelector :: Selector
setOrientationSelector = mkSelector "setOrientation:"

-- | @Selector@ for @paperSize@
paperSizeSelector :: Selector
paperSizeSelector = mkSelector "paperSize"

-- | @Selector@ for @setPaperSize:@
setPaperSizeSelector :: Selector
setPaperSizeSelector = mkSelector "setPaperSize:"

-- | @Selector@ for @attributes@
attributesSelector :: Selector
attributesSelector = mkSelector "attributes"

