{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , attributesSelector
  , fileExtensionHiddenSelector
  , orientationSelector
  , paperSizeSelector
  , setFileExtensionHiddenSelector
  , setOrientationSelector
  , setPaperSizeSelector
  , setTagNamesSelector
  , setURLSelector
  , tagNamesSelector
  , urlSelector

  -- * Enum types
  , NSPaperOrientation(NSPaperOrientation)
  , pattern NSPaperOrientationPortrait
  , pattern NSPaperOrientationLandscape

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- URL@
url :: IsNSPDFInfo nspdfInfo => nspdfInfo -> IO (Id NSURL)
url nspdfInfo =
  sendMessage nspdfInfo urlSelector

-- | @- setURL:@
setURL :: (IsNSPDFInfo nspdfInfo, IsNSURL value) => nspdfInfo -> value -> IO ()
setURL nspdfInfo value =
  sendMessage nspdfInfo setURLSelector (toNSURL value)

-- | @- fileExtensionHidden@
fileExtensionHidden :: IsNSPDFInfo nspdfInfo => nspdfInfo -> IO Bool
fileExtensionHidden nspdfInfo =
  sendMessage nspdfInfo fileExtensionHiddenSelector

-- | @- setFileExtensionHidden:@
setFileExtensionHidden :: IsNSPDFInfo nspdfInfo => nspdfInfo -> Bool -> IO ()
setFileExtensionHidden nspdfInfo value =
  sendMessage nspdfInfo setFileExtensionHiddenSelector value

-- | @- tagNames@
tagNames :: IsNSPDFInfo nspdfInfo => nspdfInfo -> IO (Id NSArray)
tagNames nspdfInfo =
  sendMessage nspdfInfo tagNamesSelector

-- | @- setTagNames:@
setTagNames :: (IsNSPDFInfo nspdfInfo, IsNSArray value) => nspdfInfo -> value -> IO ()
setTagNames nspdfInfo value =
  sendMessage nspdfInfo setTagNamesSelector (toNSArray value)

-- | @- orientation@
orientation :: IsNSPDFInfo nspdfInfo => nspdfInfo -> IO NSPaperOrientation
orientation nspdfInfo =
  sendMessage nspdfInfo orientationSelector

-- | @- setOrientation:@
setOrientation :: IsNSPDFInfo nspdfInfo => nspdfInfo -> NSPaperOrientation -> IO ()
setOrientation nspdfInfo value =
  sendMessage nspdfInfo setOrientationSelector value

-- | @- paperSize@
paperSize :: IsNSPDFInfo nspdfInfo => nspdfInfo -> IO NSSize
paperSize nspdfInfo =
  sendMessage nspdfInfo paperSizeSelector

-- | @- setPaperSize:@
setPaperSize :: IsNSPDFInfo nspdfInfo => nspdfInfo -> NSSize -> IO ()
setPaperSize nspdfInfo value =
  sendMessage nspdfInfo setPaperSizeSelector value

-- | @- attributes@
attributes :: IsNSPDFInfo nspdfInfo => nspdfInfo -> IO (Id NSMutableDictionary)
attributes nspdfInfo =
  sendMessage nspdfInfo attributesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector '[Id NSURL] ()
setURLSelector = mkSelector "setURL:"

-- | @Selector@ for @fileExtensionHidden@
fileExtensionHiddenSelector :: Selector '[] Bool
fileExtensionHiddenSelector = mkSelector "fileExtensionHidden"

-- | @Selector@ for @setFileExtensionHidden:@
setFileExtensionHiddenSelector :: Selector '[Bool] ()
setFileExtensionHiddenSelector = mkSelector "setFileExtensionHidden:"

-- | @Selector@ for @tagNames@
tagNamesSelector :: Selector '[] (Id NSArray)
tagNamesSelector = mkSelector "tagNames"

-- | @Selector@ for @setTagNames:@
setTagNamesSelector :: Selector '[Id NSArray] ()
setTagNamesSelector = mkSelector "setTagNames:"

-- | @Selector@ for @orientation@
orientationSelector :: Selector '[] NSPaperOrientation
orientationSelector = mkSelector "orientation"

-- | @Selector@ for @setOrientation:@
setOrientationSelector :: Selector '[NSPaperOrientation] ()
setOrientationSelector = mkSelector "setOrientation:"

-- | @Selector@ for @paperSize@
paperSizeSelector :: Selector '[] NSSize
paperSizeSelector = mkSelector "paperSize"

-- | @Selector@ for @setPaperSize:@
setPaperSizeSelector :: Selector '[NSSize] ()
setPaperSizeSelector = mkSelector "setPaperSize:"

-- | @Selector@ for @attributes@
attributesSelector :: Selector '[] (Id NSMutableDictionary)
attributesSelector = mkSelector "attributes"

