{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTableCellView@.
module ObjC.AppKit.NSTableCellView
  ( NSTableCellView
  , IsNSTableCellView(..)
  , objectValue
  , setObjectValue
  , textField
  , setTextField
  , imageView
  , setImageView
  , backgroundStyle
  , setBackgroundStyle
  , rowSizeStyle
  , setRowSizeStyle
  , draggingImageComponents
  , objectValueSelector
  , setObjectValueSelector
  , textFieldSelector
  , setTextFieldSelector
  , imageViewSelector
  , setImageViewSelector
  , backgroundStyleSelector
  , setBackgroundStyleSelector
  , rowSizeStyleSelector
  , setRowSizeStyleSelector
  , draggingImageComponentsSelector

  -- * Enum types
  , NSBackgroundStyle(NSBackgroundStyle)
  , pattern NSBackgroundStyleNormal
  , pattern NSBackgroundStyleEmphasized
  , pattern NSBackgroundStyleRaised
  , pattern NSBackgroundStyleLowered
  , NSTableViewRowSizeStyle(NSTableViewRowSizeStyle)
  , pattern NSTableViewRowSizeStyleDefault
  , pattern NSTableViewRowSizeStyleCustom
  , pattern NSTableViewRowSizeStyleSmall
  , pattern NSTableViewRowSizeStyleMedium
  , pattern NSTableViewRowSizeStyleLarge

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- objectValue@
objectValue :: IsNSTableCellView nsTableCellView => nsTableCellView -> IO RawId
objectValue nsTableCellView  =
  fmap (RawId . castPtr) $ sendMsg nsTableCellView (mkSelector "objectValue") (retPtr retVoid) []

-- | @- setObjectValue:@
setObjectValue :: IsNSTableCellView nsTableCellView => nsTableCellView -> RawId -> IO ()
setObjectValue nsTableCellView  value =
  sendMsg nsTableCellView (mkSelector "setObjectValue:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- textField@
textField :: IsNSTableCellView nsTableCellView => nsTableCellView -> IO (Id NSTextField)
textField nsTableCellView  =
  sendMsg nsTableCellView (mkSelector "textField") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextField:@
setTextField :: (IsNSTableCellView nsTableCellView, IsNSTextField value) => nsTableCellView -> value -> IO ()
setTextField nsTableCellView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTableCellView (mkSelector "setTextField:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- imageView@
imageView :: IsNSTableCellView nsTableCellView => nsTableCellView -> IO (Id NSImageView)
imageView nsTableCellView  =
  sendMsg nsTableCellView (mkSelector "imageView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImageView:@
setImageView :: (IsNSTableCellView nsTableCellView, IsNSImageView value) => nsTableCellView -> value -> IO ()
setImageView nsTableCellView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTableCellView (mkSelector "setImageView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- backgroundStyle@
backgroundStyle :: IsNSTableCellView nsTableCellView => nsTableCellView -> IO NSBackgroundStyle
backgroundStyle nsTableCellView  =
  fmap (coerce :: CLong -> NSBackgroundStyle) $ sendMsg nsTableCellView (mkSelector "backgroundStyle") retCLong []

-- | @- setBackgroundStyle:@
setBackgroundStyle :: IsNSTableCellView nsTableCellView => nsTableCellView -> NSBackgroundStyle -> IO ()
setBackgroundStyle nsTableCellView  value =
  sendMsg nsTableCellView (mkSelector "setBackgroundStyle:") retVoid [argCLong (coerce value)]

-- | @- rowSizeStyle@
rowSizeStyle :: IsNSTableCellView nsTableCellView => nsTableCellView -> IO NSTableViewRowSizeStyle
rowSizeStyle nsTableCellView  =
  fmap (coerce :: CLong -> NSTableViewRowSizeStyle) $ sendMsg nsTableCellView (mkSelector "rowSizeStyle") retCLong []

-- | @- setRowSizeStyle:@
setRowSizeStyle :: IsNSTableCellView nsTableCellView => nsTableCellView -> NSTableViewRowSizeStyle -> IO ()
setRowSizeStyle nsTableCellView  value =
  sendMsg nsTableCellView (mkSelector "setRowSizeStyle:") retVoid [argCLong (coerce value)]

-- | @- draggingImageComponents@
draggingImageComponents :: IsNSTableCellView nsTableCellView => nsTableCellView -> IO (Id NSArray)
draggingImageComponents nsTableCellView  =
  sendMsg nsTableCellView (mkSelector "draggingImageComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectValue@
objectValueSelector :: Selector
objectValueSelector = mkSelector "objectValue"

-- | @Selector@ for @setObjectValue:@
setObjectValueSelector :: Selector
setObjectValueSelector = mkSelector "setObjectValue:"

-- | @Selector@ for @textField@
textFieldSelector :: Selector
textFieldSelector = mkSelector "textField"

-- | @Selector@ for @setTextField:@
setTextFieldSelector :: Selector
setTextFieldSelector = mkSelector "setTextField:"

-- | @Selector@ for @imageView@
imageViewSelector :: Selector
imageViewSelector = mkSelector "imageView"

-- | @Selector@ for @setImageView:@
setImageViewSelector :: Selector
setImageViewSelector = mkSelector "setImageView:"

-- | @Selector@ for @backgroundStyle@
backgroundStyleSelector :: Selector
backgroundStyleSelector = mkSelector "backgroundStyle"

-- | @Selector@ for @setBackgroundStyle:@
setBackgroundStyleSelector :: Selector
setBackgroundStyleSelector = mkSelector "setBackgroundStyle:"

-- | @Selector@ for @rowSizeStyle@
rowSizeStyleSelector :: Selector
rowSizeStyleSelector = mkSelector "rowSizeStyle"

-- | @Selector@ for @setRowSizeStyle:@
setRowSizeStyleSelector :: Selector
setRowSizeStyleSelector = mkSelector "setRowSizeStyle:"

-- | @Selector@ for @draggingImageComponents@
draggingImageComponentsSelector :: Selector
draggingImageComponentsSelector = mkSelector "draggingImageComponents"

