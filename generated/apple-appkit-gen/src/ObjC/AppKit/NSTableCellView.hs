{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , backgroundStyleSelector
  , draggingImageComponentsSelector
  , imageViewSelector
  , objectValueSelector
  , rowSizeStyleSelector
  , setBackgroundStyleSelector
  , setImageViewSelector
  , setObjectValueSelector
  , setRowSizeStyleSelector
  , setTextFieldSelector
  , textFieldSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- objectValue@
objectValue :: IsNSTableCellView nsTableCellView => nsTableCellView -> IO RawId
objectValue nsTableCellView =
  sendMessage nsTableCellView objectValueSelector

-- | @- setObjectValue:@
setObjectValue :: IsNSTableCellView nsTableCellView => nsTableCellView -> RawId -> IO ()
setObjectValue nsTableCellView value =
  sendMessage nsTableCellView setObjectValueSelector value

-- | @- textField@
textField :: IsNSTableCellView nsTableCellView => nsTableCellView -> IO (Id NSTextField)
textField nsTableCellView =
  sendMessage nsTableCellView textFieldSelector

-- | @- setTextField:@
setTextField :: (IsNSTableCellView nsTableCellView, IsNSTextField value) => nsTableCellView -> value -> IO ()
setTextField nsTableCellView value =
  sendMessage nsTableCellView setTextFieldSelector (toNSTextField value)

-- | @- imageView@
imageView :: IsNSTableCellView nsTableCellView => nsTableCellView -> IO (Id NSImageView)
imageView nsTableCellView =
  sendMessage nsTableCellView imageViewSelector

-- | @- setImageView:@
setImageView :: (IsNSTableCellView nsTableCellView, IsNSImageView value) => nsTableCellView -> value -> IO ()
setImageView nsTableCellView value =
  sendMessage nsTableCellView setImageViewSelector (toNSImageView value)

-- | @- backgroundStyle@
backgroundStyle :: IsNSTableCellView nsTableCellView => nsTableCellView -> IO NSBackgroundStyle
backgroundStyle nsTableCellView =
  sendMessage nsTableCellView backgroundStyleSelector

-- | @- setBackgroundStyle:@
setBackgroundStyle :: IsNSTableCellView nsTableCellView => nsTableCellView -> NSBackgroundStyle -> IO ()
setBackgroundStyle nsTableCellView value =
  sendMessage nsTableCellView setBackgroundStyleSelector value

-- | @- rowSizeStyle@
rowSizeStyle :: IsNSTableCellView nsTableCellView => nsTableCellView -> IO NSTableViewRowSizeStyle
rowSizeStyle nsTableCellView =
  sendMessage nsTableCellView rowSizeStyleSelector

-- | @- setRowSizeStyle:@
setRowSizeStyle :: IsNSTableCellView nsTableCellView => nsTableCellView -> NSTableViewRowSizeStyle -> IO ()
setRowSizeStyle nsTableCellView value =
  sendMessage nsTableCellView setRowSizeStyleSelector value

-- | @- draggingImageComponents@
draggingImageComponents :: IsNSTableCellView nsTableCellView => nsTableCellView -> IO (Id NSArray)
draggingImageComponents nsTableCellView =
  sendMessage nsTableCellView draggingImageComponentsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectValue@
objectValueSelector :: Selector '[] RawId
objectValueSelector = mkSelector "objectValue"

-- | @Selector@ for @setObjectValue:@
setObjectValueSelector :: Selector '[RawId] ()
setObjectValueSelector = mkSelector "setObjectValue:"

-- | @Selector@ for @textField@
textFieldSelector :: Selector '[] (Id NSTextField)
textFieldSelector = mkSelector "textField"

-- | @Selector@ for @setTextField:@
setTextFieldSelector :: Selector '[Id NSTextField] ()
setTextFieldSelector = mkSelector "setTextField:"

-- | @Selector@ for @imageView@
imageViewSelector :: Selector '[] (Id NSImageView)
imageViewSelector = mkSelector "imageView"

-- | @Selector@ for @setImageView:@
setImageViewSelector :: Selector '[Id NSImageView] ()
setImageViewSelector = mkSelector "setImageView:"

-- | @Selector@ for @backgroundStyle@
backgroundStyleSelector :: Selector '[] NSBackgroundStyle
backgroundStyleSelector = mkSelector "backgroundStyle"

-- | @Selector@ for @setBackgroundStyle:@
setBackgroundStyleSelector :: Selector '[NSBackgroundStyle] ()
setBackgroundStyleSelector = mkSelector "setBackgroundStyle:"

-- | @Selector@ for @rowSizeStyle@
rowSizeStyleSelector :: Selector '[] NSTableViewRowSizeStyle
rowSizeStyleSelector = mkSelector "rowSizeStyle"

-- | @Selector@ for @setRowSizeStyle:@
setRowSizeStyleSelector :: Selector '[NSTableViewRowSizeStyle] ()
setRowSizeStyleSelector = mkSelector "setRowSizeStyle:"

-- | @Selector@ for @draggingImageComponents@
draggingImageComponentsSelector :: Selector '[] (Id NSArray)
draggingImageComponentsSelector = mkSelector "draggingImageComponents"

