{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSCollectionViewItem@.
module ObjC.AppKit.NSCollectionViewItem
  ( NSCollectionViewItem
  , IsNSCollectionViewItem(..)
  , collectionView
  , selected
  , setSelected
  , highlightState
  , setHighlightState
  , imageView
  , setImageView
  , textField
  , setTextField
  , draggingImageComponents
  , collectionViewSelector
  , selectedSelector
  , setSelectedSelector
  , highlightStateSelector
  , setHighlightStateSelector
  , imageViewSelector
  , setImageViewSelector
  , textFieldSelector
  , setTextFieldSelector
  , draggingImageComponentsSelector

  -- * Enum types
  , NSCollectionViewItemHighlightState(NSCollectionViewItemHighlightState)
  , pattern NSCollectionViewItemHighlightNone
  , pattern NSCollectionViewItemHighlightForSelection
  , pattern NSCollectionViewItemHighlightForDeselection
  , pattern NSCollectionViewItemHighlightAsDropTarget

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

-- | @- collectionView@
collectionView :: IsNSCollectionViewItem nsCollectionViewItem => nsCollectionViewItem -> IO (Id NSCollectionView)
collectionView nsCollectionViewItem  =
    sendMsg nsCollectionViewItem (mkSelector "collectionView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- selected@
selected :: IsNSCollectionViewItem nsCollectionViewItem => nsCollectionViewItem -> IO Bool
selected nsCollectionViewItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsCollectionViewItem (mkSelector "selected") retCULong []

-- | @- setSelected:@
setSelected :: IsNSCollectionViewItem nsCollectionViewItem => nsCollectionViewItem -> Bool -> IO ()
setSelected nsCollectionViewItem  value =
    sendMsg nsCollectionViewItem (mkSelector "setSelected:") retVoid [argCULong (if value then 1 else 0)]

-- | @- highlightState@
highlightState :: IsNSCollectionViewItem nsCollectionViewItem => nsCollectionViewItem -> IO NSCollectionViewItemHighlightState
highlightState nsCollectionViewItem  =
    fmap (coerce :: CLong -> NSCollectionViewItemHighlightState) $ sendMsg nsCollectionViewItem (mkSelector "highlightState") retCLong []

-- | @- setHighlightState:@
setHighlightState :: IsNSCollectionViewItem nsCollectionViewItem => nsCollectionViewItem -> NSCollectionViewItemHighlightState -> IO ()
setHighlightState nsCollectionViewItem  value =
    sendMsg nsCollectionViewItem (mkSelector "setHighlightState:") retVoid [argCLong (coerce value)]

-- | @- imageView@
imageView :: IsNSCollectionViewItem nsCollectionViewItem => nsCollectionViewItem -> IO (Id NSImageView)
imageView nsCollectionViewItem  =
    sendMsg nsCollectionViewItem (mkSelector "imageView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImageView:@
setImageView :: (IsNSCollectionViewItem nsCollectionViewItem, IsNSImageView value) => nsCollectionViewItem -> value -> IO ()
setImageView nsCollectionViewItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsCollectionViewItem (mkSelector "setImageView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- textField@
textField :: IsNSCollectionViewItem nsCollectionViewItem => nsCollectionViewItem -> IO (Id NSTextField)
textField nsCollectionViewItem  =
    sendMsg nsCollectionViewItem (mkSelector "textField") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTextField:@
setTextField :: (IsNSCollectionViewItem nsCollectionViewItem, IsNSTextField value) => nsCollectionViewItem -> value -> IO ()
setTextField nsCollectionViewItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsCollectionViewItem (mkSelector "setTextField:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- draggingImageComponents@
draggingImageComponents :: IsNSCollectionViewItem nsCollectionViewItem => nsCollectionViewItem -> IO (Id NSArray)
draggingImageComponents nsCollectionViewItem  =
    sendMsg nsCollectionViewItem (mkSelector "draggingImageComponents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @collectionView@
collectionViewSelector :: Selector
collectionViewSelector = mkSelector "collectionView"

-- | @Selector@ for @selected@
selectedSelector :: Selector
selectedSelector = mkSelector "selected"

-- | @Selector@ for @setSelected:@
setSelectedSelector :: Selector
setSelectedSelector = mkSelector "setSelected:"

-- | @Selector@ for @highlightState@
highlightStateSelector :: Selector
highlightStateSelector = mkSelector "highlightState"

-- | @Selector@ for @setHighlightState:@
setHighlightStateSelector :: Selector
setHighlightStateSelector = mkSelector "setHighlightState:"

-- | @Selector@ for @imageView@
imageViewSelector :: Selector
imageViewSelector = mkSelector "imageView"

-- | @Selector@ for @setImageView:@
setImageViewSelector :: Selector
setImageViewSelector = mkSelector "setImageView:"

-- | @Selector@ for @textField@
textFieldSelector :: Selector
textFieldSelector = mkSelector "textField"

-- | @Selector@ for @setTextField:@
setTextFieldSelector :: Selector
setTextFieldSelector = mkSelector "setTextField:"

-- | @Selector@ for @draggingImageComponents@
draggingImageComponentsSelector :: Selector
draggingImageComponentsSelector = mkSelector "draggingImageComponents"

