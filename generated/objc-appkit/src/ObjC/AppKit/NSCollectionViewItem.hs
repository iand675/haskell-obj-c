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
  , collectionViewSelector
  , selectedSelector
  , setSelectedSelector
  , highlightStateSelector
  , setHighlightStateSelector

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

