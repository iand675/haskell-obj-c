{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSGroupTouchBarItem@.
module ObjC.AppKit.NSGroupTouchBarItem
  ( NSGroupTouchBarItem
  , IsNSGroupTouchBarItem(..)
  , groupItemWithIdentifier_items
  , groupItemWithIdentifier_items_allowedCompressionOptions
  , alertStyleGroupItemWithIdentifier
  , groupTouchBar
  , setGroupTouchBar
  , customizationLabel
  , setCustomizationLabel
  , groupUserInterfaceLayoutDirection
  , setGroupUserInterfaceLayoutDirection
  , prefersEqualWidths
  , setPrefersEqualWidths
  , preferredItemWidth
  , setPreferredItemWidth
  , effectiveCompressionOptions
  , prioritizedCompressionOptions
  , setPrioritizedCompressionOptions
  , alertStyleGroupItemWithIdentifierSelector
  , customizationLabelSelector
  , effectiveCompressionOptionsSelector
  , groupItemWithIdentifier_itemsSelector
  , groupItemWithIdentifier_items_allowedCompressionOptionsSelector
  , groupTouchBarSelector
  , groupUserInterfaceLayoutDirectionSelector
  , preferredItemWidthSelector
  , prefersEqualWidthsSelector
  , prioritizedCompressionOptionsSelector
  , setCustomizationLabelSelector
  , setGroupTouchBarSelector
  , setGroupUserInterfaceLayoutDirectionSelector
  , setPreferredItemWidthSelector
  , setPrefersEqualWidthsSelector
  , setPrioritizedCompressionOptionsSelector

  -- * Enum types
  , NSUserInterfaceLayoutDirection(NSUserInterfaceLayoutDirection)
  , pattern NSUserInterfaceLayoutDirectionLeftToRight
  , pattern NSUserInterfaceLayoutDirectionRightToLeft

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

-- | @+ groupItemWithIdentifier:items:@
groupItemWithIdentifier_items :: (IsNSString identifier, IsNSArray items) => identifier -> items -> IO (Id NSGroupTouchBarItem)
groupItemWithIdentifier_items identifier items =
  do
    cls' <- getRequiredClass "NSGroupTouchBarItem"
    sendClassMessage cls' groupItemWithIdentifier_itemsSelector (toNSString identifier) (toNSArray items)

-- | @+ groupItemWithIdentifier:items:allowedCompressionOptions:@
groupItemWithIdentifier_items_allowedCompressionOptions :: (IsNSString identifier, IsNSArray items, IsNSUserInterfaceCompressionOptions allowedCompressionOptions) => identifier -> items -> allowedCompressionOptions -> IO (Id NSGroupTouchBarItem)
groupItemWithIdentifier_items_allowedCompressionOptions identifier items allowedCompressionOptions =
  do
    cls' <- getRequiredClass "NSGroupTouchBarItem"
    sendClassMessage cls' groupItemWithIdentifier_items_allowedCompressionOptionsSelector (toNSString identifier) (toNSArray items) (toNSUserInterfaceCompressionOptions allowedCompressionOptions)

-- | @+ alertStyleGroupItemWithIdentifier:@
alertStyleGroupItemWithIdentifier :: IsNSString identifier => identifier -> IO (Id NSGroupTouchBarItem)
alertStyleGroupItemWithIdentifier identifier =
  do
    cls' <- getRequiredClass "NSGroupTouchBarItem"
    sendClassMessage cls' alertStyleGroupItemWithIdentifierSelector (toNSString identifier)

-- | @- groupTouchBar@
groupTouchBar :: IsNSGroupTouchBarItem nsGroupTouchBarItem => nsGroupTouchBarItem -> IO (Id NSTouchBar)
groupTouchBar nsGroupTouchBarItem =
  sendMessage nsGroupTouchBarItem groupTouchBarSelector

-- | @- setGroupTouchBar:@
setGroupTouchBar :: (IsNSGroupTouchBarItem nsGroupTouchBarItem, IsNSTouchBar value) => nsGroupTouchBarItem -> value -> IO ()
setGroupTouchBar nsGroupTouchBarItem value =
  sendMessage nsGroupTouchBarItem setGroupTouchBarSelector (toNSTouchBar value)

-- | @- customizationLabel@
customizationLabel :: IsNSGroupTouchBarItem nsGroupTouchBarItem => nsGroupTouchBarItem -> IO (Id NSString)
customizationLabel nsGroupTouchBarItem =
  sendMessage nsGroupTouchBarItem customizationLabelSelector

-- | @- setCustomizationLabel:@
setCustomizationLabel :: (IsNSGroupTouchBarItem nsGroupTouchBarItem, IsNSString value) => nsGroupTouchBarItem -> value -> IO ()
setCustomizationLabel nsGroupTouchBarItem value =
  sendMessage nsGroupTouchBarItem setCustomizationLabelSelector (toNSString value)

-- | @- groupUserInterfaceLayoutDirection@
groupUserInterfaceLayoutDirection :: IsNSGroupTouchBarItem nsGroupTouchBarItem => nsGroupTouchBarItem -> IO NSUserInterfaceLayoutDirection
groupUserInterfaceLayoutDirection nsGroupTouchBarItem =
  sendMessage nsGroupTouchBarItem groupUserInterfaceLayoutDirectionSelector

-- | @- setGroupUserInterfaceLayoutDirection:@
setGroupUserInterfaceLayoutDirection :: IsNSGroupTouchBarItem nsGroupTouchBarItem => nsGroupTouchBarItem -> NSUserInterfaceLayoutDirection -> IO ()
setGroupUserInterfaceLayoutDirection nsGroupTouchBarItem value =
  sendMessage nsGroupTouchBarItem setGroupUserInterfaceLayoutDirectionSelector value

-- | @- prefersEqualWidths@
prefersEqualWidths :: IsNSGroupTouchBarItem nsGroupTouchBarItem => nsGroupTouchBarItem -> IO Bool
prefersEqualWidths nsGroupTouchBarItem =
  sendMessage nsGroupTouchBarItem prefersEqualWidthsSelector

-- | @- setPrefersEqualWidths:@
setPrefersEqualWidths :: IsNSGroupTouchBarItem nsGroupTouchBarItem => nsGroupTouchBarItem -> Bool -> IO ()
setPrefersEqualWidths nsGroupTouchBarItem value =
  sendMessage nsGroupTouchBarItem setPrefersEqualWidthsSelector value

-- | @- preferredItemWidth@
preferredItemWidth :: IsNSGroupTouchBarItem nsGroupTouchBarItem => nsGroupTouchBarItem -> IO CDouble
preferredItemWidth nsGroupTouchBarItem =
  sendMessage nsGroupTouchBarItem preferredItemWidthSelector

-- | @- setPreferredItemWidth:@
setPreferredItemWidth :: IsNSGroupTouchBarItem nsGroupTouchBarItem => nsGroupTouchBarItem -> CDouble -> IO ()
setPreferredItemWidth nsGroupTouchBarItem value =
  sendMessage nsGroupTouchBarItem setPreferredItemWidthSelector value

-- | @- effectiveCompressionOptions@
effectiveCompressionOptions :: IsNSGroupTouchBarItem nsGroupTouchBarItem => nsGroupTouchBarItem -> IO (Id NSUserInterfaceCompressionOptions)
effectiveCompressionOptions nsGroupTouchBarItem =
  sendMessage nsGroupTouchBarItem effectiveCompressionOptionsSelector

-- | @- prioritizedCompressionOptions@
prioritizedCompressionOptions :: IsNSGroupTouchBarItem nsGroupTouchBarItem => nsGroupTouchBarItem -> IO (Id NSArray)
prioritizedCompressionOptions nsGroupTouchBarItem =
  sendMessage nsGroupTouchBarItem prioritizedCompressionOptionsSelector

-- | @- setPrioritizedCompressionOptions:@
setPrioritizedCompressionOptions :: (IsNSGroupTouchBarItem nsGroupTouchBarItem, IsNSArray value) => nsGroupTouchBarItem -> value -> IO ()
setPrioritizedCompressionOptions nsGroupTouchBarItem value =
  sendMessage nsGroupTouchBarItem setPrioritizedCompressionOptionsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupItemWithIdentifier:items:@
groupItemWithIdentifier_itemsSelector :: Selector '[Id NSString, Id NSArray] (Id NSGroupTouchBarItem)
groupItemWithIdentifier_itemsSelector = mkSelector "groupItemWithIdentifier:items:"

-- | @Selector@ for @groupItemWithIdentifier:items:allowedCompressionOptions:@
groupItemWithIdentifier_items_allowedCompressionOptionsSelector :: Selector '[Id NSString, Id NSArray, Id NSUserInterfaceCompressionOptions] (Id NSGroupTouchBarItem)
groupItemWithIdentifier_items_allowedCompressionOptionsSelector = mkSelector "groupItemWithIdentifier:items:allowedCompressionOptions:"

-- | @Selector@ for @alertStyleGroupItemWithIdentifier:@
alertStyleGroupItemWithIdentifierSelector :: Selector '[Id NSString] (Id NSGroupTouchBarItem)
alertStyleGroupItemWithIdentifierSelector = mkSelector "alertStyleGroupItemWithIdentifier:"

-- | @Selector@ for @groupTouchBar@
groupTouchBarSelector :: Selector '[] (Id NSTouchBar)
groupTouchBarSelector = mkSelector "groupTouchBar"

-- | @Selector@ for @setGroupTouchBar:@
setGroupTouchBarSelector :: Selector '[Id NSTouchBar] ()
setGroupTouchBarSelector = mkSelector "setGroupTouchBar:"

-- | @Selector@ for @customizationLabel@
customizationLabelSelector :: Selector '[] (Id NSString)
customizationLabelSelector = mkSelector "customizationLabel"

-- | @Selector@ for @setCustomizationLabel:@
setCustomizationLabelSelector :: Selector '[Id NSString] ()
setCustomizationLabelSelector = mkSelector "setCustomizationLabel:"

-- | @Selector@ for @groupUserInterfaceLayoutDirection@
groupUserInterfaceLayoutDirectionSelector :: Selector '[] NSUserInterfaceLayoutDirection
groupUserInterfaceLayoutDirectionSelector = mkSelector "groupUserInterfaceLayoutDirection"

-- | @Selector@ for @setGroupUserInterfaceLayoutDirection:@
setGroupUserInterfaceLayoutDirectionSelector :: Selector '[NSUserInterfaceLayoutDirection] ()
setGroupUserInterfaceLayoutDirectionSelector = mkSelector "setGroupUserInterfaceLayoutDirection:"

-- | @Selector@ for @prefersEqualWidths@
prefersEqualWidthsSelector :: Selector '[] Bool
prefersEqualWidthsSelector = mkSelector "prefersEqualWidths"

-- | @Selector@ for @setPrefersEqualWidths:@
setPrefersEqualWidthsSelector :: Selector '[Bool] ()
setPrefersEqualWidthsSelector = mkSelector "setPrefersEqualWidths:"

-- | @Selector@ for @preferredItemWidth@
preferredItemWidthSelector :: Selector '[] CDouble
preferredItemWidthSelector = mkSelector "preferredItemWidth"

-- | @Selector@ for @setPreferredItemWidth:@
setPreferredItemWidthSelector :: Selector '[CDouble] ()
setPreferredItemWidthSelector = mkSelector "setPreferredItemWidth:"

-- | @Selector@ for @effectiveCompressionOptions@
effectiveCompressionOptionsSelector :: Selector '[] (Id NSUserInterfaceCompressionOptions)
effectiveCompressionOptionsSelector = mkSelector "effectiveCompressionOptions"

-- | @Selector@ for @prioritizedCompressionOptions@
prioritizedCompressionOptionsSelector :: Selector '[] (Id NSArray)
prioritizedCompressionOptionsSelector = mkSelector "prioritizedCompressionOptions"

-- | @Selector@ for @setPrioritizedCompressionOptions:@
setPrioritizedCompressionOptionsSelector :: Selector '[Id NSArray] ()
setPrioritizedCompressionOptionsSelector = mkSelector "setPrioritizedCompressionOptions:"

