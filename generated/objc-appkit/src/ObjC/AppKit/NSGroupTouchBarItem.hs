{-# LANGUAGE PatternSynonyms #-}
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
  , groupItemWithIdentifier_itemsSelector
  , groupItemWithIdentifier_items_allowedCompressionOptionsSelector
  , alertStyleGroupItemWithIdentifierSelector
  , groupTouchBarSelector
  , setGroupTouchBarSelector
  , customizationLabelSelector
  , setCustomizationLabelSelector
  , groupUserInterfaceLayoutDirectionSelector
  , setGroupUserInterfaceLayoutDirectionSelector
  , prefersEqualWidthsSelector
  , setPrefersEqualWidthsSelector
  , preferredItemWidthSelector
  , setPreferredItemWidthSelector

  -- * Enum types
  , NSUserInterfaceLayoutDirection(NSUserInterfaceLayoutDirection)
  , pattern NSUserInterfaceLayoutDirectionLeftToRight
  , pattern NSUserInterfaceLayoutDirectionRightToLeft

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

-- | @+ groupItemWithIdentifier:items:@
groupItemWithIdentifier_items :: (IsNSString identifier, IsNSArray items) => identifier -> items -> IO (Id NSGroupTouchBarItem)
groupItemWithIdentifier_items identifier items =
  do
    cls' <- getRequiredClass "NSGroupTouchBarItem"
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr items $ \raw_items ->
        sendClassMsg cls' (mkSelector "groupItemWithIdentifier:items:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_items :: Ptr ())] >>= retainedObject . castPtr

-- | @+ groupItemWithIdentifier:items:allowedCompressionOptions:@
groupItemWithIdentifier_items_allowedCompressionOptions :: (IsNSString identifier, IsNSArray items, IsNSUserInterfaceCompressionOptions allowedCompressionOptions) => identifier -> items -> allowedCompressionOptions -> IO (Id NSGroupTouchBarItem)
groupItemWithIdentifier_items_allowedCompressionOptions identifier items allowedCompressionOptions =
  do
    cls' <- getRequiredClass "NSGroupTouchBarItem"
    withObjCPtr identifier $ \raw_identifier ->
      withObjCPtr items $ \raw_items ->
        withObjCPtr allowedCompressionOptions $ \raw_allowedCompressionOptions ->
          sendClassMsg cls' (mkSelector "groupItemWithIdentifier:items:allowedCompressionOptions:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_items :: Ptr ()), argPtr (castPtr raw_allowedCompressionOptions :: Ptr ())] >>= retainedObject . castPtr

-- | @+ alertStyleGroupItemWithIdentifier:@
alertStyleGroupItemWithIdentifier :: IsNSString identifier => identifier -> IO (Id NSGroupTouchBarItem)
alertStyleGroupItemWithIdentifier identifier =
  do
    cls' <- getRequiredClass "NSGroupTouchBarItem"
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "alertStyleGroupItemWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | @- groupTouchBar@
groupTouchBar :: IsNSGroupTouchBarItem nsGroupTouchBarItem => nsGroupTouchBarItem -> IO (Id NSTouchBar)
groupTouchBar nsGroupTouchBarItem  =
  sendMsg nsGroupTouchBarItem (mkSelector "groupTouchBar") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupTouchBar:@
setGroupTouchBar :: (IsNSGroupTouchBarItem nsGroupTouchBarItem, IsNSTouchBar value) => nsGroupTouchBarItem -> value -> IO ()
setGroupTouchBar nsGroupTouchBarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsGroupTouchBarItem (mkSelector "setGroupTouchBar:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- customizationLabel@
customizationLabel :: IsNSGroupTouchBarItem nsGroupTouchBarItem => nsGroupTouchBarItem -> IO (Id NSString)
customizationLabel nsGroupTouchBarItem  =
  sendMsg nsGroupTouchBarItem (mkSelector "customizationLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCustomizationLabel:@
setCustomizationLabel :: (IsNSGroupTouchBarItem nsGroupTouchBarItem, IsNSString value) => nsGroupTouchBarItem -> value -> IO ()
setCustomizationLabel nsGroupTouchBarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsGroupTouchBarItem (mkSelector "setCustomizationLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupUserInterfaceLayoutDirection@
groupUserInterfaceLayoutDirection :: IsNSGroupTouchBarItem nsGroupTouchBarItem => nsGroupTouchBarItem -> IO NSUserInterfaceLayoutDirection
groupUserInterfaceLayoutDirection nsGroupTouchBarItem  =
  fmap (coerce :: CLong -> NSUserInterfaceLayoutDirection) $ sendMsg nsGroupTouchBarItem (mkSelector "groupUserInterfaceLayoutDirection") retCLong []

-- | @- setGroupUserInterfaceLayoutDirection:@
setGroupUserInterfaceLayoutDirection :: IsNSGroupTouchBarItem nsGroupTouchBarItem => nsGroupTouchBarItem -> NSUserInterfaceLayoutDirection -> IO ()
setGroupUserInterfaceLayoutDirection nsGroupTouchBarItem  value =
  sendMsg nsGroupTouchBarItem (mkSelector "setGroupUserInterfaceLayoutDirection:") retVoid [argCLong (coerce value)]

-- | @- prefersEqualWidths@
prefersEqualWidths :: IsNSGroupTouchBarItem nsGroupTouchBarItem => nsGroupTouchBarItem -> IO Bool
prefersEqualWidths nsGroupTouchBarItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsGroupTouchBarItem (mkSelector "prefersEqualWidths") retCULong []

-- | @- setPrefersEqualWidths:@
setPrefersEqualWidths :: IsNSGroupTouchBarItem nsGroupTouchBarItem => nsGroupTouchBarItem -> Bool -> IO ()
setPrefersEqualWidths nsGroupTouchBarItem  value =
  sendMsg nsGroupTouchBarItem (mkSelector "setPrefersEqualWidths:") retVoid [argCULong (if value then 1 else 0)]

-- | @- preferredItemWidth@
preferredItemWidth :: IsNSGroupTouchBarItem nsGroupTouchBarItem => nsGroupTouchBarItem -> IO CDouble
preferredItemWidth nsGroupTouchBarItem  =
  sendMsg nsGroupTouchBarItem (mkSelector "preferredItemWidth") retCDouble []

-- | @- setPreferredItemWidth:@
setPreferredItemWidth :: IsNSGroupTouchBarItem nsGroupTouchBarItem => nsGroupTouchBarItem -> CDouble -> IO ()
setPreferredItemWidth nsGroupTouchBarItem  value =
  sendMsg nsGroupTouchBarItem (mkSelector "setPreferredItemWidth:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @groupItemWithIdentifier:items:@
groupItemWithIdentifier_itemsSelector :: Selector
groupItemWithIdentifier_itemsSelector = mkSelector "groupItemWithIdentifier:items:"

-- | @Selector@ for @groupItemWithIdentifier:items:allowedCompressionOptions:@
groupItemWithIdentifier_items_allowedCompressionOptionsSelector :: Selector
groupItemWithIdentifier_items_allowedCompressionOptionsSelector = mkSelector "groupItemWithIdentifier:items:allowedCompressionOptions:"

-- | @Selector@ for @alertStyleGroupItemWithIdentifier:@
alertStyleGroupItemWithIdentifierSelector :: Selector
alertStyleGroupItemWithIdentifierSelector = mkSelector "alertStyleGroupItemWithIdentifier:"

-- | @Selector@ for @groupTouchBar@
groupTouchBarSelector :: Selector
groupTouchBarSelector = mkSelector "groupTouchBar"

-- | @Selector@ for @setGroupTouchBar:@
setGroupTouchBarSelector :: Selector
setGroupTouchBarSelector = mkSelector "setGroupTouchBar:"

-- | @Selector@ for @customizationLabel@
customizationLabelSelector :: Selector
customizationLabelSelector = mkSelector "customizationLabel"

-- | @Selector@ for @setCustomizationLabel:@
setCustomizationLabelSelector :: Selector
setCustomizationLabelSelector = mkSelector "setCustomizationLabel:"

-- | @Selector@ for @groupUserInterfaceLayoutDirection@
groupUserInterfaceLayoutDirectionSelector :: Selector
groupUserInterfaceLayoutDirectionSelector = mkSelector "groupUserInterfaceLayoutDirection"

-- | @Selector@ for @setGroupUserInterfaceLayoutDirection:@
setGroupUserInterfaceLayoutDirectionSelector :: Selector
setGroupUserInterfaceLayoutDirectionSelector = mkSelector "setGroupUserInterfaceLayoutDirection:"

-- | @Selector@ for @prefersEqualWidths@
prefersEqualWidthsSelector :: Selector
prefersEqualWidthsSelector = mkSelector "prefersEqualWidths"

-- | @Selector@ for @setPrefersEqualWidths:@
setPrefersEqualWidthsSelector :: Selector
setPrefersEqualWidthsSelector = mkSelector "setPrefersEqualWidths:"

-- | @Selector@ for @preferredItemWidth@
preferredItemWidthSelector :: Selector
preferredItemWidthSelector = mkSelector "preferredItemWidth"

-- | @Selector@ for @setPreferredItemWidth:@
setPreferredItemWidthSelector :: Selector
setPreferredItemWidthSelector = mkSelector "setPreferredItemWidth:"

