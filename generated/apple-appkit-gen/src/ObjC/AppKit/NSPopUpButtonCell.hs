{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPopUpButtonCell@.
module ObjC.AppKit.NSPopUpButtonCell
  ( NSPopUpButtonCell
  , IsNSPopUpButtonCell(..)
  , initTextCell_pullsDown
  , initWithCoder
  , addItemWithTitle
  , addItemsWithTitles
  , insertItemWithTitle_atIndex
  , removeItemWithTitle
  , removeItemAtIndex
  , removeAllItems
  , indexOfItem
  , indexOfItemWithTitle
  , indexOfItemWithTag
  , indexOfItemWithRepresentedObject
  , indexOfItemWithTarget_andAction
  , itemAtIndex
  , itemWithTitle
  , selectItem
  , selectItemAtIndex
  , selectItemWithTitle
  , selectItemWithTag
  , setTitle
  , synchronizeTitleAndSelectedItem
  , itemTitleAtIndex
  , attachPopUpWithFrame_inView
  , dismissPopUp
  , performClickWithFrame_inView
  , menu
  , setMenu
  , pullsDown
  , setPullsDown
  , autoenablesItems
  , setAutoenablesItems
  , preferredEdge
  , setPreferredEdge
  , usesItemFromMenu
  , setUsesItemFromMenu
  , altersStateOfSelectedItem
  , setAltersStateOfSelectedItem
  , itemArray
  , numberOfItems
  , lastItem
  , selectedItem
  , indexOfSelectedItem
  , itemTitles
  , titleOfSelectedItem
  , arrowPosition
  , setArrowPosition
  , addItemWithTitleSelector
  , addItemsWithTitlesSelector
  , altersStateOfSelectedItemSelector
  , arrowPositionSelector
  , attachPopUpWithFrame_inViewSelector
  , autoenablesItemsSelector
  , dismissPopUpSelector
  , indexOfItemSelector
  , indexOfItemWithRepresentedObjectSelector
  , indexOfItemWithTagSelector
  , indexOfItemWithTarget_andActionSelector
  , indexOfItemWithTitleSelector
  , indexOfSelectedItemSelector
  , initTextCell_pullsDownSelector
  , initWithCoderSelector
  , insertItemWithTitle_atIndexSelector
  , itemArraySelector
  , itemAtIndexSelector
  , itemTitleAtIndexSelector
  , itemTitlesSelector
  , itemWithTitleSelector
  , lastItemSelector
  , menuSelector
  , numberOfItemsSelector
  , performClickWithFrame_inViewSelector
  , preferredEdgeSelector
  , pullsDownSelector
  , removeAllItemsSelector
  , removeItemAtIndexSelector
  , removeItemWithTitleSelector
  , selectItemAtIndexSelector
  , selectItemSelector
  , selectItemWithTagSelector
  , selectItemWithTitleSelector
  , selectedItemSelector
  , setAltersStateOfSelectedItemSelector
  , setArrowPositionSelector
  , setAutoenablesItemsSelector
  , setMenuSelector
  , setPreferredEdgeSelector
  , setPullsDownSelector
  , setTitleSelector
  , setUsesItemFromMenuSelector
  , synchronizeTitleAndSelectedItemSelector
  , titleOfSelectedItemSelector
  , usesItemFromMenuSelector

  -- * Enum types
  , NSPopUpArrowPosition(NSPopUpArrowPosition)
  , pattern NSPopUpNoArrow
  , pattern NSPopUpArrowAtCenter
  , pattern NSPopUpArrowAtBottom
  , NSRectEdge(NSRectEdge)
  , pattern NSRectEdgeMinX
  , pattern NSRectEdgeMinY
  , pattern NSRectEdgeMaxX
  , pattern NSRectEdgeMaxY
  , pattern NSMinXEdge
  , pattern NSMinYEdge
  , pattern NSMaxXEdge
  , pattern NSMaxYEdge

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
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initTextCell:pullsDown:@
initTextCell_pullsDown :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSString stringValue) => nsPopUpButtonCell -> stringValue -> Bool -> IO (Id NSPopUpButtonCell)
initTextCell_pullsDown nsPopUpButtonCell stringValue pullDown =
  sendOwnedMessage nsPopUpButtonCell initTextCell_pullsDownSelector (toNSString stringValue) pullDown

-- | @- initWithCoder:@
initWithCoder :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSCoder coder) => nsPopUpButtonCell -> coder -> IO (Id NSPopUpButtonCell)
initWithCoder nsPopUpButtonCell coder =
  sendOwnedMessage nsPopUpButtonCell initWithCoderSelector (toNSCoder coder)

-- | @- addItemWithTitle:@
addItemWithTitle :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSString title) => nsPopUpButtonCell -> title -> IO ()
addItemWithTitle nsPopUpButtonCell title =
  sendMessage nsPopUpButtonCell addItemWithTitleSelector (toNSString title)

-- | @- addItemsWithTitles:@
addItemsWithTitles :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSArray itemTitles) => nsPopUpButtonCell -> itemTitles -> IO ()
addItemsWithTitles nsPopUpButtonCell itemTitles =
  sendMessage nsPopUpButtonCell addItemsWithTitlesSelector (toNSArray itemTitles)

-- | @- insertItemWithTitle:atIndex:@
insertItemWithTitle_atIndex :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSString title) => nsPopUpButtonCell -> title -> CLong -> IO ()
insertItemWithTitle_atIndex nsPopUpButtonCell title index =
  sendMessage nsPopUpButtonCell insertItemWithTitle_atIndexSelector (toNSString title) index

-- | @- removeItemWithTitle:@
removeItemWithTitle :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSString title) => nsPopUpButtonCell -> title -> IO ()
removeItemWithTitle nsPopUpButtonCell title =
  sendMessage nsPopUpButtonCell removeItemWithTitleSelector (toNSString title)

-- | @- removeItemAtIndex:@
removeItemAtIndex :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> CLong -> IO ()
removeItemAtIndex nsPopUpButtonCell index =
  sendMessage nsPopUpButtonCell removeItemAtIndexSelector index

-- | @- removeAllItems@
removeAllItems :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO ()
removeAllItems nsPopUpButtonCell =
  sendMessage nsPopUpButtonCell removeAllItemsSelector

-- | @- indexOfItem:@
indexOfItem :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSMenuItem item) => nsPopUpButtonCell -> item -> IO CLong
indexOfItem nsPopUpButtonCell item =
  sendMessage nsPopUpButtonCell indexOfItemSelector (toNSMenuItem item)

-- | @- indexOfItemWithTitle:@
indexOfItemWithTitle :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSString title) => nsPopUpButtonCell -> title -> IO CLong
indexOfItemWithTitle nsPopUpButtonCell title =
  sendMessage nsPopUpButtonCell indexOfItemWithTitleSelector (toNSString title)

-- | @- indexOfItemWithTag:@
indexOfItemWithTag :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> CLong -> IO CLong
indexOfItemWithTag nsPopUpButtonCell tag =
  sendMessage nsPopUpButtonCell indexOfItemWithTagSelector tag

-- | @- indexOfItemWithRepresentedObject:@
indexOfItemWithRepresentedObject :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> RawId -> IO CLong
indexOfItemWithRepresentedObject nsPopUpButtonCell obj_ =
  sendMessage nsPopUpButtonCell indexOfItemWithRepresentedObjectSelector obj_

-- | @- indexOfItemWithTarget:andAction:@
indexOfItemWithTarget_andAction :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> RawId -> Sel -> IO CLong
indexOfItemWithTarget_andAction nsPopUpButtonCell target actionSelector =
  sendMessage nsPopUpButtonCell indexOfItemWithTarget_andActionSelector target actionSelector

-- | @- itemAtIndex:@
itemAtIndex :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> CLong -> IO (Id NSMenuItem)
itemAtIndex nsPopUpButtonCell index =
  sendMessage nsPopUpButtonCell itemAtIndexSelector index

-- | @- itemWithTitle:@
itemWithTitle :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSString title) => nsPopUpButtonCell -> title -> IO (Id NSMenuItem)
itemWithTitle nsPopUpButtonCell title =
  sendMessage nsPopUpButtonCell itemWithTitleSelector (toNSString title)

-- | @- selectItem:@
selectItem :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSMenuItem item) => nsPopUpButtonCell -> item -> IO ()
selectItem nsPopUpButtonCell item =
  sendMessage nsPopUpButtonCell selectItemSelector (toNSMenuItem item)

-- | @- selectItemAtIndex:@
selectItemAtIndex :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> CLong -> IO ()
selectItemAtIndex nsPopUpButtonCell index =
  sendMessage nsPopUpButtonCell selectItemAtIndexSelector index

-- | @- selectItemWithTitle:@
selectItemWithTitle :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSString title) => nsPopUpButtonCell -> title -> IO ()
selectItemWithTitle nsPopUpButtonCell title =
  sendMessage nsPopUpButtonCell selectItemWithTitleSelector (toNSString title)

-- | @- selectItemWithTag:@
selectItemWithTag :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> CLong -> IO Bool
selectItemWithTag nsPopUpButtonCell tag =
  sendMessage nsPopUpButtonCell selectItemWithTagSelector tag

-- | @- setTitle:@
setTitle :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSString string) => nsPopUpButtonCell -> string -> IO ()
setTitle nsPopUpButtonCell string =
  sendMessage nsPopUpButtonCell setTitleSelector (toNSString string)

-- | @- synchronizeTitleAndSelectedItem@
synchronizeTitleAndSelectedItem :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO ()
synchronizeTitleAndSelectedItem nsPopUpButtonCell =
  sendMessage nsPopUpButtonCell synchronizeTitleAndSelectedItemSelector

-- | @- itemTitleAtIndex:@
itemTitleAtIndex :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> CLong -> IO (Id NSString)
itemTitleAtIndex nsPopUpButtonCell index =
  sendMessage nsPopUpButtonCell itemTitleAtIndexSelector index

-- | @- attachPopUpWithFrame:inView:@
attachPopUpWithFrame_inView :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSView controlView) => nsPopUpButtonCell -> NSRect -> controlView -> IO ()
attachPopUpWithFrame_inView nsPopUpButtonCell cellFrame controlView =
  sendMessage nsPopUpButtonCell attachPopUpWithFrame_inViewSelector cellFrame (toNSView controlView)

-- | @- dismissPopUp@
dismissPopUp :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO ()
dismissPopUp nsPopUpButtonCell =
  sendMessage nsPopUpButtonCell dismissPopUpSelector

-- | @- performClickWithFrame:inView:@
performClickWithFrame_inView :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSView controlView) => nsPopUpButtonCell -> NSRect -> controlView -> IO ()
performClickWithFrame_inView nsPopUpButtonCell frame controlView =
  sendMessage nsPopUpButtonCell performClickWithFrame_inViewSelector frame (toNSView controlView)

-- | @- menu@
menu :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO (Id NSMenu)
menu nsPopUpButtonCell =
  sendMessage nsPopUpButtonCell menuSelector

-- | @- setMenu:@
setMenu :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSMenu value) => nsPopUpButtonCell -> value -> IO ()
setMenu nsPopUpButtonCell value =
  sendMessage nsPopUpButtonCell setMenuSelector (toNSMenu value)

-- | @- pullsDown@
pullsDown :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO Bool
pullsDown nsPopUpButtonCell =
  sendMessage nsPopUpButtonCell pullsDownSelector

-- | @- setPullsDown:@
setPullsDown :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> Bool -> IO ()
setPullsDown nsPopUpButtonCell value =
  sendMessage nsPopUpButtonCell setPullsDownSelector value

-- | @- autoenablesItems@
autoenablesItems :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO Bool
autoenablesItems nsPopUpButtonCell =
  sendMessage nsPopUpButtonCell autoenablesItemsSelector

-- | @- setAutoenablesItems:@
setAutoenablesItems :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> Bool -> IO ()
setAutoenablesItems nsPopUpButtonCell value =
  sendMessage nsPopUpButtonCell setAutoenablesItemsSelector value

-- | @- preferredEdge@
preferredEdge :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO NSRectEdge
preferredEdge nsPopUpButtonCell =
  sendMessage nsPopUpButtonCell preferredEdgeSelector

-- | @- setPreferredEdge:@
setPreferredEdge :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> NSRectEdge -> IO ()
setPreferredEdge nsPopUpButtonCell value =
  sendMessage nsPopUpButtonCell setPreferredEdgeSelector value

-- | @- usesItemFromMenu@
usesItemFromMenu :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO Bool
usesItemFromMenu nsPopUpButtonCell =
  sendMessage nsPopUpButtonCell usesItemFromMenuSelector

-- | @- setUsesItemFromMenu:@
setUsesItemFromMenu :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> Bool -> IO ()
setUsesItemFromMenu nsPopUpButtonCell value =
  sendMessage nsPopUpButtonCell setUsesItemFromMenuSelector value

-- | @- altersStateOfSelectedItem@
altersStateOfSelectedItem :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO Bool
altersStateOfSelectedItem nsPopUpButtonCell =
  sendMessage nsPopUpButtonCell altersStateOfSelectedItemSelector

-- | @- setAltersStateOfSelectedItem:@
setAltersStateOfSelectedItem :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> Bool -> IO ()
setAltersStateOfSelectedItem nsPopUpButtonCell value =
  sendMessage nsPopUpButtonCell setAltersStateOfSelectedItemSelector value

-- | @- itemArray@
itemArray :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO (Id NSArray)
itemArray nsPopUpButtonCell =
  sendMessage nsPopUpButtonCell itemArraySelector

-- | @- numberOfItems@
numberOfItems :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO CLong
numberOfItems nsPopUpButtonCell =
  sendMessage nsPopUpButtonCell numberOfItemsSelector

-- | @- lastItem@
lastItem :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO (Id NSMenuItem)
lastItem nsPopUpButtonCell =
  sendMessage nsPopUpButtonCell lastItemSelector

-- | @- selectedItem@
selectedItem :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO (Id NSMenuItem)
selectedItem nsPopUpButtonCell =
  sendMessage nsPopUpButtonCell selectedItemSelector

-- | @- indexOfSelectedItem@
indexOfSelectedItem :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO CLong
indexOfSelectedItem nsPopUpButtonCell =
  sendMessage nsPopUpButtonCell indexOfSelectedItemSelector

-- | @- itemTitles@
itemTitles :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO (Id NSArray)
itemTitles nsPopUpButtonCell =
  sendMessage nsPopUpButtonCell itemTitlesSelector

-- | @- titleOfSelectedItem@
titleOfSelectedItem :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO (Id NSString)
titleOfSelectedItem nsPopUpButtonCell =
  sendMessage nsPopUpButtonCell titleOfSelectedItemSelector

-- | @- arrowPosition@
arrowPosition :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO NSPopUpArrowPosition
arrowPosition nsPopUpButtonCell =
  sendMessage nsPopUpButtonCell arrowPositionSelector

-- | @- setArrowPosition:@
setArrowPosition :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> NSPopUpArrowPosition -> IO ()
setArrowPosition nsPopUpButtonCell value =
  sendMessage nsPopUpButtonCell setArrowPositionSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initTextCell:pullsDown:@
initTextCell_pullsDownSelector :: Selector '[Id NSString, Bool] (Id NSPopUpButtonCell)
initTextCell_pullsDownSelector = mkSelector "initTextCell:pullsDown:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSPopUpButtonCell)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @addItemWithTitle:@
addItemWithTitleSelector :: Selector '[Id NSString] ()
addItemWithTitleSelector = mkSelector "addItemWithTitle:"

-- | @Selector@ for @addItemsWithTitles:@
addItemsWithTitlesSelector :: Selector '[Id NSArray] ()
addItemsWithTitlesSelector = mkSelector "addItemsWithTitles:"

-- | @Selector@ for @insertItemWithTitle:atIndex:@
insertItemWithTitle_atIndexSelector :: Selector '[Id NSString, CLong] ()
insertItemWithTitle_atIndexSelector = mkSelector "insertItemWithTitle:atIndex:"

-- | @Selector@ for @removeItemWithTitle:@
removeItemWithTitleSelector :: Selector '[Id NSString] ()
removeItemWithTitleSelector = mkSelector "removeItemWithTitle:"

-- | @Selector@ for @removeItemAtIndex:@
removeItemAtIndexSelector :: Selector '[CLong] ()
removeItemAtIndexSelector = mkSelector "removeItemAtIndex:"

-- | @Selector@ for @removeAllItems@
removeAllItemsSelector :: Selector '[] ()
removeAllItemsSelector = mkSelector "removeAllItems"

-- | @Selector@ for @indexOfItem:@
indexOfItemSelector :: Selector '[Id NSMenuItem] CLong
indexOfItemSelector = mkSelector "indexOfItem:"

-- | @Selector@ for @indexOfItemWithTitle:@
indexOfItemWithTitleSelector :: Selector '[Id NSString] CLong
indexOfItemWithTitleSelector = mkSelector "indexOfItemWithTitle:"

-- | @Selector@ for @indexOfItemWithTag:@
indexOfItemWithTagSelector :: Selector '[CLong] CLong
indexOfItemWithTagSelector = mkSelector "indexOfItemWithTag:"

-- | @Selector@ for @indexOfItemWithRepresentedObject:@
indexOfItemWithRepresentedObjectSelector :: Selector '[RawId] CLong
indexOfItemWithRepresentedObjectSelector = mkSelector "indexOfItemWithRepresentedObject:"

-- | @Selector@ for @indexOfItemWithTarget:andAction:@
indexOfItemWithTarget_andActionSelector :: Selector '[RawId, Sel] CLong
indexOfItemWithTarget_andActionSelector = mkSelector "indexOfItemWithTarget:andAction:"

-- | @Selector@ for @itemAtIndex:@
itemAtIndexSelector :: Selector '[CLong] (Id NSMenuItem)
itemAtIndexSelector = mkSelector "itemAtIndex:"

-- | @Selector@ for @itemWithTitle:@
itemWithTitleSelector :: Selector '[Id NSString] (Id NSMenuItem)
itemWithTitleSelector = mkSelector "itemWithTitle:"

-- | @Selector@ for @selectItem:@
selectItemSelector :: Selector '[Id NSMenuItem] ()
selectItemSelector = mkSelector "selectItem:"

-- | @Selector@ for @selectItemAtIndex:@
selectItemAtIndexSelector :: Selector '[CLong] ()
selectItemAtIndexSelector = mkSelector "selectItemAtIndex:"

-- | @Selector@ for @selectItemWithTitle:@
selectItemWithTitleSelector :: Selector '[Id NSString] ()
selectItemWithTitleSelector = mkSelector "selectItemWithTitle:"

-- | @Selector@ for @selectItemWithTag:@
selectItemWithTagSelector :: Selector '[CLong] Bool
selectItemWithTagSelector = mkSelector "selectItemWithTag:"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @synchronizeTitleAndSelectedItem@
synchronizeTitleAndSelectedItemSelector :: Selector '[] ()
synchronizeTitleAndSelectedItemSelector = mkSelector "synchronizeTitleAndSelectedItem"

-- | @Selector@ for @itemTitleAtIndex:@
itemTitleAtIndexSelector :: Selector '[CLong] (Id NSString)
itemTitleAtIndexSelector = mkSelector "itemTitleAtIndex:"

-- | @Selector@ for @attachPopUpWithFrame:inView:@
attachPopUpWithFrame_inViewSelector :: Selector '[NSRect, Id NSView] ()
attachPopUpWithFrame_inViewSelector = mkSelector "attachPopUpWithFrame:inView:"

-- | @Selector@ for @dismissPopUp@
dismissPopUpSelector :: Selector '[] ()
dismissPopUpSelector = mkSelector "dismissPopUp"

-- | @Selector@ for @performClickWithFrame:inView:@
performClickWithFrame_inViewSelector :: Selector '[NSRect, Id NSView] ()
performClickWithFrame_inViewSelector = mkSelector "performClickWithFrame:inView:"

-- | @Selector@ for @menu@
menuSelector :: Selector '[] (Id NSMenu)
menuSelector = mkSelector "menu"

-- | @Selector@ for @setMenu:@
setMenuSelector :: Selector '[Id NSMenu] ()
setMenuSelector = mkSelector "setMenu:"

-- | @Selector@ for @pullsDown@
pullsDownSelector :: Selector '[] Bool
pullsDownSelector = mkSelector "pullsDown"

-- | @Selector@ for @setPullsDown:@
setPullsDownSelector :: Selector '[Bool] ()
setPullsDownSelector = mkSelector "setPullsDown:"

-- | @Selector@ for @autoenablesItems@
autoenablesItemsSelector :: Selector '[] Bool
autoenablesItemsSelector = mkSelector "autoenablesItems"

-- | @Selector@ for @setAutoenablesItems:@
setAutoenablesItemsSelector :: Selector '[Bool] ()
setAutoenablesItemsSelector = mkSelector "setAutoenablesItems:"

-- | @Selector@ for @preferredEdge@
preferredEdgeSelector :: Selector '[] NSRectEdge
preferredEdgeSelector = mkSelector "preferredEdge"

-- | @Selector@ for @setPreferredEdge:@
setPreferredEdgeSelector :: Selector '[NSRectEdge] ()
setPreferredEdgeSelector = mkSelector "setPreferredEdge:"

-- | @Selector@ for @usesItemFromMenu@
usesItemFromMenuSelector :: Selector '[] Bool
usesItemFromMenuSelector = mkSelector "usesItemFromMenu"

-- | @Selector@ for @setUsesItemFromMenu:@
setUsesItemFromMenuSelector :: Selector '[Bool] ()
setUsesItemFromMenuSelector = mkSelector "setUsesItemFromMenu:"

-- | @Selector@ for @altersStateOfSelectedItem@
altersStateOfSelectedItemSelector :: Selector '[] Bool
altersStateOfSelectedItemSelector = mkSelector "altersStateOfSelectedItem"

-- | @Selector@ for @setAltersStateOfSelectedItem:@
setAltersStateOfSelectedItemSelector :: Selector '[Bool] ()
setAltersStateOfSelectedItemSelector = mkSelector "setAltersStateOfSelectedItem:"

-- | @Selector@ for @itemArray@
itemArraySelector :: Selector '[] (Id NSArray)
itemArraySelector = mkSelector "itemArray"

-- | @Selector@ for @numberOfItems@
numberOfItemsSelector :: Selector '[] CLong
numberOfItemsSelector = mkSelector "numberOfItems"

-- | @Selector@ for @lastItem@
lastItemSelector :: Selector '[] (Id NSMenuItem)
lastItemSelector = mkSelector "lastItem"

-- | @Selector@ for @selectedItem@
selectedItemSelector :: Selector '[] (Id NSMenuItem)
selectedItemSelector = mkSelector "selectedItem"

-- | @Selector@ for @indexOfSelectedItem@
indexOfSelectedItemSelector :: Selector '[] CLong
indexOfSelectedItemSelector = mkSelector "indexOfSelectedItem"

-- | @Selector@ for @itemTitles@
itemTitlesSelector :: Selector '[] (Id NSArray)
itemTitlesSelector = mkSelector "itemTitles"

-- | @Selector@ for @titleOfSelectedItem@
titleOfSelectedItemSelector :: Selector '[] (Id NSString)
titleOfSelectedItemSelector = mkSelector "titleOfSelectedItem"

-- | @Selector@ for @arrowPosition@
arrowPositionSelector :: Selector '[] NSPopUpArrowPosition
arrowPositionSelector = mkSelector "arrowPosition"

-- | @Selector@ for @setArrowPosition:@
setArrowPositionSelector :: Selector '[NSPopUpArrowPosition] ()
setArrowPositionSelector = mkSelector "setArrowPosition:"

