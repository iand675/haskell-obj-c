{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NCWidgetListViewController@.
module ObjC.NotificationCenter.NCWidgetListViewController
  ( NCWidgetListViewController
  , IsNCWidgetListViewController(..)
  , viewControllerAtRow_makeIfNecessary
  , rowForViewController
  , delegate
  , setDelegate
  , contents
  , setContents
  , minimumVisibleRowCount
  , setMinimumVisibleRowCount
  , hasDividerLines
  , setHasDividerLines
  , editing
  , setEditing
  , showsAddButtonWhenEditing
  , setShowsAddButtonWhenEditing
  , contentsSelector
  , delegateSelector
  , editingSelector
  , hasDividerLinesSelector
  , minimumVisibleRowCountSelector
  , rowForViewControllerSelector
  , setContentsSelector
  , setDelegateSelector
  , setEditingSelector
  , setHasDividerLinesSelector
  , setMinimumVisibleRowCountSelector
  , setShowsAddButtonWhenEditingSelector
  , showsAddButtonWhenEditingSelector
  , viewControllerAtRow_makeIfNecessarySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NotificationCenter.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- viewControllerAtRow:makeIfNecessary:@
viewControllerAtRow_makeIfNecessary :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> CULong -> Bool -> IO (Id NSViewController)
viewControllerAtRow_makeIfNecessary ncWidgetListViewController row makeIfNecesary =
  sendMessage ncWidgetListViewController viewControllerAtRow_makeIfNecessarySelector row makeIfNecesary

-- | @- rowForViewController:@
rowForViewController :: (IsNCWidgetListViewController ncWidgetListViewController, IsNSViewController viewController) => ncWidgetListViewController -> viewController -> IO CULong
rowForViewController ncWidgetListViewController viewController =
  sendMessage ncWidgetListViewController rowForViewControllerSelector (toNSViewController viewController)

-- | @- delegate@
delegate :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> IO RawId
delegate ncWidgetListViewController =
  sendMessage ncWidgetListViewController delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> RawId -> IO ()
setDelegate ncWidgetListViewController value =
  sendMessage ncWidgetListViewController setDelegateSelector value

-- | @- contents@
contents :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> IO (Id NSArray)
contents ncWidgetListViewController =
  sendMessage ncWidgetListViewController contentsSelector

-- | @- setContents:@
setContents :: (IsNCWidgetListViewController ncWidgetListViewController, IsNSArray value) => ncWidgetListViewController -> value -> IO ()
setContents ncWidgetListViewController value =
  sendMessage ncWidgetListViewController setContentsSelector (toNSArray value)

-- | @- minimumVisibleRowCount@
minimumVisibleRowCount :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> IO CULong
minimumVisibleRowCount ncWidgetListViewController =
  sendMessage ncWidgetListViewController minimumVisibleRowCountSelector

-- | @- setMinimumVisibleRowCount:@
setMinimumVisibleRowCount :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> CULong -> IO ()
setMinimumVisibleRowCount ncWidgetListViewController value =
  sendMessage ncWidgetListViewController setMinimumVisibleRowCountSelector value

-- | @- hasDividerLines@
hasDividerLines :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> IO Bool
hasDividerLines ncWidgetListViewController =
  sendMessage ncWidgetListViewController hasDividerLinesSelector

-- | @- setHasDividerLines:@
setHasDividerLines :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> Bool -> IO ()
setHasDividerLines ncWidgetListViewController value =
  sendMessage ncWidgetListViewController setHasDividerLinesSelector value

-- | @- editing@
editing :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> IO Bool
editing ncWidgetListViewController =
  sendMessage ncWidgetListViewController editingSelector

-- | @- setEditing:@
setEditing :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> Bool -> IO ()
setEditing ncWidgetListViewController value =
  sendMessage ncWidgetListViewController setEditingSelector value

-- | @- showsAddButtonWhenEditing@
showsAddButtonWhenEditing :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> IO Bool
showsAddButtonWhenEditing ncWidgetListViewController =
  sendMessage ncWidgetListViewController showsAddButtonWhenEditingSelector

-- | @- setShowsAddButtonWhenEditing:@
setShowsAddButtonWhenEditing :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> Bool -> IO ()
setShowsAddButtonWhenEditing ncWidgetListViewController value =
  sendMessage ncWidgetListViewController setShowsAddButtonWhenEditingSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @viewControllerAtRow:makeIfNecessary:@
viewControllerAtRow_makeIfNecessarySelector :: Selector '[CULong, Bool] (Id NSViewController)
viewControllerAtRow_makeIfNecessarySelector = mkSelector "viewControllerAtRow:makeIfNecessary:"

-- | @Selector@ for @rowForViewController:@
rowForViewControllerSelector :: Selector '[Id NSViewController] CULong
rowForViewControllerSelector = mkSelector "rowForViewController:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @contents@
contentsSelector :: Selector '[] (Id NSArray)
contentsSelector = mkSelector "contents"

-- | @Selector@ for @setContents:@
setContentsSelector :: Selector '[Id NSArray] ()
setContentsSelector = mkSelector "setContents:"

-- | @Selector@ for @minimumVisibleRowCount@
minimumVisibleRowCountSelector :: Selector '[] CULong
minimumVisibleRowCountSelector = mkSelector "minimumVisibleRowCount"

-- | @Selector@ for @setMinimumVisibleRowCount:@
setMinimumVisibleRowCountSelector :: Selector '[CULong] ()
setMinimumVisibleRowCountSelector = mkSelector "setMinimumVisibleRowCount:"

-- | @Selector@ for @hasDividerLines@
hasDividerLinesSelector :: Selector '[] Bool
hasDividerLinesSelector = mkSelector "hasDividerLines"

-- | @Selector@ for @setHasDividerLines:@
setHasDividerLinesSelector :: Selector '[Bool] ()
setHasDividerLinesSelector = mkSelector "setHasDividerLines:"

-- | @Selector@ for @editing@
editingSelector :: Selector '[] Bool
editingSelector = mkSelector "editing"

-- | @Selector@ for @setEditing:@
setEditingSelector :: Selector '[Bool] ()
setEditingSelector = mkSelector "setEditing:"

-- | @Selector@ for @showsAddButtonWhenEditing@
showsAddButtonWhenEditingSelector :: Selector '[] Bool
showsAddButtonWhenEditingSelector = mkSelector "showsAddButtonWhenEditing"

-- | @Selector@ for @setShowsAddButtonWhenEditing:@
setShowsAddButtonWhenEditingSelector :: Selector '[Bool] ()
setShowsAddButtonWhenEditingSelector = mkSelector "setShowsAddButtonWhenEditing:"

