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
  , viewControllerAtRow_makeIfNecessarySelector
  , rowForViewControllerSelector
  , delegateSelector
  , setDelegateSelector
  , contentsSelector
  , setContentsSelector
  , minimumVisibleRowCountSelector
  , setMinimumVisibleRowCountSelector
  , hasDividerLinesSelector
  , setHasDividerLinesSelector
  , editingSelector
  , setEditingSelector
  , showsAddButtonWhenEditingSelector
  , setShowsAddButtonWhenEditingSelector


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

import ObjC.NotificationCenter.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- viewControllerAtRow:makeIfNecessary:@
viewControllerAtRow_makeIfNecessary :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> CULong -> Bool -> IO (Id NSViewController)
viewControllerAtRow_makeIfNecessary ncWidgetListViewController  row makeIfNecesary =
    sendMsg ncWidgetListViewController (mkSelector "viewControllerAtRow:makeIfNecessary:") (retPtr retVoid) [argCULong row, argCULong (if makeIfNecesary then 1 else 0)] >>= retainedObject . castPtr

-- | @- rowForViewController:@
rowForViewController :: (IsNCWidgetListViewController ncWidgetListViewController, IsNSViewController viewController) => ncWidgetListViewController -> viewController -> IO CULong
rowForViewController ncWidgetListViewController  viewController =
  withObjCPtr viewController $ \raw_viewController ->
      sendMsg ncWidgetListViewController (mkSelector "rowForViewController:") retCULong [argPtr (castPtr raw_viewController :: Ptr ())]

-- | @- delegate@
delegate :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> IO RawId
delegate ncWidgetListViewController  =
    fmap (RawId . castPtr) $ sendMsg ncWidgetListViewController (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> RawId -> IO ()
setDelegate ncWidgetListViewController  value =
    sendMsg ncWidgetListViewController (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- contents@
contents :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> IO (Id NSArray)
contents ncWidgetListViewController  =
    sendMsg ncWidgetListViewController (mkSelector "contents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContents:@
setContents :: (IsNCWidgetListViewController ncWidgetListViewController, IsNSArray value) => ncWidgetListViewController -> value -> IO ()
setContents ncWidgetListViewController  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ncWidgetListViewController (mkSelector "setContents:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- minimumVisibleRowCount@
minimumVisibleRowCount :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> IO CULong
minimumVisibleRowCount ncWidgetListViewController  =
    sendMsg ncWidgetListViewController (mkSelector "minimumVisibleRowCount") retCULong []

-- | @- setMinimumVisibleRowCount:@
setMinimumVisibleRowCount :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> CULong -> IO ()
setMinimumVisibleRowCount ncWidgetListViewController  value =
    sendMsg ncWidgetListViewController (mkSelector "setMinimumVisibleRowCount:") retVoid [argCULong value]

-- | @- hasDividerLines@
hasDividerLines :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> IO Bool
hasDividerLines ncWidgetListViewController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ncWidgetListViewController (mkSelector "hasDividerLines") retCULong []

-- | @- setHasDividerLines:@
setHasDividerLines :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> Bool -> IO ()
setHasDividerLines ncWidgetListViewController  value =
    sendMsg ncWidgetListViewController (mkSelector "setHasDividerLines:") retVoid [argCULong (if value then 1 else 0)]

-- | @- editing@
editing :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> IO Bool
editing ncWidgetListViewController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ncWidgetListViewController (mkSelector "editing") retCULong []

-- | @- setEditing:@
setEditing :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> Bool -> IO ()
setEditing ncWidgetListViewController  value =
    sendMsg ncWidgetListViewController (mkSelector "setEditing:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsAddButtonWhenEditing@
showsAddButtonWhenEditing :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> IO Bool
showsAddButtonWhenEditing ncWidgetListViewController  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ncWidgetListViewController (mkSelector "showsAddButtonWhenEditing") retCULong []

-- | @- setShowsAddButtonWhenEditing:@
setShowsAddButtonWhenEditing :: IsNCWidgetListViewController ncWidgetListViewController => ncWidgetListViewController -> Bool -> IO ()
setShowsAddButtonWhenEditing ncWidgetListViewController  value =
    sendMsg ncWidgetListViewController (mkSelector "setShowsAddButtonWhenEditing:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @viewControllerAtRow:makeIfNecessary:@
viewControllerAtRow_makeIfNecessarySelector :: Selector
viewControllerAtRow_makeIfNecessarySelector = mkSelector "viewControllerAtRow:makeIfNecessary:"

-- | @Selector@ for @rowForViewController:@
rowForViewControllerSelector :: Selector
rowForViewControllerSelector = mkSelector "rowForViewController:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @contents@
contentsSelector :: Selector
contentsSelector = mkSelector "contents"

-- | @Selector@ for @setContents:@
setContentsSelector :: Selector
setContentsSelector = mkSelector "setContents:"

-- | @Selector@ for @minimumVisibleRowCount@
minimumVisibleRowCountSelector :: Selector
minimumVisibleRowCountSelector = mkSelector "minimumVisibleRowCount"

-- | @Selector@ for @setMinimumVisibleRowCount:@
setMinimumVisibleRowCountSelector :: Selector
setMinimumVisibleRowCountSelector = mkSelector "setMinimumVisibleRowCount:"

-- | @Selector@ for @hasDividerLines@
hasDividerLinesSelector :: Selector
hasDividerLinesSelector = mkSelector "hasDividerLines"

-- | @Selector@ for @setHasDividerLines:@
setHasDividerLinesSelector :: Selector
setHasDividerLinesSelector = mkSelector "setHasDividerLines:"

-- | @Selector@ for @editing@
editingSelector :: Selector
editingSelector = mkSelector "editing"

-- | @Selector@ for @setEditing:@
setEditingSelector :: Selector
setEditingSelector = mkSelector "setEditing:"

-- | @Selector@ for @showsAddButtonWhenEditing@
showsAddButtonWhenEditingSelector :: Selector
showsAddButtonWhenEditingSelector = mkSelector "showsAddButtonWhenEditing"

-- | @Selector@ for @setShowsAddButtonWhenEditing:@
setShowsAddButtonWhenEditingSelector :: Selector
setShowsAddButtonWhenEditingSelector = mkSelector "setShowsAddButtonWhenEditing:"

