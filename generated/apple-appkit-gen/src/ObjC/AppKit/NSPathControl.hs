{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPathControl@.
module ObjC.AppKit.NSPathControl
  ( NSPathControl
  , IsNSPathControl(..)
  , setDraggingSourceOperationMask_forLocal
  , clickedPathComponentCell
  , pathComponentCells
  , setPathComponentCells
  , editable
  , setEditable
  , allowedTypes
  , setAllowedTypes
  , placeholderString
  , setPlaceholderString
  , placeholderAttributedString
  , setPlaceholderAttributedString
  , url
  , setURL
  , doubleAction
  , setDoubleAction
  , pathStyle
  , setPathStyle
  , clickedPathItem
  , pathItems
  , setPathItems
  , backgroundColor
  , setBackgroundColor
  , delegate
  , setDelegate
  , menu
  , setMenu
  , allowedTypesSelector
  , backgroundColorSelector
  , clickedPathComponentCellSelector
  , clickedPathItemSelector
  , delegateSelector
  , doubleActionSelector
  , editableSelector
  , menuSelector
  , pathComponentCellsSelector
  , pathItemsSelector
  , pathStyleSelector
  , placeholderAttributedStringSelector
  , placeholderStringSelector
  , setAllowedTypesSelector
  , setBackgroundColorSelector
  , setDelegateSelector
  , setDoubleActionSelector
  , setDraggingSourceOperationMask_forLocalSelector
  , setEditableSelector
  , setMenuSelector
  , setPathComponentCellsSelector
  , setPathItemsSelector
  , setPathStyleSelector
  , setPlaceholderAttributedStringSelector
  , setPlaceholderStringSelector
  , setURLSelector
  , urlSelector

  -- * Enum types
  , NSDragOperation(NSDragOperation)
  , pattern NSDragOperationNone
  , pattern NSDragOperationCopy
  , pattern NSDragOperationLink
  , pattern NSDragOperationGeneric
  , pattern NSDragOperationPrivate
  , pattern NSDragOperationMove
  , pattern NSDragOperationDelete
  , pattern NSDragOperationEvery
  , pattern NSDragOperationAll_Obsolete
  , pattern NSDragOperationAll
  , NSPathStyle(NSPathStyle)
  , pattern NSPathStyleStandard
  , pattern NSPathStylePopUp
  , pattern NSPathStyleNavigationBar

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

-- | @- setDraggingSourceOperationMask:forLocal:@
setDraggingSourceOperationMask_forLocal :: IsNSPathControl nsPathControl => nsPathControl -> NSDragOperation -> Bool -> IO ()
setDraggingSourceOperationMask_forLocal nsPathControl mask isLocal =
  sendMessage nsPathControl setDraggingSourceOperationMask_forLocalSelector mask isLocal

-- | @- clickedPathComponentCell@
clickedPathComponentCell :: IsNSPathControl nsPathControl => nsPathControl -> IO (Id NSPathComponentCell)
clickedPathComponentCell nsPathControl =
  sendMessage nsPathControl clickedPathComponentCellSelector

-- | @- pathComponentCells@
pathComponentCells :: IsNSPathControl nsPathControl => nsPathControl -> IO (Id NSArray)
pathComponentCells nsPathControl =
  sendMessage nsPathControl pathComponentCellsSelector

-- | @- setPathComponentCells:@
setPathComponentCells :: (IsNSPathControl nsPathControl, IsNSArray cells) => nsPathControl -> cells -> IO ()
setPathComponentCells nsPathControl cells =
  sendMessage nsPathControl setPathComponentCellsSelector (toNSArray cells)

-- | @- editable@
editable :: IsNSPathControl nsPathControl => nsPathControl -> IO Bool
editable nsPathControl =
  sendMessage nsPathControl editableSelector

-- | @- setEditable:@
setEditable :: IsNSPathControl nsPathControl => nsPathControl -> Bool -> IO ()
setEditable nsPathControl value =
  sendMessage nsPathControl setEditableSelector value

-- | @- allowedTypes@
allowedTypes :: IsNSPathControl nsPathControl => nsPathControl -> IO (Id NSArray)
allowedTypes nsPathControl =
  sendMessage nsPathControl allowedTypesSelector

-- | @- setAllowedTypes:@
setAllowedTypes :: (IsNSPathControl nsPathControl, IsNSArray value) => nsPathControl -> value -> IO ()
setAllowedTypes nsPathControl value =
  sendMessage nsPathControl setAllowedTypesSelector (toNSArray value)

-- | @- placeholderString@
placeholderString :: IsNSPathControl nsPathControl => nsPathControl -> IO (Id NSString)
placeholderString nsPathControl =
  sendMessage nsPathControl placeholderStringSelector

-- | @- setPlaceholderString:@
setPlaceholderString :: (IsNSPathControl nsPathControl, IsNSString value) => nsPathControl -> value -> IO ()
setPlaceholderString nsPathControl value =
  sendMessage nsPathControl setPlaceholderStringSelector (toNSString value)

-- | @- placeholderAttributedString@
placeholderAttributedString :: IsNSPathControl nsPathControl => nsPathControl -> IO (Id NSAttributedString)
placeholderAttributedString nsPathControl =
  sendMessage nsPathControl placeholderAttributedStringSelector

-- | @- setPlaceholderAttributedString:@
setPlaceholderAttributedString :: (IsNSPathControl nsPathControl, IsNSAttributedString value) => nsPathControl -> value -> IO ()
setPlaceholderAttributedString nsPathControl value =
  sendMessage nsPathControl setPlaceholderAttributedStringSelector (toNSAttributedString value)

-- | @- URL@
url :: IsNSPathControl nsPathControl => nsPathControl -> IO (Id NSURL)
url nsPathControl =
  sendMessage nsPathControl urlSelector

-- | @- setURL:@
setURL :: (IsNSPathControl nsPathControl, IsNSURL value) => nsPathControl -> value -> IO ()
setURL nsPathControl value =
  sendMessage nsPathControl setURLSelector (toNSURL value)

-- | @- doubleAction@
doubleAction :: IsNSPathControl nsPathControl => nsPathControl -> IO Sel
doubleAction nsPathControl =
  sendMessage nsPathControl doubleActionSelector

-- | @- setDoubleAction:@
setDoubleAction :: IsNSPathControl nsPathControl => nsPathControl -> Sel -> IO ()
setDoubleAction nsPathControl value =
  sendMessage nsPathControl setDoubleActionSelector value

-- | @- pathStyle@
pathStyle :: IsNSPathControl nsPathControl => nsPathControl -> IO NSPathStyle
pathStyle nsPathControl =
  sendMessage nsPathControl pathStyleSelector

-- | @- setPathStyle:@
setPathStyle :: IsNSPathControl nsPathControl => nsPathControl -> NSPathStyle -> IO ()
setPathStyle nsPathControl value =
  sendMessage nsPathControl setPathStyleSelector value

-- | @- clickedPathItem@
clickedPathItem :: IsNSPathControl nsPathControl => nsPathControl -> IO (Id NSPathControlItem)
clickedPathItem nsPathControl =
  sendMessage nsPathControl clickedPathItemSelector

-- | @- pathItems@
pathItems :: IsNSPathControl nsPathControl => nsPathControl -> IO (Id NSArray)
pathItems nsPathControl =
  sendMessage nsPathControl pathItemsSelector

-- | @- setPathItems:@
setPathItems :: (IsNSPathControl nsPathControl, IsNSArray value) => nsPathControl -> value -> IO ()
setPathItems nsPathControl value =
  sendMessage nsPathControl setPathItemsSelector (toNSArray value)

-- | @- backgroundColor@
backgroundColor :: IsNSPathControl nsPathControl => nsPathControl -> IO (Id NSColor)
backgroundColor nsPathControl =
  sendMessage nsPathControl backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSPathControl nsPathControl, IsNSColor value) => nsPathControl -> value -> IO ()
setBackgroundColor nsPathControl value =
  sendMessage nsPathControl setBackgroundColorSelector (toNSColor value)

-- | @- delegate@
delegate :: IsNSPathControl nsPathControl => nsPathControl -> IO RawId
delegate nsPathControl =
  sendMessage nsPathControl delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSPathControl nsPathControl => nsPathControl -> RawId -> IO ()
setDelegate nsPathControl value =
  sendMessage nsPathControl setDelegateSelector value

-- | @- menu@
menu :: IsNSPathControl nsPathControl => nsPathControl -> IO (Id NSMenu)
menu nsPathControl =
  sendMessage nsPathControl menuSelector

-- | @- setMenu:@
setMenu :: (IsNSPathControl nsPathControl, IsNSMenu value) => nsPathControl -> value -> IO ()
setMenu nsPathControl value =
  sendMessage nsPathControl setMenuSelector (toNSMenu value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setDraggingSourceOperationMask:forLocal:@
setDraggingSourceOperationMask_forLocalSelector :: Selector '[NSDragOperation, Bool] ()
setDraggingSourceOperationMask_forLocalSelector = mkSelector "setDraggingSourceOperationMask:forLocal:"

-- | @Selector@ for @clickedPathComponentCell@
clickedPathComponentCellSelector :: Selector '[] (Id NSPathComponentCell)
clickedPathComponentCellSelector = mkSelector "clickedPathComponentCell"

-- | @Selector@ for @pathComponentCells@
pathComponentCellsSelector :: Selector '[] (Id NSArray)
pathComponentCellsSelector = mkSelector "pathComponentCells"

-- | @Selector@ for @setPathComponentCells:@
setPathComponentCellsSelector :: Selector '[Id NSArray] ()
setPathComponentCellsSelector = mkSelector "setPathComponentCells:"

-- | @Selector@ for @editable@
editableSelector :: Selector '[] Bool
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector '[Bool] ()
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @allowedTypes@
allowedTypesSelector :: Selector '[] (Id NSArray)
allowedTypesSelector = mkSelector "allowedTypes"

-- | @Selector@ for @setAllowedTypes:@
setAllowedTypesSelector :: Selector '[Id NSArray] ()
setAllowedTypesSelector = mkSelector "setAllowedTypes:"

-- | @Selector@ for @placeholderString@
placeholderStringSelector :: Selector '[] (Id NSString)
placeholderStringSelector = mkSelector "placeholderString"

-- | @Selector@ for @setPlaceholderString:@
setPlaceholderStringSelector :: Selector '[Id NSString] ()
setPlaceholderStringSelector = mkSelector "setPlaceholderString:"

-- | @Selector@ for @placeholderAttributedString@
placeholderAttributedStringSelector :: Selector '[] (Id NSAttributedString)
placeholderAttributedStringSelector = mkSelector "placeholderAttributedString"

-- | @Selector@ for @setPlaceholderAttributedString:@
setPlaceholderAttributedStringSelector :: Selector '[Id NSAttributedString] ()
setPlaceholderAttributedStringSelector = mkSelector "setPlaceholderAttributedString:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector '[Id NSURL] ()
setURLSelector = mkSelector "setURL:"

-- | @Selector@ for @doubleAction@
doubleActionSelector :: Selector '[] Sel
doubleActionSelector = mkSelector "doubleAction"

-- | @Selector@ for @setDoubleAction:@
setDoubleActionSelector :: Selector '[Sel] ()
setDoubleActionSelector = mkSelector "setDoubleAction:"

-- | @Selector@ for @pathStyle@
pathStyleSelector :: Selector '[] NSPathStyle
pathStyleSelector = mkSelector "pathStyle"

-- | @Selector@ for @setPathStyle:@
setPathStyleSelector :: Selector '[NSPathStyle] ()
setPathStyleSelector = mkSelector "setPathStyle:"

-- | @Selector@ for @clickedPathItem@
clickedPathItemSelector :: Selector '[] (Id NSPathControlItem)
clickedPathItemSelector = mkSelector "clickedPathItem"

-- | @Selector@ for @pathItems@
pathItemsSelector :: Selector '[] (Id NSArray)
pathItemsSelector = mkSelector "pathItems"

-- | @Selector@ for @setPathItems:@
setPathItemsSelector :: Selector '[Id NSArray] ()
setPathItemsSelector = mkSelector "setPathItems:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @menu@
menuSelector :: Selector '[] (Id NSMenu)
menuSelector = mkSelector "menu"

-- | @Selector@ for @setMenu:@
setMenuSelector :: Selector '[Id NSMenu] ()
setMenuSelector = mkSelector "setMenu:"

