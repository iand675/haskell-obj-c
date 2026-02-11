{-# LANGUAGE PatternSynonyms #-}
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
  , url
  , setURL
  , doubleAction
  , setDoubleAction
  , pathStyle
  , setPathStyle
  , backgroundColor
  , setBackgroundColor
  , menu
  , setMenu
  , setDraggingSourceOperationMask_forLocalSelector
  , clickedPathComponentCellSelector
  , pathComponentCellsSelector
  , setPathComponentCellsSelector
  , editableSelector
  , setEditableSelector
  , urlSelector
  , setURLSelector
  , doubleActionSelector
  , setDoubleActionSelector
  , pathStyleSelector
  , setPathStyleSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , menuSelector
  , setMenuSelector

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

-- | @- setDraggingSourceOperationMask:forLocal:@
setDraggingSourceOperationMask_forLocal :: IsNSPathControl nsPathControl => nsPathControl -> NSDragOperation -> Bool -> IO ()
setDraggingSourceOperationMask_forLocal nsPathControl  mask isLocal =
  sendMsg nsPathControl (mkSelector "setDraggingSourceOperationMask:forLocal:") retVoid [argCULong (coerce mask), argCULong (if isLocal then 1 else 0)]

-- | @- clickedPathComponentCell@
clickedPathComponentCell :: IsNSPathControl nsPathControl => nsPathControl -> IO (Id NSPathComponentCell)
clickedPathComponentCell nsPathControl  =
  sendMsg nsPathControl (mkSelector "clickedPathComponentCell") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pathComponentCells@
pathComponentCells :: IsNSPathControl nsPathControl => nsPathControl -> IO (Id NSArray)
pathComponentCells nsPathControl  =
  sendMsg nsPathControl (mkSelector "pathComponentCells") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPathComponentCells:@
setPathComponentCells :: (IsNSPathControl nsPathControl, IsNSArray cells) => nsPathControl -> cells -> IO ()
setPathComponentCells nsPathControl  cells =
withObjCPtr cells $ \raw_cells ->
    sendMsg nsPathControl (mkSelector "setPathComponentCells:") retVoid [argPtr (castPtr raw_cells :: Ptr ())]

-- | @- editable@
editable :: IsNSPathControl nsPathControl => nsPathControl -> IO Bool
editable nsPathControl  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPathControl (mkSelector "editable") retCULong []

-- | @- setEditable:@
setEditable :: IsNSPathControl nsPathControl => nsPathControl -> Bool -> IO ()
setEditable nsPathControl  value =
  sendMsg nsPathControl (mkSelector "setEditable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- URL@
url :: IsNSPathControl nsPathControl => nsPathControl -> IO (Id NSURL)
url nsPathControl  =
  sendMsg nsPathControl (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setURL:@
setURL :: (IsNSPathControl nsPathControl, IsNSURL value) => nsPathControl -> value -> IO ()
setURL nsPathControl  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPathControl (mkSelector "setURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- doubleAction@
doubleAction :: IsNSPathControl nsPathControl => nsPathControl -> IO Selector
doubleAction nsPathControl  =
  fmap (Selector . castPtr) $ sendMsg nsPathControl (mkSelector "doubleAction") (retPtr retVoid) []

-- | @- setDoubleAction:@
setDoubleAction :: IsNSPathControl nsPathControl => nsPathControl -> Selector -> IO ()
setDoubleAction nsPathControl  value =
  sendMsg nsPathControl (mkSelector "setDoubleAction:") retVoid [argPtr (unSelector value)]

-- | @- pathStyle@
pathStyle :: IsNSPathControl nsPathControl => nsPathControl -> IO NSPathStyle
pathStyle nsPathControl  =
  fmap (coerce :: CLong -> NSPathStyle) $ sendMsg nsPathControl (mkSelector "pathStyle") retCLong []

-- | @- setPathStyle:@
setPathStyle :: IsNSPathControl nsPathControl => nsPathControl -> NSPathStyle -> IO ()
setPathStyle nsPathControl  value =
  sendMsg nsPathControl (mkSelector "setPathStyle:") retVoid [argCLong (coerce value)]

-- | @- backgroundColor@
backgroundColor :: IsNSPathControl nsPathControl => nsPathControl -> IO (Id NSColor)
backgroundColor nsPathControl  =
  sendMsg nsPathControl (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSPathControl nsPathControl, IsNSColor value) => nsPathControl -> value -> IO ()
setBackgroundColor nsPathControl  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPathControl (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- menu@
menu :: IsNSPathControl nsPathControl => nsPathControl -> IO (Id NSMenu)
menu nsPathControl  =
  sendMsg nsPathControl (mkSelector "menu") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMenu:@
setMenu :: (IsNSPathControl nsPathControl, IsNSMenu value) => nsPathControl -> value -> IO ()
setMenu nsPathControl  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPathControl (mkSelector "setMenu:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setDraggingSourceOperationMask:forLocal:@
setDraggingSourceOperationMask_forLocalSelector :: Selector
setDraggingSourceOperationMask_forLocalSelector = mkSelector "setDraggingSourceOperationMask:forLocal:"

-- | @Selector@ for @clickedPathComponentCell@
clickedPathComponentCellSelector :: Selector
clickedPathComponentCellSelector = mkSelector "clickedPathComponentCell"

-- | @Selector@ for @pathComponentCells@
pathComponentCellsSelector :: Selector
pathComponentCellsSelector = mkSelector "pathComponentCells"

-- | @Selector@ for @setPathComponentCells:@
setPathComponentCellsSelector :: Selector
setPathComponentCellsSelector = mkSelector "setPathComponentCells:"

-- | @Selector@ for @editable@
editableSelector :: Selector
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector
setURLSelector = mkSelector "setURL:"

-- | @Selector@ for @doubleAction@
doubleActionSelector :: Selector
doubleActionSelector = mkSelector "doubleAction"

-- | @Selector@ for @setDoubleAction:@
setDoubleActionSelector :: Selector
setDoubleActionSelector = mkSelector "setDoubleAction:"

-- | @Selector@ for @pathStyle@
pathStyleSelector :: Selector
pathStyleSelector = mkSelector "pathStyle"

-- | @Selector@ for @setPathStyle:@
setPathStyleSelector :: Selector
setPathStyleSelector = mkSelector "setPathStyle:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @menu@
menuSelector :: Selector
menuSelector = mkSelector "menu"

-- | @Selector@ for @setMenu:@
setMenuSelector :: Selector
setMenuSelector = mkSelector "setMenu:"

