{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTableHeaderView@.
module ObjC.AppKit.NSTableHeaderView
  ( NSTableHeaderView
  , IsNSTableHeaderView(..)
  , headerRectOfColumn
  , columnAtPoint
  , tableView
  , setTableView
  , draggedColumn
  , draggedDistance
  , resizedColumn
  , headerRectOfColumnSelector
  , columnAtPointSelector
  , tableViewSelector
  , setTableViewSelector
  , draggedColumnSelector
  , draggedDistanceSelector
  , resizedColumnSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- headerRectOfColumn:@
headerRectOfColumn :: IsNSTableHeaderView nsTableHeaderView => nsTableHeaderView -> CLong -> IO NSRect
headerRectOfColumn nsTableHeaderView  column =
  sendMsgStret nsTableHeaderView (mkSelector "headerRectOfColumn:") retNSRect [argCLong (fromIntegral column)]

-- | @- columnAtPoint:@
columnAtPoint :: IsNSTableHeaderView nsTableHeaderView => nsTableHeaderView -> NSPoint -> IO CLong
columnAtPoint nsTableHeaderView  point =
  sendMsg nsTableHeaderView (mkSelector "columnAtPoint:") retCLong [argNSPoint point]

-- | @- tableView@
tableView :: IsNSTableHeaderView nsTableHeaderView => nsTableHeaderView -> IO (Id NSTableView)
tableView nsTableHeaderView  =
  sendMsg nsTableHeaderView (mkSelector "tableView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTableView:@
setTableView :: (IsNSTableHeaderView nsTableHeaderView, IsNSTableView value) => nsTableHeaderView -> value -> IO ()
setTableView nsTableHeaderView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTableHeaderView (mkSelector "setTableView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- draggedColumn@
draggedColumn :: IsNSTableHeaderView nsTableHeaderView => nsTableHeaderView -> IO CLong
draggedColumn nsTableHeaderView  =
  sendMsg nsTableHeaderView (mkSelector "draggedColumn") retCLong []

-- | @- draggedDistance@
draggedDistance :: IsNSTableHeaderView nsTableHeaderView => nsTableHeaderView -> IO CDouble
draggedDistance nsTableHeaderView  =
  sendMsg nsTableHeaderView (mkSelector "draggedDistance") retCDouble []

-- | @- resizedColumn@
resizedColumn :: IsNSTableHeaderView nsTableHeaderView => nsTableHeaderView -> IO CLong
resizedColumn nsTableHeaderView  =
  sendMsg nsTableHeaderView (mkSelector "resizedColumn") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @headerRectOfColumn:@
headerRectOfColumnSelector :: Selector
headerRectOfColumnSelector = mkSelector "headerRectOfColumn:"

-- | @Selector@ for @columnAtPoint:@
columnAtPointSelector :: Selector
columnAtPointSelector = mkSelector "columnAtPoint:"

-- | @Selector@ for @tableView@
tableViewSelector :: Selector
tableViewSelector = mkSelector "tableView"

-- | @Selector@ for @setTableView:@
setTableViewSelector :: Selector
setTableViewSelector = mkSelector "setTableView:"

-- | @Selector@ for @draggedColumn@
draggedColumnSelector :: Selector
draggedColumnSelector = mkSelector "draggedColumn"

-- | @Selector@ for @draggedDistance@
draggedDistanceSelector :: Selector
draggedDistanceSelector = mkSelector "draggedDistance"

-- | @Selector@ for @resizedColumn@
resizedColumnSelector :: Selector
resizedColumnSelector = mkSelector "resizedColumn"

