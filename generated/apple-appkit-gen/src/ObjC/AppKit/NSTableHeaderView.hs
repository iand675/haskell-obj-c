{-# LANGUAGE DataKinds #-}
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
  , columnAtPointSelector
  , draggedColumnSelector
  , draggedDistanceSelector
  , headerRectOfColumnSelector
  , resizedColumnSelector
  , setTableViewSelector
  , tableViewSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- headerRectOfColumn:@
headerRectOfColumn :: IsNSTableHeaderView nsTableHeaderView => nsTableHeaderView -> CLong -> IO NSRect
headerRectOfColumn nsTableHeaderView column =
  sendMessage nsTableHeaderView headerRectOfColumnSelector column

-- | @- columnAtPoint:@
columnAtPoint :: IsNSTableHeaderView nsTableHeaderView => nsTableHeaderView -> NSPoint -> IO CLong
columnAtPoint nsTableHeaderView point =
  sendMessage nsTableHeaderView columnAtPointSelector point

-- | @- tableView@
tableView :: IsNSTableHeaderView nsTableHeaderView => nsTableHeaderView -> IO (Id NSTableView)
tableView nsTableHeaderView =
  sendMessage nsTableHeaderView tableViewSelector

-- | @- setTableView:@
setTableView :: (IsNSTableHeaderView nsTableHeaderView, IsNSTableView value) => nsTableHeaderView -> value -> IO ()
setTableView nsTableHeaderView value =
  sendMessage nsTableHeaderView setTableViewSelector (toNSTableView value)

-- | @- draggedColumn@
draggedColumn :: IsNSTableHeaderView nsTableHeaderView => nsTableHeaderView -> IO CLong
draggedColumn nsTableHeaderView =
  sendMessage nsTableHeaderView draggedColumnSelector

-- | @- draggedDistance@
draggedDistance :: IsNSTableHeaderView nsTableHeaderView => nsTableHeaderView -> IO CDouble
draggedDistance nsTableHeaderView =
  sendMessage nsTableHeaderView draggedDistanceSelector

-- | @- resizedColumn@
resizedColumn :: IsNSTableHeaderView nsTableHeaderView => nsTableHeaderView -> IO CLong
resizedColumn nsTableHeaderView =
  sendMessage nsTableHeaderView resizedColumnSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @headerRectOfColumn:@
headerRectOfColumnSelector :: Selector '[CLong] NSRect
headerRectOfColumnSelector = mkSelector "headerRectOfColumn:"

-- | @Selector@ for @columnAtPoint:@
columnAtPointSelector :: Selector '[NSPoint] CLong
columnAtPointSelector = mkSelector "columnAtPoint:"

-- | @Selector@ for @tableView@
tableViewSelector :: Selector '[] (Id NSTableView)
tableViewSelector = mkSelector "tableView"

-- | @Selector@ for @setTableView:@
setTableViewSelector :: Selector '[Id NSTableView] ()
setTableViewSelector = mkSelector "setTableView:"

-- | @Selector@ for @draggedColumn@
draggedColumnSelector :: Selector '[] CLong
draggedColumnSelector = mkSelector "draggedColumn"

-- | @Selector@ for @draggedDistance@
draggedDistanceSelector :: Selector '[] CDouble
draggedDistanceSelector = mkSelector "draggedDistance"

-- | @Selector@ for @resizedColumn@
resizedColumnSelector :: Selector '[] CLong
resizedColumnSelector = mkSelector "resizedColumn"

