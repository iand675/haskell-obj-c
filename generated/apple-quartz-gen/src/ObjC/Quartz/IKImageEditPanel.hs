{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IKImageEditPanel
--
-- The IKImageEditPanel class provides a panel, that is, a utility window that floats on top of document windows, optimized for image editing.
--
-- Generated bindings for @IKImageEditPanel@.
module ObjC.Quartz.IKImageEditPanel
  ( IKImageEditPanel
  , IsIKImageEditPanel(..)
  , sharedImageEditPanel
  , reloadData
  , dataSource
  , setDataSource
  , filterArray
  , dataSourceSelector
  , filterArraySelector
  , reloadDataSelector
  , setDataSourceSelector
  , sharedImageEditPanelSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Quartz.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | sharedImageEditPanel
--
-- Creates a shared instance of an image editing panel.
--
-- ObjC selector: @+ sharedImageEditPanel@
sharedImageEditPanel :: IO (Id IKImageEditPanel)
sharedImageEditPanel  =
  do
    cls' <- getRequiredClass "IKImageEditPanel"
    sendClassMessage cls' sharedImageEditPanelSelector

-- | reloadData
--
-- Reloads the data from the data associated with an image editing panel.
--
-- ObjC selector: @- reloadData@
reloadData :: IsIKImageEditPanel ikImageEditPanel => ikImageEditPanel -> IO ()
reloadData ikImageEditPanel =
  sendMessage ikImageEditPanel reloadDataSelector

-- | dataSource
--
-- Data source associated with an image editing panel
--
-- ObjC selector: @- dataSource@
dataSource :: IsIKImageEditPanel ikImageEditPanel => ikImageEditPanel -> IO RawId
dataSource ikImageEditPanel =
  sendMessage ikImageEditPanel dataSourceSelector

-- | dataSource
--
-- Data source associated with an image editing panel
--
-- ObjC selector: @- setDataSource:@
setDataSource :: IsIKImageEditPanel ikImageEditPanel => ikImageEditPanel -> RawId -> IO ()
setDataSource ikImageEditPanel value =
  sendMessage ikImageEditPanel setDataSourceSelector value

-- | filterArray
--
-- Array of filters reflecting the current user adjustments in the adjust or effects tab.
--
-- ObjC selector: @- filterArray@
filterArray :: IsIKImageEditPanel ikImageEditPanel => ikImageEditPanel -> IO (Id NSArray)
filterArray ikImageEditPanel =
  sendMessage ikImageEditPanel filterArraySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedImageEditPanel@
sharedImageEditPanelSelector :: Selector '[] (Id IKImageEditPanel)
sharedImageEditPanelSelector = mkSelector "sharedImageEditPanel"

-- | @Selector@ for @reloadData@
reloadDataSelector :: Selector '[] ()
reloadDataSelector = mkSelector "reloadData"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector '[] RawId
dataSourceSelector = mkSelector "dataSource"

-- | @Selector@ for @setDataSource:@
setDataSourceSelector :: Selector '[RawId] ()
setDataSourceSelector = mkSelector "setDataSource:"

-- | @Selector@ for @filterArray@
filterArraySelector :: Selector '[] (Id NSArray)
filterArraySelector = mkSelector "filterArray"

