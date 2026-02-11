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
  , sharedImageEditPanelSelector
  , reloadDataSelector
  , dataSourceSelector
  , setDataSourceSelector
  , filterArraySelector


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
    sendClassMsg cls' (mkSelector "sharedImageEditPanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | reloadData
--
-- Reloads the data from the data associated with an image editing panel.
--
-- ObjC selector: @- reloadData@
reloadData :: IsIKImageEditPanel ikImageEditPanel => ikImageEditPanel -> IO ()
reloadData ikImageEditPanel  =
    sendMsg ikImageEditPanel (mkSelector "reloadData") retVoid []

-- | dataSource
--
-- Data source associated with an image editing panel
--
-- ObjC selector: @- dataSource@
dataSource :: IsIKImageEditPanel ikImageEditPanel => ikImageEditPanel -> IO RawId
dataSource ikImageEditPanel  =
    fmap (RawId . castPtr) $ sendMsg ikImageEditPanel (mkSelector "dataSource") (retPtr retVoid) []

-- | dataSource
--
-- Data source associated with an image editing panel
--
-- ObjC selector: @- setDataSource:@
setDataSource :: IsIKImageEditPanel ikImageEditPanel => ikImageEditPanel -> RawId -> IO ()
setDataSource ikImageEditPanel  value =
    sendMsg ikImageEditPanel (mkSelector "setDataSource:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | filterArray
--
-- Array of filters reflecting the current user adjustments in the adjust or effects tab.
--
-- ObjC selector: @- filterArray@
filterArray :: IsIKImageEditPanel ikImageEditPanel => ikImageEditPanel -> IO (Id NSArray)
filterArray ikImageEditPanel  =
    sendMsg ikImageEditPanel (mkSelector "filterArray") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedImageEditPanel@
sharedImageEditPanelSelector :: Selector
sharedImageEditPanelSelector = mkSelector "sharedImageEditPanel"

-- | @Selector@ for @reloadData@
reloadDataSelector :: Selector
reloadDataSelector = mkSelector "reloadData"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector
dataSourceSelector = mkSelector "dataSource"

-- | @Selector@ for @setDataSource:@
setDataSourceSelector :: Selector
setDataSourceSelector = mkSelector "setDataSource:"

-- | @Selector@ for @filterArray@
filterArraySelector :: Selector
filterArraySelector = mkSelector "filterArray"

