{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @QuartzFilterManager@.
module ObjC.Quartz.QuartzFilterManager
  ( QuartzFilterManager
  , IsQuartzFilterManager(..)
  , filterManager
  , filtersInDomains
  , filterPanel
  , filterView
  , selectedFilter
  , selectFilter
  , setDelegate
  , delegate
  , importFilter
  , filterManagerSelector
  , filtersInDomainsSelector
  , filterPanelSelector
  , filterViewSelector
  , selectedFilterSelector
  , selectFilterSelector
  , setDelegateSelector
  , delegateSelector
  , importFilterSelector


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

-- | @+ filterManager@
filterManager :: IO (Id QuartzFilterManager)
filterManager  =
  do
    cls' <- getRequiredClass "QuartzFilterManager"
    sendClassMsg cls' (mkSelector "filterManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ filtersInDomains:@
filtersInDomains :: IsNSArray domains => domains -> IO (Id NSArray)
filtersInDomains domains =
  do
    cls' <- getRequiredClass "QuartzFilterManager"
    withObjCPtr domains $ \raw_domains ->
      sendClassMsg cls' (mkSelector "filtersInDomains:") (retPtr retVoid) [argPtr (castPtr raw_domains :: Ptr ())] >>= retainedObject . castPtr

-- | @- filterPanel@
filterPanel :: IsQuartzFilterManager quartzFilterManager => quartzFilterManager -> IO (Id NSPanel)
filterPanel quartzFilterManager  =
  sendMsg quartzFilterManager (mkSelector "filterPanel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- filterView@
filterView :: IsQuartzFilterManager quartzFilterManager => quartzFilterManager -> IO (Id QuartzFilterView)
filterView quartzFilterManager  =
  sendMsg quartzFilterManager (mkSelector "filterView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- selectedFilter@
selectedFilter :: IsQuartzFilterManager quartzFilterManager => quartzFilterManager -> IO (Id QuartzFilter)
selectedFilter quartzFilterManager  =
  sendMsg quartzFilterManager (mkSelector "selectedFilter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- selectFilter:@
selectFilter :: (IsQuartzFilterManager quartzFilterManager, IsQuartzFilter filter_) => quartzFilterManager -> filter_ -> IO Bool
selectFilter quartzFilterManager  filter_ =
withObjCPtr filter_ $ \raw_filter_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg quartzFilterManager (mkSelector "selectFilter:") retCULong [argPtr (castPtr raw_filter_ :: Ptr ())]

-- | @- setDelegate:@
setDelegate :: IsQuartzFilterManager quartzFilterManager => quartzFilterManager -> RawId -> IO ()
setDelegate quartzFilterManager  aDelegate =
  sendMsg quartzFilterManager (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId aDelegate) :: Ptr ())]

-- | @- delegate@
delegate :: IsQuartzFilterManager quartzFilterManager => quartzFilterManager -> IO RawId
delegate quartzFilterManager  =
  fmap (RawId . castPtr) $ sendMsg quartzFilterManager (mkSelector "delegate") (retPtr retVoid) []

-- | @- importFilter:@
importFilter :: (IsQuartzFilterManager quartzFilterManager, IsNSDictionary filterProperties) => quartzFilterManager -> filterProperties -> IO (Id QuartzFilter)
importFilter quartzFilterManager  filterProperties =
withObjCPtr filterProperties $ \raw_filterProperties ->
    sendMsg quartzFilterManager (mkSelector "importFilter:") (retPtr retVoid) [argPtr (castPtr raw_filterProperties :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @filterManager@
filterManagerSelector :: Selector
filterManagerSelector = mkSelector "filterManager"

-- | @Selector@ for @filtersInDomains:@
filtersInDomainsSelector :: Selector
filtersInDomainsSelector = mkSelector "filtersInDomains:"

-- | @Selector@ for @filterPanel@
filterPanelSelector :: Selector
filterPanelSelector = mkSelector "filterPanel"

-- | @Selector@ for @filterView@
filterViewSelector :: Selector
filterViewSelector = mkSelector "filterView"

-- | @Selector@ for @selectedFilter@
selectedFilterSelector :: Selector
selectedFilterSelector = mkSelector "selectedFilter"

-- | @Selector@ for @selectFilter:@
selectFilterSelector :: Selector
selectFilterSelector = mkSelector "selectFilter:"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @importFilter:@
importFilterSelector :: Selector
importFilterSelector = mkSelector "importFilter:"

