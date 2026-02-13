{-# LANGUAGE DataKinds #-}
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
  , delegateSelector
  , filterManagerSelector
  , filterPanelSelector
  , filterViewSelector
  , filtersInDomainsSelector
  , importFilterSelector
  , selectFilterSelector
  , selectedFilterSelector
  , setDelegateSelector


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

-- | @+ filterManager@
filterManager :: IO (Id QuartzFilterManager)
filterManager  =
  do
    cls' <- getRequiredClass "QuartzFilterManager"
    sendClassMessage cls' filterManagerSelector

-- | @+ filtersInDomains:@
filtersInDomains :: IsNSArray domains => domains -> IO (Id NSArray)
filtersInDomains domains =
  do
    cls' <- getRequiredClass "QuartzFilterManager"
    sendClassMessage cls' filtersInDomainsSelector (toNSArray domains)

-- | @- filterPanel@
filterPanel :: IsQuartzFilterManager quartzFilterManager => quartzFilterManager -> IO (Id NSPanel)
filterPanel quartzFilterManager =
  sendMessage quartzFilterManager filterPanelSelector

-- | @- filterView@
filterView :: IsQuartzFilterManager quartzFilterManager => quartzFilterManager -> IO (Id QuartzFilterView)
filterView quartzFilterManager =
  sendMessage quartzFilterManager filterViewSelector

-- | @- selectedFilter@
selectedFilter :: IsQuartzFilterManager quartzFilterManager => quartzFilterManager -> IO (Id QuartzFilter)
selectedFilter quartzFilterManager =
  sendMessage quartzFilterManager selectedFilterSelector

-- | @- selectFilter:@
selectFilter :: (IsQuartzFilterManager quartzFilterManager, IsQuartzFilter filter_) => quartzFilterManager -> filter_ -> IO Bool
selectFilter quartzFilterManager filter_ =
  sendMessage quartzFilterManager selectFilterSelector (toQuartzFilter filter_)

-- | @- setDelegate:@
setDelegate :: IsQuartzFilterManager quartzFilterManager => quartzFilterManager -> RawId -> IO ()
setDelegate quartzFilterManager aDelegate =
  sendMessage quartzFilterManager setDelegateSelector aDelegate

-- | @- delegate@
delegate :: IsQuartzFilterManager quartzFilterManager => quartzFilterManager -> IO RawId
delegate quartzFilterManager =
  sendMessage quartzFilterManager delegateSelector

-- | @- importFilter:@
importFilter :: (IsQuartzFilterManager quartzFilterManager, IsNSDictionary filterProperties) => quartzFilterManager -> filterProperties -> IO (Id QuartzFilter)
importFilter quartzFilterManager filterProperties =
  sendMessage quartzFilterManager importFilterSelector (toNSDictionary filterProperties)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @filterManager@
filterManagerSelector :: Selector '[] (Id QuartzFilterManager)
filterManagerSelector = mkSelector "filterManager"

-- | @Selector@ for @filtersInDomains:@
filtersInDomainsSelector :: Selector '[Id NSArray] (Id NSArray)
filtersInDomainsSelector = mkSelector "filtersInDomains:"

-- | @Selector@ for @filterPanel@
filterPanelSelector :: Selector '[] (Id NSPanel)
filterPanelSelector = mkSelector "filterPanel"

-- | @Selector@ for @filterView@
filterViewSelector :: Selector '[] (Id QuartzFilterView)
filterViewSelector = mkSelector "filterView"

-- | @Selector@ for @selectedFilter@
selectedFilterSelector :: Selector '[] (Id QuartzFilter)
selectedFilterSelector = mkSelector "selectedFilter"

-- | @Selector@ for @selectFilter:@
selectFilterSelector :: Selector '[Id QuartzFilter] Bool
selectFilterSelector = mkSelector "selectFilter:"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @importFilter:@
importFilterSelector :: Selector '[Id NSDictionary] (Id QuartzFilter)
importFilterSelector = mkSelector "importFilter:"

