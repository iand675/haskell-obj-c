{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @IKFilterUIView@.
module ObjC.Quartz.IKFilterUIView
  ( IKFilterUIView
  , IsIKFilterUIView(..)
  , viewWithFrame_filter
  , initWithFrame_filter
  , filter_
  , objectController
  , filterSelector
  , initWithFrame_filterSelector
  , objectControllerSelector
  , viewWithFrame_filterSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Quartz.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Classes
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | viewWithFrame:filter:
--
-- The viewWithFrame method creates a view that retains the filter passed into it.
--
-- ObjC selector: @+ viewWithFrame:filter:@
viewWithFrame_filter :: IsCIFilter inFilter => NSRect -> inFilter -> IO RawId
viewWithFrame_filter frameRect inFilter =
  do
    cls' <- getRequiredClass "IKFilterUIView"
    sendClassMessage cls' viewWithFrame_filterSelector frameRect (toCIFilter inFilter)

-- | initWithFrame:filter:
--
-- The initWithFrame method initializes a view that retains the filter passed into it.
--
-- ObjC selector: @- initWithFrame:filter:@
initWithFrame_filter :: (IsIKFilterUIView ikFilterUIView, IsCIFilter inFilter) => ikFilterUIView -> NSRect -> inFilter -> IO RawId
initWithFrame_filter ikFilterUIView frameRect inFilter =
  sendOwnedMessage ikFilterUIView initWithFrame_filterSelector frameRect (toCIFilter inFilter)

-- | filter
--
-- Accessor method to return the filter instance that the view controls.
--
-- ObjC selector: @- filter@
filter_ :: IsIKFilterUIView ikFilterUIView => ikFilterUIView -> IO (Id CIFilter)
filter_ ikFilterUIView =
  sendMessage ikFilterUIView filterSelector

-- | objectController
--
-- Accessor method for the object controller for all bindings between the filter and the UI representation.
--
-- ObjC selector: @- objectController@
objectController :: IsIKFilterUIView ikFilterUIView => ikFilterUIView -> IO (Id NSObjectController)
objectController ikFilterUIView =
  sendMessage ikFilterUIView objectControllerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @viewWithFrame:filter:@
viewWithFrame_filterSelector :: Selector '[NSRect, Id CIFilter] RawId
viewWithFrame_filterSelector = mkSelector "viewWithFrame:filter:"

-- | @Selector@ for @initWithFrame:filter:@
initWithFrame_filterSelector :: Selector '[NSRect, Id CIFilter] RawId
initWithFrame_filterSelector = mkSelector "initWithFrame:filter:"

-- | @Selector@ for @filter@
filterSelector :: Selector '[] (Id CIFilter)
filterSelector = mkSelector "filter"

-- | @Selector@ for @objectController@
objectControllerSelector :: Selector '[] (Id NSObjectController)
objectControllerSelector = mkSelector "objectController"

