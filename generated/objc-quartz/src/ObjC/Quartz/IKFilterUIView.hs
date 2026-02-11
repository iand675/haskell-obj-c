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
  , viewWithFrame_filterSelector
  , initWithFrame_filterSelector
  , filterSelector
  , objectControllerSelector


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
    withObjCPtr inFilter $ \raw_inFilter ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "viewWithFrame:filter:") (retPtr retVoid) [argNSRect frameRect, argPtr (castPtr raw_inFilter :: Ptr ())]

-- | initWithFrame:filter:
--
-- The initWithFrame method initializes a view that retains the filter passed into it.
--
-- ObjC selector: @- initWithFrame:filter:@
initWithFrame_filter :: (IsIKFilterUIView ikFilterUIView, IsCIFilter inFilter) => ikFilterUIView -> NSRect -> inFilter -> IO RawId
initWithFrame_filter ikFilterUIView  frameRect inFilter =
withObjCPtr inFilter $ \raw_inFilter ->
    fmap (RawId . castPtr) $ sendMsg ikFilterUIView (mkSelector "initWithFrame:filter:") (retPtr retVoid) [argNSRect frameRect, argPtr (castPtr raw_inFilter :: Ptr ())]

-- | filter
--
-- Accessor method to return the filter instance that the view controls.
--
-- ObjC selector: @- filter@
filter_ :: IsIKFilterUIView ikFilterUIView => ikFilterUIView -> IO (Id CIFilter)
filter_ ikFilterUIView  =
  sendMsg ikFilterUIView (mkSelector "filter") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | objectController
--
-- Accessor method for the object controller for all bindings between the filter and the UI representation.
--
-- ObjC selector: @- objectController@
objectController :: IsIKFilterUIView ikFilterUIView => ikFilterUIView -> IO (Id NSObjectController)
objectController ikFilterUIView  =
  sendMsg ikFilterUIView (mkSelector "objectController") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @viewWithFrame:filter:@
viewWithFrame_filterSelector :: Selector
viewWithFrame_filterSelector = mkSelector "viewWithFrame:filter:"

-- | @Selector@ for @initWithFrame:filter:@
initWithFrame_filterSelector :: Selector
initWithFrame_filterSelector = mkSelector "initWithFrame:filter:"

-- | @Selector@ for @filter@
filterSelector :: Selector
filterSelector = mkSelector "filter"

-- | @Selector@ for @objectController@
objectControllerSelector :: Selector
objectControllerSelector = mkSelector "objectController"

