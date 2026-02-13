{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSScrubberProportionalLayout
--
-- @NSScrubberProportionalLayout@ is a concrete layout object that sizes each item to some fraction of the scrubber's visible size.
--
-- Generated bindings for @NSScrubberProportionalLayout@.
module ObjC.AppKit.NSScrubberProportionalLayout
  ( NSScrubberProportionalLayout
  , IsNSScrubberProportionalLayout(..)
  , initWithNumberOfVisibleItems
  , initWithCoder
  , numberOfVisibleItems
  , setNumberOfVisibleItems
  , initWithCoderSelector
  , initWithNumberOfVisibleItemsSelector
  , numberOfVisibleItemsSelector
  , setNumberOfVisibleItemsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithNumberOfVisibleItems:@
initWithNumberOfVisibleItems :: IsNSScrubberProportionalLayout nsScrubberProportionalLayout => nsScrubberProportionalLayout -> CLong -> IO (Id NSScrubberProportionalLayout)
initWithNumberOfVisibleItems nsScrubberProportionalLayout numberOfVisibleItems =
  sendOwnedMessage nsScrubberProportionalLayout initWithNumberOfVisibleItemsSelector numberOfVisibleItems

-- | @- initWithCoder:@
initWithCoder :: (IsNSScrubberProportionalLayout nsScrubberProportionalLayout, IsNSCoder coder) => nsScrubberProportionalLayout -> coder -> IO (Id NSScrubberProportionalLayout)
initWithCoder nsScrubberProportionalLayout coder =
  sendOwnedMessage nsScrubberProportionalLayout initWithCoderSelector (toNSCoder coder)

-- | The number of items that should fit within the scrubber's viewport at once.
--
-- ObjC selector: @- numberOfVisibleItems@
numberOfVisibleItems :: IsNSScrubberProportionalLayout nsScrubberProportionalLayout => nsScrubberProportionalLayout -> IO CLong
numberOfVisibleItems nsScrubberProportionalLayout =
  sendMessage nsScrubberProportionalLayout numberOfVisibleItemsSelector

-- | The number of items that should fit within the scrubber's viewport at once.
--
-- ObjC selector: @- setNumberOfVisibleItems:@
setNumberOfVisibleItems :: IsNSScrubberProportionalLayout nsScrubberProportionalLayout => nsScrubberProportionalLayout -> CLong -> IO ()
setNumberOfVisibleItems nsScrubberProportionalLayout value =
  sendMessage nsScrubberProportionalLayout setNumberOfVisibleItemsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNumberOfVisibleItems:@
initWithNumberOfVisibleItemsSelector :: Selector '[CLong] (Id NSScrubberProportionalLayout)
initWithNumberOfVisibleItemsSelector = mkSelector "initWithNumberOfVisibleItems:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSScrubberProportionalLayout)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @numberOfVisibleItems@
numberOfVisibleItemsSelector :: Selector '[] CLong
numberOfVisibleItemsSelector = mkSelector "numberOfVisibleItems"

-- | @Selector@ for @setNumberOfVisibleItems:@
setNumberOfVisibleItemsSelector :: Selector '[CLong] ()
setNumberOfVisibleItemsSelector = mkSelector "setNumberOfVisibleItems:"

