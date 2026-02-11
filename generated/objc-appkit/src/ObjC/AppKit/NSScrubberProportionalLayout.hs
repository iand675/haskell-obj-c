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
  , initWithNumberOfVisibleItemsSelector
  , initWithCoderSelector
  , numberOfVisibleItemsSelector
  , setNumberOfVisibleItemsSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- initWithNumberOfVisibleItems:@
initWithNumberOfVisibleItems :: IsNSScrubberProportionalLayout nsScrubberProportionalLayout => nsScrubberProportionalLayout -> CLong -> IO (Id NSScrubberProportionalLayout)
initWithNumberOfVisibleItems nsScrubberProportionalLayout  numberOfVisibleItems =
  sendMsg nsScrubberProportionalLayout (mkSelector "initWithNumberOfVisibleItems:") (retPtr retVoid) [argCLong (fromIntegral numberOfVisibleItems)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSScrubberProportionalLayout nsScrubberProportionalLayout, IsNSCoder coder) => nsScrubberProportionalLayout -> coder -> IO (Id NSScrubberProportionalLayout)
initWithCoder nsScrubberProportionalLayout  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsScrubberProportionalLayout (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | The number of items that should fit within the scrubber's viewport at once.
--
-- ObjC selector: @- numberOfVisibleItems@
numberOfVisibleItems :: IsNSScrubberProportionalLayout nsScrubberProportionalLayout => nsScrubberProportionalLayout -> IO CLong
numberOfVisibleItems nsScrubberProportionalLayout  =
  sendMsg nsScrubberProportionalLayout (mkSelector "numberOfVisibleItems") retCLong []

-- | The number of items that should fit within the scrubber's viewport at once.
--
-- ObjC selector: @- setNumberOfVisibleItems:@
setNumberOfVisibleItems :: IsNSScrubberProportionalLayout nsScrubberProportionalLayout => nsScrubberProportionalLayout -> CLong -> IO ()
setNumberOfVisibleItems nsScrubberProportionalLayout  value =
  sendMsg nsScrubberProportionalLayout (mkSelector "setNumberOfVisibleItems:") retVoid [argCLong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithNumberOfVisibleItems:@
initWithNumberOfVisibleItemsSelector :: Selector
initWithNumberOfVisibleItemsSelector = mkSelector "initWithNumberOfVisibleItems:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @numberOfVisibleItems@
numberOfVisibleItemsSelector :: Selector
numberOfVisibleItemsSelector = mkSelector "numberOfVisibleItems"

-- | @Selector@ for @setNumberOfVisibleItems:@
setNumberOfVisibleItemsSelector :: Selector
setNumberOfVisibleItemsSelector = mkSelector "setNumberOfVisibleItems:"

