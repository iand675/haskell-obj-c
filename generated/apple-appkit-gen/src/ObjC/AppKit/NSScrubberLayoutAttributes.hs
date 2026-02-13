{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NSScrubberLayoutAttributes
--
-- @NSScrubberLayoutAttributes@ describes the layout of a single @NSScrubber@ item.
--
-- @NSScrubberLayout@ objects transact in terms of @NSScrubberLayoutAttributes.@ @NSScrubberLayoutAttributes@ can be subclassed if a layout object wants to include more layout information than the base implementation provides. Subclasses of @NSScrubberLayoutAttributes@ must implement @isEqual:,@ @hash,@ and the @NSCopying@ protocol.
--
-- Generated bindings for @NSScrubberLayoutAttributes@.
module ObjC.AppKit.NSScrubberLayoutAttributes
  ( NSScrubberLayoutAttributes
  , IsNSScrubberLayoutAttributes(..)
  , layoutAttributesForItemAtIndex
  , itemIndex
  , setItemIndex
  , frame
  , setFrame
  , alpha
  , setAlpha
  , alphaSelector
  , frameSelector
  , itemIndexSelector
  , layoutAttributesForItemAtIndexSelector
  , setAlphaSelector
  , setFrameSelector
  , setItemIndexSelector


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

-- | @+ layoutAttributesForItemAtIndex:@
layoutAttributesForItemAtIndex :: CLong -> IO (Id NSScrubberLayoutAttributes)
layoutAttributesForItemAtIndex index =
  do
    cls' <- getRequiredClass "NSScrubberLayoutAttributes"
    sendClassMessage cls' layoutAttributesForItemAtIndexSelector index

-- | @- itemIndex@
itemIndex :: IsNSScrubberLayoutAttributes nsScrubberLayoutAttributes => nsScrubberLayoutAttributes -> IO CLong
itemIndex nsScrubberLayoutAttributes =
  sendMessage nsScrubberLayoutAttributes itemIndexSelector

-- | @- setItemIndex:@
setItemIndex :: IsNSScrubberLayoutAttributes nsScrubberLayoutAttributes => nsScrubberLayoutAttributes -> CLong -> IO ()
setItemIndex nsScrubberLayoutAttributes value =
  sendMessage nsScrubberLayoutAttributes setItemIndexSelector value

-- | @- frame@
frame :: IsNSScrubberLayoutAttributes nsScrubberLayoutAttributes => nsScrubberLayoutAttributes -> IO NSRect
frame nsScrubberLayoutAttributes =
  sendMessage nsScrubberLayoutAttributes frameSelector

-- | @- setFrame:@
setFrame :: IsNSScrubberLayoutAttributes nsScrubberLayoutAttributes => nsScrubberLayoutAttributes -> NSRect -> IO ()
setFrame nsScrubberLayoutAttributes value =
  sendMessage nsScrubberLayoutAttributes setFrameSelector value

-- | @- alpha@
alpha :: IsNSScrubberLayoutAttributes nsScrubberLayoutAttributes => nsScrubberLayoutAttributes -> IO CDouble
alpha nsScrubberLayoutAttributes =
  sendMessage nsScrubberLayoutAttributes alphaSelector

-- | @- setAlpha:@
setAlpha :: IsNSScrubberLayoutAttributes nsScrubberLayoutAttributes => nsScrubberLayoutAttributes -> CDouble -> IO ()
setAlpha nsScrubberLayoutAttributes value =
  sendMessage nsScrubberLayoutAttributes setAlphaSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layoutAttributesForItemAtIndex:@
layoutAttributesForItemAtIndexSelector :: Selector '[CLong] (Id NSScrubberLayoutAttributes)
layoutAttributesForItemAtIndexSelector = mkSelector "layoutAttributesForItemAtIndex:"

-- | @Selector@ for @itemIndex@
itemIndexSelector :: Selector '[] CLong
itemIndexSelector = mkSelector "itemIndex"

-- | @Selector@ for @setItemIndex:@
setItemIndexSelector :: Selector '[CLong] ()
setItemIndexSelector = mkSelector "setItemIndex:"

-- | @Selector@ for @frame@
frameSelector :: Selector '[] NSRect
frameSelector = mkSelector "frame"

-- | @Selector@ for @setFrame:@
setFrameSelector :: Selector '[NSRect] ()
setFrameSelector = mkSelector "setFrame:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector '[] CDouble
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector '[CDouble] ()
setAlphaSelector = mkSelector "setAlpha:"

