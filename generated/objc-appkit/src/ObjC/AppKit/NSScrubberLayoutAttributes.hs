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
  , layoutAttributesForItemAtIndexSelector
  , itemIndexSelector
  , setItemIndexSelector
  , frameSelector
  , setFrameSelector
  , alphaSelector
  , setAlphaSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
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
    sendClassMsg cls' (mkSelector "layoutAttributesForItemAtIndex:") (retPtr retVoid) [argCLong (fromIntegral index)] >>= retainedObject . castPtr

-- | @- itemIndex@
itemIndex :: IsNSScrubberLayoutAttributes nsScrubberLayoutAttributes => nsScrubberLayoutAttributes -> IO CLong
itemIndex nsScrubberLayoutAttributes  =
  sendMsg nsScrubberLayoutAttributes (mkSelector "itemIndex") retCLong []

-- | @- setItemIndex:@
setItemIndex :: IsNSScrubberLayoutAttributes nsScrubberLayoutAttributes => nsScrubberLayoutAttributes -> CLong -> IO ()
setItemIndex nsScrubberLayoutAttributes  value =
  sendMsg nsScrubberLayoutAttributes (mkSelector "setItemIndex:") retVoid [argCLong (fromIntegral value)]

-- | @- frame@
frame :: IsNSScrubberLayoutAttributes nsScrubberLayoutAttributes => nsScrubberLayoutAttributes -> IO NSRect
frame nsScrubberLayoutAttributes  =
  sendMsgStret nsScrubberLayoutAttributes (mkSelector "frame") retNSRect []

-- | @- setFrame:@
setFrame :: IsNSScrubberLayoutAttributes nsScrubberLayoutAttributes => nsScrubberLayoutAttributes -> NSRect -> IO ()
setFrame nsScrubberLayoutAttributes  value =
  sendMsg nsScrubberLayoutAttributes (mkSelector "setFrame:") retVoid [argNSRect value]

-- | @- alpha@
alpha :: IsNSScrubberLayoutAttributes nsScrubberLayoutAttributes => nsScrubberLayoutAttributes -> IO CDouble
alpha nsScrubberLayoutAttributes  =
  sendMsg nsScrubberLayoutAttributes (mkSelector "alpha") retCDouble []

-- | @- setAlpha:@
setAlpha :: IsNSScrubberLayoutAttributes nsScrubberLayoutAttributes => nsScrubberLayoutAttributes -> CDouble -> IO ()
setAlpha nsScrubberLayoutAttributes  value =
  sendMsg nsScrubberLayoutAttributes (mkSelector "setAlpha:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layoutAttributesForItemAtIndex:@
layoutAttributesForItemAtIndexSelector :: Selector
layoutAttributesForItemAtIndexSelector = mkSelector "layoutAttributesForItemAtIndex:"

-- | @Selector@ for @itemIndex@
itemIndexSelector :: Selector
itemIndexSelector = mkSelector "itemIndex"

-- | @Selector@ for @setItemIndex:@
setItemIndexSelector :: Selector
setItemIndexSelector = mkSelector "setItemIndex:"

-- | @Selector@ for @frame@
frameSelector :: Selector
frameSelector = mkSelector "frame"

-- | @Selector@ for @setFrame:@
setFrameSelector :: Selector
setFrameSelector = mkSelector "setFrame:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector
setAlphaSelector = mkSelector "setAlpha:"

