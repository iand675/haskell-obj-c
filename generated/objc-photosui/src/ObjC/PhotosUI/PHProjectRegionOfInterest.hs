{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | In PHProjectAssetElement objects, an array of PHProjectRegionOfInterest objects may be provided. These regions represent specific areas in an asset that have signficant meaning. For example, faces that are relevant to the user (as opposed to faces in a crowd) will be highlighted in the asset to help with things like auto-pan, auto-zoom, or focusing on specific areas in the asset during animations or  transitions. Regions representing the same person or object across multiple assets are cross-referenced through the use of the identifier.
--
-- Generated bindings for @PHProjectRegionOfInterest@.
module ObjC.PhotosUI.PHProjectRegionOfInterest
  ( PHProjectRegionOfInterest
  , IsPHProjectRegionOfInterest(..)
  , init_
  , new
  , weight
  , quality
  , identifier
  , initSelector
  , newSelector
  , weightSelector
  , qualitySelector
  , identifierSelector


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

import ObjC.PhotosUI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHProjectRegionOfInterest phProjectRegionOfInterest => phProjectRegionOfInterest -> IO (Id PHProjectRegionOfInterest)
init_ phProjectRegionOfInterest  =
  sendMsg phProjectRegionOfInterest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHProjectRegionOfInterest)
new  =
  do
    cls' <- getRequiredClass "PHProjectRegionOfInterest"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Significance of the regionOfInterest in the overall project context is provided as a weight score. All regions of interest with the same identifier in the project have the same weight. For projects doing things like animation or transition between assets, focusing on the highest weighted regions of interest will ensure that the presentation represents something that is most meaningful to the user. Value range is a double between 0.0 and 1.0. Default is 0.5.
--
-- ObjC selector: @- weight@
weight :: IsPHProjectRegionOfInterest phProjectRegionOfInterest => phProjectRegionOfInterest -> IO CDouble
weight phProjectRegionOfInterest  =
  sendMsg phProjectRegionOfInterest (mkSelector "weight") retCDouble []

-- | Quality of the represented region of interest in the asset. Different regions of interest with the same identifier may have different quality values. If the project wants to decide between multiple assets containing the same region of interest, the quality score can be used to pick the best representation of the region of interest. Value range is a double between 0.0 and 1.0.
--
-- ObjC selector: @- quality@
quality :: IsPHProjectRegionOfInterest phProjectRegionOfInterest => phProjectRegionOfInterest -> IO CDouble
quality phProjectRegionOfInterest  =
  sendMsg phProjectRegionOfInterest (mkSelector "quality") retCDouble []

-- | Identifier of the region of interest. Regions representing the same person or object will have the same identifier across multiple assets.
--
-- ObjC selector: @- identifier@
identifier :: IsPHProjectRegionOfInterest phProjectRegionOfInterest => phProjectRegionOfInterest -> IO (Id NSString)
identifier phProjectRegionOfInterest  =
  sendMsg phProjectRegionOfInterest (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @weight@
weightSelector :: Selector
weightSelector = mkSelector "weight"

-- | @Selector@ for @quality@
qualitySelector :: Selector
qualitySelector = mkSelector "quality"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

