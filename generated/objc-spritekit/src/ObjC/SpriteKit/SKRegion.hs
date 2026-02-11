{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @SKRegion@.
module ObjC.SpriteKit.SKRegion
  ( SKRegion
  , IsSKRegion(..)
  , infiniteRegion
  , initWithRadius
  , initWithPath
  , inverseRegion
  , regionByUnionWithRegion
  , regionByDifferenceFromRegion
  , regionByIntersectionWithRegion
  , path
  , infiniteRegionSelector
  , initWithRadiusSelector
  , initWithPathSelector
  , inverseRegionSelector
  , regionByUnionWithRegionSelector
  , regionByDifferenceFromRegionSelector
  , regionByIntersectionWithRegionSelector
  , pathSelector


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

import ObjC.SpriteKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A shared infinite region
--
-- ObjC selector: @+ infiniteRegion@
infiniteRegion :: IO (Id SKRegion)
infiniteRegion  =
  do
    cls' <- getRequiredClass "SKRegion"
    sendClassMsg cls' (mkSelector "infiniteRegion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create a circular region with radius
--
-- ObjC selector: @- initWithRadius:@
initWithRadius :: IsSKRegion skRegion => skRegion -> CFloat -> IO (Id SKRegion)
initWithRadius skRegion  radius =
  sendMsg skRegion (mkSelector "initWithRadius:") (retPtr retVoid) [argCFloat (fromIntegral radius)] >>= ownedObject . castPtr

-- | Create a region bounded by a CGPath. Note that this option can be    costly to evaluate.
--
-- ObjC selector: @- initWithPath:@
initWithPath :: IsSKRegion skRegion => skRegion -> RawId -> IO (Id SKRegion)
initWithPath skRegion  path =
  sendMsg skRegion (mkSelector "initWithPath:") (retPtr retVoid) [argPtr (castPtr (unRawId path) :: Ptr ())] >>= ownedObject . castPtr

-- | Create a new region that is the inverse of the current region.    The inverse of the infiniteRegion is an empty region.    Subclasses of SKRegion need to provide an implementation of inverseRegion.
--
-- ObjC selector: @- inverseRegion@
inverseRegion :: IsSKRegion skRegion => skRegion -> IO (Id SKRegion)
inverseRegion skRegion  =
  sendMsg skRegion (mkSelector "inverseRegion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Create a new region that is the original region plus the supplied region
--
-- ObjC selector: @- regionByUnionWithRegion:@
regionByUnionWithRegion :: (IsSKRegion skRegion, IsSKRegion region) => skRegion -> region -> IO (Id SKRegion)
regionByUnionWithRegion skRegion  region =
withObjCPtr region $ \raw_region ->
    sendMsg skRegion (mkSelector "regionByUnionWithRegion:") (retPtr retVoid) [argPtr (castPtr raw_region :: Ptr ())] >>= retainedObject . castPtr

-- | Create a new region that is the original region minus the supplied region
--
-- ObjC selector: @- regionByDifferenceFromRegion:@
regionByDifferenceFromRegion :: (IsSKRegion skRegion, IsSKRegion region) => skRegion -> region -> IO (Id SKRegion)
regionByDifferenceFromRegion skRegion  region =
withObjCPtr region $ \raw_region ->
    sendMsg skRegion (mkSelector "regionByDifferenceFromRegion:") (retPtr retVoid) [argPtr (castPtr raw_region :: Ptr ())] >>= retainedObject . castPtr

-- | Create a new region that is the region covered by the original region and the supplied region
--
-- ObjC selector: @- regionByIntersectionWithRegion:@
regionByIntersectionWithRegion :: (IsSKRegion skRegion, IsSKRegion region) => skRegion -> region -> IO (Id SKRegion)
regionByIntersectionWithRegion skRegion  region =
withObjCPtr region $ \raw_region ->
    sendMsg skRegion (mkSelector "regionByIntersectionWithRegion:") (retPtr retVoid) [argPtr (castPtr raw_region :: Ptr ())] >>= retainedObject . castPtr

-- | @- path@
path :: IsSKRegion skRegion => skRegion -> IO RawId
path skRegion  =
  fmap (RawId . castPtr) $ sendMsg skRegion (mkSelector "path") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @infiniteRegion@
infiniteRegionSelector :: Selector
infiniteRegionSelector = mkSelector "infiniteRegion"

-- | @Selector@ for @initWithRadius:@
initWithRadiusSelector :: Selector
initWithRadiusSelector = mkSelector "initWithRadius:"

-- | @Selector@ for @initWithPath:@
initWithPathSelector :: Selector
initWithPathSelector = mkSelector "initWithPath:"

-- | @Selector@ for @inverseRegion@
inverseRegionSelector :: Selector
inverseRegionSelector = mkSelector "inverseRegion"

-- | @Selector@ for @regionByUnionWithRegion:@
regionByUnionWithRegionSelector :: Selector
regionByUnionWithRegionSelector = mkSelector "regionByUnionWithRegion:"

-- | @Selector@ for @regionByDifferenceFromRegion:@
regionByDifferenceFromRegionSelector :: Selector
regionByDifferenceFromRegionSelector = mkSelector "regionByDifferenceFromRegion:"

-- | @Selector@ for @regionByIntersectionWithRegion:@
regionByIntersectionWithRegionSelector :: Selector
regionByIntersectionWithRegionSelector = mkSelector "regionByIntersectionWithRegion:"

-- | @Selector@ for @path@
pathSelector :: Selector
pathSelector = mkSelector "path"

