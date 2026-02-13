{-# LANGUAGE DataKinds #-}
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
  , initWithPathSelector
  , initWithRadiusSelector
  , inverseRegionSelector
  , pathSelector
  , regionByDifferenceFromRegionSelector
  , regionByIntersectionWithRegionSelector
  , regionByUnionWithRegionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' infiniteRegionSelector

-- | Create a circular region with radius
--
-- ObjC selector: @- initWithRadius:@
initWithRadius :: IsSKRegion skRegion => skRegion -> CFloat -> IO (Id SKRegion)
initWithRadius skRegion radius =
  sendOwnedMessage skRegion initWithRadiusSelector radius

-- | Create a region bounded by a CGPath. Note that this option can be    costly to evaluate.
--
-- ObjC selector: @- initWithPath:@
initWithPath :: IsSKRegion skRegion => skRegion -> RawId -> IO (Id SKRegion)
initWithPath skRegion path =
  sendOwnedMessage skRegion initWithPathSelector path

-- | Create a new region that is the inverse of the current region.    The inverse of the infiniteRegion is an empty region.    Subclasses of SKRegion need to provide an implementation of inverseRegion.
--
-- ObjC selector: @- inverseRegion@
inverseRegion :: IsSKRegion skRegion => skRegion -> IO (Id SKRegion)
inverseRegion skRegion =
  sendMessage skRegion inverseRegionSelector

-- | Create a new region that is the original region plus the supplied region
--
-- ObjC selector: @- regionByUnionWithRegion:@
regionByUnionWithRegion :: (IsSKRegion skRegion, IsSKRegion region) => skRegion -> region -> IO (Id SKRegion)
regionByUnionWithRegion skRegion region =
  sendMessage skRegion regionByUnionWithRegionSelector (toSKRegion region)

-- | Create a new region that is the original region minus the supplied region
--
-- ObjC selector: @- regionByDifferenceFromRegion:@
regionByDifferenceFromRegion :: (IsSKRegion skRegion, IsSKRegion region) => skRegion -> region -> IO (Id SKRegion)
regionByDifferenceFromRegion skRegion region =
  sendMessage skRegion regionByDifferenceFromRegionSelector (toSKRegion region)

-- | Create a new region that is the region covered by the original region and the supplied region
--
-- ObjC selector: @- regionByIntersectionWithRegion:@
regionByIntersectionWithRegion :: (IsSKRegion skRegion, IsSKRegion region) => skRegion -> region -> IO (Id SKRegion)
regionByIntersectionWithRegion skRegion region =
  sendMessage skRegion regionByIntersectionWithRegionSelector (toSKRegion region)

-- | @- path@
path :: IsSKRegion skRegion => skRegion -> IO RawId
path skRegion =
  sendMessage skRegion pathSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @infiniteRegion@
infiniteRegionSelector :: Selector '[] (Id SKRegion)
infiniteRegionSelector = mkSelector "infiniteRegion"

-- | @Selector@ for @initWithRadius:@
initWithRadiusSelector :: Selector '[CFloat] (Id SKRegion)
initWithRadiusSelector = mkSelector "initWithRadius:"

-- | @Selector@ for @initWithPath:@
initWithPathSelector :: Selector '[RawId] (Id SKRegion)
initWithPathSelector = mkSelector "initWithPath:"

-- | @Selector@ for @inverseRegion@
inverseRegionSelector :: Selector '[] (Id SKRegion)
inverseRegionSelector = mkSelector "inverseRegion"

-- | @Selector@ for @regionByUnionWithRegion:@
regionByUnionWithRegionSelector :: Selector '[Id SKRegion] (Id SKRegion)
regionByUnionWithRegionSelector = mkSelector "regionByUnionWithRegion:"

-- | @Selector@ for @regionByDifferenceFromRegion:@
regionByDifferenceFromRegionSelector :: Selector '[Id SKRegion] (Id SKRegion)
regionByDifferenceFromRegionSelector = mkSelector "regionByDifferenceFromRegion:"

-- | @Selector@ for @regionByIntersectionWithRegion:@
regionByIntersectionWithRegionSelector :: Selector '[Id SKRegion] (Id SKRegion)
regionByIntersectionWithRegionSelector = mkSelector "regionByIntersectionWithRegion:"

-- | @Selector@ for @path@
pathSelector :: Selector '[] RawId
pathSelector = mkSelector "path"

