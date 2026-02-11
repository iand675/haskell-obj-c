{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNMorpher
--
-- SCNMorpher controls the deformation of morphed geometries
--
-- Generated bindings for @SCNMorpher@.
module ObjC.SceneKit.SCNMorpher
  ( SCNMorpher
  , IsSCNMorpher(..)
  , setWeight_forTargetAtIndex
  , weightForTargetAtIndex
  , setWeight_forTargetNamed
  , weightForTargetNamed
  , targets
  , setTargets
  , weights
  , setWeights
  , calculationMode
  , setCalculationMode
  , unifiesNormals
  , setUnifiesNormals
  , setWeight_forTargetAtIndexSelector
  , weightForTargetAtIndexSelector
  , setWeight_forTargetNamedSelector
  , weightForTargetNamedSelector
  , targetsSelector
  , setTargetsSelector
  , weightsSelector
  , setWeightsSelector
  , calculationModeSelector
  , setCalculationModeSelector
  , unifiesNormalsSelector
  , setUnifiesNormalsSelector

  -- * Enum types
  , SCNMorpherCalculationMode(SCNMorpherCalculationMode)
  , pattern SCNMorpherCalculationModeNormalized
  , pattern SCNMorpherCalculationModeAdditive

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

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | setWeight:forTargetAtIndex:
--
-- Sets the weight for the target at the specified index. Animatable implicitly or explicitly with the keyPath "weights[index]" or "weights["targetName"]" (targetName is the name of the target geometry).
--
-- ObjC selector: @- setWeight:forTargetAtIndex:@
setWeight_forTargetAtIndex :: IsSCNMorpher scnMorpher => scnMorpher -> CDouble -> CULong -> IO ()
setWeight_forTargetAtIndex scnMorpher  weight targetIndex =
  sendMsg scnMorpher (mkSelector "setWeight:forTargetAtIndex:") retVoid [argCDouble (fromIntegral weight), argCULong (fromIntegral targetIndex)]

-- | weightForTargetAtIndex:
--
-- Retrieves the weight for the target at the specified index.
--
-- ObjC selector: @- weightForTargetAtIndex:@
weightForTargetAtIndex :: IsSCNMorpher scnMorpher => scnMorpher -> CULong -> IO CDouble
weightForTargetAtIndex scnMorpher  targetIndex =
  sendMsg scnMorpher (mkSelector "weightForTargetAtIndex:") retCDouble [argCULong (fromIntegral targetIndex)]

-- | setWeight:forTargetNamed:
--
-- Sets the weight for the target with the specified name (targetName is the name of the target geometry).
--
-- ObjC selector: @- setWeight:forTargetNamed:@
setWeight_forTargetNamed :: (IsSCNMorpher scnMorpher, IsNSString targetName) => scnMorpher -> CDouble -> targetName -> IO ()
setWeight_forTargetNamed scnMorpher  weight targetName =
withObjCPtr targetName $ \raw_targetName ->
    sendMsg scnMorpher (mkSelector "setWeight:forTargetNamed:") retVoid [argCDouble (fromIntegral weight), argPtr (castPtr raw_targetName :: Ptr ())]

-- | weightForTargetNamed:
--
-- Retrieves the weight for the target with the specified name (targetName is the name of the target geometry).
--
-- ObjC selector: @- weightForTargetNamed:@
weightForTargetNamed :: (IsSCNMorpher scnMorpher, IsNSString targetName) => scnMorpher -> targetName -> IO CDouble
weightForTargetNamed scnMorpher  targetName =
withObjCPtr targetName $ \raw_targetName ->
    sendMsg scnMorpher (mkSelector "weightForTargetNamed:") retCDouble [argPtr (castPtr raw_targetName :: Ptr ())]

-- | targets
--
-- Specifies the morph targets as an array of SCNGeometry.
--
-- The target geometries must have the same number of entries in their geometry sources and the same topology as the base geometry.
--
-- ObjC selector: @- targets@
targets :: IsSCNMorpher scnMorpher => scnMorpher -> IO (Id NSArray)
targets scnMorpher  =
  sendMsg scnMorpher (mkSelector "targets") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | targets
--
-- Specifies the morph targets as an array of SCNGeometry.
--
-- The target geometries must have the same number of entries in their geometry sources and the same topology as the base geometry.
--
-- ObjC selector: @- setTargets:@
setTargets :: (IsSCNMorpher scnMorpher, IsNSArray value) => scnMorpher -> value -> IO ()
setTargets scnMorpher  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnMorpher (mkSelector "setTargets:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | weights
--
-- Access to all the weights of all the targets.
--
-- ObjC selector: @- weights@
weights :: IsSCNMorpher scnMorpher => scnMorpher -> IO (Id NSArray)
weights scnMorpher  =
  sendMsg scnMorpher (mkSelector "weights") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | weights
--
-- Access to all the weights of all the targets.
--
-- ObjC selector: @- setWeights:@
setWeights :: (IsSCNMorpher scnMorpher, IsNSArray value) => scnMorpher -> value -> IO ()
setWeights scnMorpher  value =
withObjCPtr value $ \raw_value ->
    sendMsg scnMorpher (mkSelector "setWeights:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | calculationMode
--
-- Specifies how the morph result is calculated by the receiver. Defaults to SCNMorpherCalculationModeNormalized.
--
-- ObjC selector: @- calculationMode@
calculationMode :: IsSCNMorpher scnMorpher => scnMorpher -> IO SCNMorpherCalculationMode
calculationMode scnMorpher  =
  fmap (coerce :: CLong -> SCNMorpherCalculationMode) $ sendMsg scnMorpher (mkSelector "calculationMode") retCLong []

-- | calculationMode
--
-- Specifies how the morph result is calculated by the receiver. Defaults to SCNMorpherCalculationModeNormalized.
--
-- ObjC selector: @- setCalculationMode:@
setCalculationMode :: IsSCNMorpher scnMorpher => scnMorpher -> SCNMorpherCalculationMode -> IO ()
setCalculationMode scnMorpher  value =
  sendMsg scnMorpher (mkSelector "setCalculationMode:") retVoid [argCLong (coerce value)]

-- | unifiesNormals
--
-- When set to YES the normals are not morphed but are recomputed after morphing the vertex instead. When set to NO, the morpher will morph the normals if the geometry targets have normals. Defaults to NO.
--
-- ObjC selector: @- unifiesNormals@
unifiesNormals :: IsSCNMorpher scnMorpher => scnMorpher -> IO Bool
unifiesNormals scnMorpher  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnMorpher (mkSelector "unifiesNormals") retCULong []

-- | unifiesNormals
--
-- When set to YES the normals are not morphed but are recomputed after morphing the vertex instead. When set to NO, the morpher will morph the normals if the geometry targets have normals. Defaults to NO.
--
-- ObjC selector: @- setUnifiesNormals:@
setUnifiesNormals :: IsSCNMorpher scnMorpher => scnMorpher -> Bool -> IO ()
setUnifiesNormals scnMorpher  value =
  sendMsg scnMorpher (mkSelector "setUnifiesNormals:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setWeight:forTargetAtIndex:@
setWeight_forTargetAtIndexSelector :: Selector
setWeight_forTargetAtIndexSelector = mkSelector "setWeight:forTargetAtIndex:"

-- | @Selector@ for @weightForTargetAtIndex:@
weightForTargetAtIndexSelector :: Selector
weightForTargetAtIndexSelector = mkSelector "weightForTargetAtIndex:"

-- | @Selector@ for @setWeight:forTargetNamed:@
setWeight_forTargetNamedSelector :: Selector
setWeight_forTargetNamedSelector = mkSelector "setWeight:forTargetNamed:"

-- | @Selector@ for @weightForTargetNamed:@
weightForTargetNamedSelector :: Selector
weightForTargetNamedSelector = mkSelector "weightForTargetNamed:"

-- | @Selector@ for @targets@
targetsSelector :: Selector
targetsSelector = mkSelector "targets"

-- | @Selector@ for @setTargets:@
setTargetsSelector :: Selector
setTargetsSelector = mkSelector "setTargets:"

-- | @Selector@ for @weights@
weightsSelector :: Selector
weightsSelector = mkSelector "weights"

-- | @Selector@ for @setWeights:@
setWeightsSelector :: Selector
setWeightsSelector = mkSelector "setWeights:"

-- | @Selector@ for @calculationMode@
calculationModeSelector :: Selector
calculationModeSelector = mkSelector "calculationMode"

-- | @Selector@ for @setCalculationMode:@
setCalculationModeSelector :: Selector
setCalculationModeSelector = mkSelector "setCalculationMode:"

-- | @Selector@ for @unifiesNormals@
unifiesNormalsSelector :: Selector
unifiesNormalsSelector = mkSelector "unifiesNormals"

-- | @Selector@ for @setUnifiesNormals:@
setUnifiesNormalsSelector :: Selector
setUnifiesNormalsSelector = mkSelector "setUnifiesNormals:"

