{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , calculationModeSelector
  , setCalculationModeSelector
  , setTargetsSelector
  , setUnifiesNormalsSelector
  , setWeight_forTargetAtIndexSelector
  , setWeight_forTargetNamedSelector
  , setWeightsSelector
  , targetsSelector
  , unifiesNormalsSelector
  , weightForTargetAtIndexSelector
  , weightForTargetNamedSelector
  , weightsSelector

  -- * Enum types
  , SCNMorpherCalculationMode(SCNMorpherCalculationMode)
  , pattern SCNMorpherCalculationModeNormalized
  , pattern SCNMorpherCalculationModeAdditive

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
setWeight_forTargetAtIndex scnMorpher weight targetIndex =
  sendMessage scnMorpher setWeight_forTargetAtIndexSelector weight targetIndex

-- | weightForTargetAtIndex:
--
-- Retrieves the weight for the target at the specified index.
--
-- ObjC selector: @- weightForTargetAtIndex:@
weightForTargetAtIndex :: IsSCNMorpher scnMorpher => scnMorpher -> CULong -> IO CDouble
weightForTargetAtIndex scnMorpher targetIndex =
  sendMessage scnMorpher weightForTargetAtIndexSelector targetIndex

-- | setWeight:forTargetNamed:
--
-- Sets the weight for the target with the specified name (targetName is the name of the target geometry).
--
-- ObjC selector: @- setWeight:forTargetNamed:@
setWeight_forTargetNamed :: (IsSCNMorpher scnMorpher, IsNSString targetName) => scnMorpher -> CDouble -> targetName -> IO ()
setWeight_forTargetNamed scnMorpher weight targetName =
  sendMessage scnMorpher setWeight_forTargetNamedSelector weight (toNSString targetName)

-- | weightForTargetNamed:
--
-- Retrieves the weight for the target with the specified name (targetName is the name of the target geometry).
--
-- ObjC selector: @- weightForTargetNamed:@
weightForTargetNamed :: (IsSCNMorpher scnMorpher, IsNSString targetName) => scnMorpher -> targetName -> IO CDouble
weightForTargetNamed scnMorpher targetName =
  sendMessage scnMorpher weightForTargetNamedSelector (toNSString targetName)

-- | targets
--
-- Specifies the morph targets as an array of SCNGeometry.
--
-- The target geometries must have the same number of entries in their geometry sources and the same topology as the base geometry.
--
-- ObjC selector: @- targets@
targets :: IsSCNMorpher scnMorpher => scnMorpher -> IO (Id NSArray)
targets scnMorpher =
  sendMessage scnMorpher targetsSelector

-- | targets
--
-- Specifies the morph targets as an array of SCNGeometry.
--
-- The target geometries must have the same number of entries in their geometry sources and the same topology as the base geometry.
--
-- ObjC selector: @- setTargets:@
setTargets :: (IsSCNMorpher scnMorpher, IsNSArray value) => scnMorpher -> value -> IO ()
setTargets scnMorpher value =
  sendMessage scnMorpher setTargetsSelector (toNSArray value)

-- | weights
--
-- Access to all the weights of all the targets.
--
-- ObjC selector: @- weights@
weights :: IsSCNMorpher scnMorpher => scnMorpher -> IO (Id NSArray)
weights scnMorpher =
  sendMessage scnMorpher weightsSelector

-- | weights
--
-- Access to all the weights of all the targets.
--
-- ObjC selector: @- setWeights:@
setWeights :: (IsSCNMorpher scnMorpher, IsNSArray value) => scnMorpher -> value -> IO ()
setWeights scnMorpher value =
  sendMessage scnMorpher setWeightsSelector (toNSArray value)

-- | calculationMode
--
-- Specifies how the morph result is calculated by the receiver. Defaults to SCNMorpherCalculationModeNormalized.
--
-- ObjC selector: @- calculationMode@
calculationMode :: IsSCNMorpher scnMorpher => scnMorpher -> IO SCNMorpherCalculationMode
calculationMode scnMorpher =
  sendMessage scnMorpher calculationModeSelector

-- | calculationMode
--
-- Specifies how the morph result is calculated by the receiver. Defaults to SCNMorpherCalculationModeNormalized.
--
-- ObjC selector: @- setCalculationMode:@
setCalculationMode :: IsSCNMorpher scnMorpher => scnMorpher -> SCNMorpherCalculationMode -> IO ()
setCalculationMode scnMorpher value =
  sendMessage scnMorpher setCalculationModeSelector value

-- | unifiesNormals
--
-- When set to YES the normals are not morphed but are recomputed after morphing the vertex instead. When set to NO, the morpher will morph the normals if the geometry targets have normals. Defaults to NO.
--
-- ObjC selector: @- unifiesNormals@
unifiesNormals :: IsSCNMorpher scnMorpher => scnMorpher -> IO Bool
unifiesNormals scnMorpher =
  sendMessage scnMorpher unifiesNormalsSelector

-- | unifiesNormals
--
-- When set to YES the normals are not morphed but are recomputed after morphing the vertex instead. When set to NO, the morpher will morph the normals if the geometry targets have normals. Defaults to NO.
--
-- ObjC selector: @- setUnifiesNormals:@
setUnifiesNormals :: IsSCNMorpher scnMorpher => scnMorpher -> Bool -> IO ()
setUnifiesNormals scnMorpher value =
  sendMessage scnMorpher setUnifiesNormalsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setWeight:forTargetAtIndex:@
setWeight_forTargetAtIndexSelector :: Selector '[CDouble, CULong] ()
setWeight_forTargetAtIndexSelector = mkSelector "setWeight:forTargetAtIndex:"

-- | @Selector@ for @weightForTargetAtIndex:@
weightForTargetAtIndexSelector :: Selector '[CULong] CDouble
weightForTargetAtIndexSelector = mkSelector "weightForTargetAtIndex:"

-- | @Selector@ for @setWeight:forTargetNamed:@
setWeight_forTargetNamedSelector :: Selector '[CDouble, Id NSString] ()
setWeight_forTargetNamedSelector = mkSelector "setWeight:forTargetNamed:"

-- | @Selector@ for @weightForTargetNamed:@
weightForTargetNamedSelector :: Selector '[Id NSString] CDouble
weightForTargetNamedSelector = mkSelector "weightForTargetNamed:"

-- | @Selector@ for @targets@
targetsSelector :: Selector '[] (Id NSArray)
targetsSelector = mkSelector "targets"

-- | @Selector@ for @setTargets:@
setTargetsSelector :: Selector '[Id NSArray] ()
setTargetsSelector = mkSelector "setTargets:"

-- | @Selector@ for @weights@
weightsSelector :: Selector '[] (Id NSArray)
weightsSelector = mkSelector "weights"

-- | @Selector@ for @setWeights:@
setWeightsSelector :: Selector '[Id NSArray] ()
setWeightsSelector = mkSelector "setWeights:"

-- | @Selector@ for @calculationMode@
calculationModeSelector :: Selector '[] SCNMorpherCalculationMode
calculationModeSelector = mkSelector "calculationMode"

-- | @Selector@ for @setCalculationMode:@
setCalculationModeSelector :: Selector '[SCNMorpherCalculationMode] ()
setCalculationModeSelector = mkSelector "setCalculationMode:"

-- | @Selector@ for @unifiesNormals@
unifiesNormalsSelector :: Selector '[] Bool
unifiesNormalsSelector = mkSelector "unifiesNormals"

-- | @Selector@ for @setUnifiesNormals:@
setUnifiesNormalsSelector :: Selector '[Bool] ()
setUnifiesNormalsSelector = mkSelector "setUnifiesNormals:"

