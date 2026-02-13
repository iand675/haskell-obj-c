{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | SCNConstraint
--
-- A SCNConstraint is an abstract class that represents a single constraint that can be applied to a node.
--
-- Generated bindings for @SCNConstraint@.
module ObjC.SceneKit.SCNConstraint
  ( SCNConstraint
  , IsSCNConstraint(..)
  , enabled
  , setEnabled
  , influenceFactor
  , setInfluenceFactor
  , incremental
  , setIncremental
  , enabledSelector
  , incrementalSelector
  , influenceFactorSelector
  , setEnabledSelector
  , setIncrementalSelector
  , setInfluenceFactorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | enable
--
-- Determines whether the constraint is enabled or not. Defaults to YES.
--
-- ObjC selector: @- enabled@
enabled :: IsSCNConstraint scnConstraint => scnConstraint -> IO Bool
enabled scnConstraint =
  sendMessage scnConstraint enabledSelector

-- | enable
--
-- Determines whether the constraint is enabled or not. Defaults to YES.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsSCNConstraint scnConstraint => scnConstraint -> Bool -> IO ()
setEnabled scnConstraint value =
  sendMessage scnConstraint setEnabledSelector value

-- | influenceFactor
--
-- Specifies the inflence factor of the receiver. Defaults to 1. Animatable
--
-- ObjC selector: @- influenceFactor@
influenceFactor :: IsSCNConstraint scnConstraint => scnConstraint -> IO CDouble
influenceFactor scnConstraint =
  sendMessage scnConstraint influenceFactorSelector

-- | influenceFactor
--
-- Specifies the inflence factor of the receiver. Defaults to 1. Animatable
--
-- ObjC selector: @- setInfluenceFactor:@
setInfluenceFactor :: IsSCNConstraint scnConstraint => scnConstraint -> CDouble -> IO ()
setInfluenceFactor scnConstraint value =
  sendMessage scnConstraint setInfluenceFactorSelector value

-- | incremental
--
-- Specifies whether or not the contraint should applies incrementally and have it's effect being cumulated over the rendered frames. Defaults to YES starting macOS 10.13, iOS 11, tvOS 11 and watchOS 4. Defaults to NO in previous versions.
--
-- ObjC selector: @- incremental@
incremental :: IsSCNConstraint scnConstraint => scnConstraint -> IO Bool
incremental scnConstraint =
  sendMessage scnConstraint incrementalSelector

-- | incremental
--
-- Specifies whether or not the contraint should applies incrementally and have it's effect being cumulated over the rendered frames. Defaults to YES starting macOS 10.13, iOS 11, tvOS 11 and watchOS 4. Defaults to NO in previous versions.
--
-- ObjC selector: @- setIncremental:@
setIncremental :: IsSCNConstraint scnConstraint => scnConstraint -> Bool -> IO ()
setIncremental scnConstraint value =
  sendMessage scnConstraint setIncrementalSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @influenceFactor@
influenceFactorSelector :: Selector '[] CDouble
influenceFactorSelector = mkSelector "influenceFactor"

-- | @Selector@ for @setInfluenceFactor:@
setInfluenceFactorSelector :: Selector '[CDouble] ()
setInfluenceFactorSelector = mkSelector "setInfluenceFactor:"

-- | @Selector@ for @incremental@
incrementalSelector :: Selector '[] Bool
incrementalSelector = mkSelector "incremental"

-- | @Selector@ for @setIncremental:@
setIncrementalSelector :: Selector '[Bool] ()
setIncrementalSelector = mkSelector "setIncremental:"

