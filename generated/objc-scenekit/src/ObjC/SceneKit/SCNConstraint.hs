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
  , setEnabledSelector
  , influenceFactorSelector
  , setInfluenceFactorSelector
  , incrementalSelector
  , setIncrementalSelector


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
import ObjC.Foundation.Internal.Classes

-- | enable
--
-- Determines whether the constraint is enabled or not. Defaults to YES.
--
-- ObjC selector: @- enabled@
enabled :: IsSCNConstraint scnConstraint => scnConstraint -> IO Bool
enabled scnConstraint  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnConstraint (mkSelector "enabled") retCULong []

-- | enable
--
-- Determines whether the constraint is enabled or not. Defaults to YES.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsSCNConstraint scnConstraint => scnConstraint -> Bool -> IO ()
setEnabled scnConstraint  value =
  sendMsg scnConstraint (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | influenceFactor
--
-- Specifies the inflence factor of the receiver. Defaults to 1. Animatable
--
-- ObjC selector: @- influenceFactor@
influenceFactor :: IsSCNConstraint scnConstraint => scnConstraint -> IO CDouble
influenceFactor scnConstraint  =
  sendMsg scnConstraint (mkSelector "influenceFactor") retCDouble []

-- | influenceFactor
--
-- Specifies the inflence factor of the receiver. Defaults to 1. Animatable
--
-- ObjC selector: @- setInfluenceFactor:@
setInfluenceFactor :: IsSCNConstraint scnConstraint => scnConstraint -> CDouble -> IO ()
setInfluenceFactor scnConstraint  value =
  sendMsg scnConstraint (mkSelector "setInfluenceFactor:") retVoid [argCDouble (fromIntegral value)]

-- | incremental
--
-- Specifies whether or not the contraint should applies incrementally and have it's effect being cumulated over the rendered frames. Defaults to YES starting macOS 10.13, iOS 11, tvOS 11 and watchOS 4. Defaults to NO in previous versions.
--
-- ObjC selector: @- incremental@
incremental :: IsSCNConstraint scnConstraint => scnConstraint -> IO Bool
incremental scnConstraint  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg scnConstraint (mkSelector "incremental") retCULong []

-- | incremental
--
-- Specifies whether or not the contraint should applies incrementally and have it's effect being cumulated over the rendered frames. Defaults to YES starting macOS 10.13, iOS 11, tvOS 11 and watchOS 4. Defaults to NO in previous versions.
--
-- ObjC selector: @- setIncremental:@
setIncremental :: IsSCNConstraint scnConstraint => scnConstraint -> Bool -> IO ()
setIncremental scnConstraint  value =
  sendMsg scnConstraint (mkSelector "setIncremental:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @influenceFactor@
influenceFactorSelector :: Selector
influenceFactorSelector = mkSelector "influenceFactor"

-- | @Selector@ for @setInfluenceFactor:@
setInfluenceFactorSelector :: Selector
setInfluenceFactorSelector = mkSelector "setInfluenceFactor:"

-- | @Selector@ for @incremental@
incrementalSelector :: Selector
incrementalSelector = mkSelector "incremental"

-- | @Selector@ for @setIncremental:@
setIncrementalSelector :: Selector
setIncrementalSelector = mkSelector "setIncremental:"

