{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSLayoutYAxisAnchor@.
module ObjC.AppKit.NSLayoutYAxisAnchor
  ( NSLayoutYAxisAnchor
  , IsNSLayoutYAxisAnchor(..)
  , anchorWithOffsetToAnchor
  , constraintEqualToSystemSpacingBelowAnchor_multiplier
  , constraintGreaterThanOrEqualToSystemSpacingBelowAnchor_multiplier
  , constraintLessThanOrEqualToSystemSpacingBelowAnchor_multiplier
  , anchorWithOffsetToAnchorSelector
  , constraintEqualToSystemSpacingBelowAnchor_multiplierSelector
  , constraintGreaterThanOrEqualToSystemSpacingBelowAnchor_multiplierSelector
  , constraintLessThanOrEqualToSystemSpacingBelowAnchor_multiplierSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- anchorWithOffsetToAnchor:@
anchorWithOffsetToAnchor :: (IsNSLayoutYAxisAnchor nsLayoutYAxisAnchor, IsNSLayoutYAxisAnchor otherAnchor) => nsLayoutYAxisAnchor -> otherAnchor -> IO (Id NSLayoutDimension)
anchorWithOffsetToAnchor nsLayoutYAxisAnchor otherAnchor =
  sendMessage nsLayoutYAxisAnchor anchorWithOffsetToAnchorSelector (toNSLayoutYAxisAnchor otherAnchor)

-- | @- constraintEqualToSystemSpacingBelowAnchor:multiplier:@
constraintEqualToSystemSpacingBelowAnchor_multiplier :: (IsNSLayoutYAxisAnchor nsLayoutYAxisAnchor, IsNSLayoutYAxisAnchor anchor) => nsLayoutYAxisAnchor -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintEqualToSystemSpacingBelowAnchor_multiplier nsLayoutYAxisAnchor anchor multiplier =
  sendMessage nsLayoutYAxisAnchor constraintEqualToSystemSpacingBelowAnchor_multiplierSelector (toNSLayoutYAxisAnchor anchor) multiplier

-- | @- constraintGreaterThanOrEqualToSystemSpacingBelowAnchor:multiplier:@
constraintGreaterThanOrEqualToSystemSpacingBelowAnchor_multiplier :: (IsNSLayoutYAxisAnchor nsLayoutYAxisAnchor, IsNSLayoutYAxisAnchor anchor) => nsLayoutYAxisAnchor -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintGreaterThanOrEqualToSystemSpacingBelowAnchor_multiplier nsLayoutYAxisAnchor anchor multiplier =
  sendMessage nsLayoutYAxisAnchor constraintGreaterThanOrEqualToSystemSpacingBelowAnchor_multiplierSelector (toNSLayoutYAxisAnchor anchor) multiplier

-- | @- constraintLessThanOrEqualToSystemSpacingBelowAnchor:multiplier:@
constraintLessThanOrEqualToSystemSpacingBelowAnchor_multiplier :: (IsNSLayoutYAxisAnchor nsLayoutYAxisAnchor, IsNSLayoutYAxisAnchor anchor) => nsLayoutYAxisAnchor -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintLessThanOrEqualToSystemSpacingBelowAnchor_multiplier nsLayoutYAxisAnchor anchor multiplier =
  sendMessage nsLayoutYAxisAnchor constraintLessThanOrEqualToSystemSpacingBelowAnchor_multiplierSelector (toNSLayoutYAxisAnchor anchor) multiplier

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @anchorWithOffsetToAnchor:@
anchorWithOffsetToAnchorSelector :: Selector '[Id NSLayoutYAxisAnchor] (Id NSLayoutDimension)
anchorWithOffsetToAnchorSelector = mkSelector "anchorWithOffsetToAnchor:"

-- | @Selector@ for @constraintEqualToSystemSpacingBelowAnchor:multiplier:@
constraintEqualToSystemSpacingBelowAnchor_multiplierSelector :: Selector '[Id NSLayoutYAxisAnchor, CDouble] (Id NSLayoutConstraint)
constraintEqualToSystemSpacingBelowAnchor_multiplierSelector = mkSelector "constraintEqualToSystemSpacingBelowAnchor:multiplier:"

-- | @Selector@ for @constraintGreaterThanOrEqualToSystemSpacingBelowAnchor:multiplier:@
constraintGreaterThanOrEqualToSystemSpacingBelowAnchor_multiplierSelector :: Selector '[Id NSLayoutYAxisAnchor, CDouble] (Id NSLayoutConstraint)
constraintGreaterThanOrEqualToSystemSpacingBelowAnchor_multiplierSelector = mkSelector "constraintGreaterThanOrEqualToSystemSpacingBelowAnchor:multiplier:"

-- | @Selector@ for @constraintLessThanOrEqualToSystemSpacingBelowAnchor:multiplier:@
constraintLessThanOrEqualToSystemSpacingBelowAnchor_multiplierSelector :: Selector '[Id NSLayoutYAxisAnchor, CDouble] (Id NSLayoutConstraint)
constraintLessThanOrEqualToSystemSpacingBelowAnchor_multiplierSelector = mkSelector "constraintLessThanOrEqualToSystemSpacingBelowAnchor:multiplier:"

