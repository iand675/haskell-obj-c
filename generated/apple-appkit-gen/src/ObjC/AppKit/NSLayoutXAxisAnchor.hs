{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSLayoutXAxisAnchor@.
module ObjC.AppKit.NSLayoutXAxisAnchor
  ( NSLayoutXAxisAnchor
  , IsNSLayoutXAxisAnchor(..)
  , anchorWithOffsetToAnchor
  , constraintEqualToSystemSpacingAfterAnchor_multiplier
  , constraintGreaterThanOrEqualToSystemSpacingAfterAnchor_multiplier
  , constraintLessThanOrEqualToSystemSpacingAfterAnchor_multiplier
  , anchorWithOffsetToAnchorSelector
  , constraintEqualToSystemSpacingAfterAnchor_multiplierSelector
  , constraintGreaterThanOrEqualToSystemSpacingAfterAnchor_multiplierSelector
  , constraintLessThanOrEqualToSystemSpacingAfterAnchor_multiplierSelector


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
anchorWithOffsetToAnchor :: (IsNSLayoutXAxisAnchor nsLayoutXAxisAnchor, IsNSLayoutXAxisAnchor otherAnchor) => nsLayoutXAxisAnchor -> otherAnchor -> IO (Id NSLayoutDimension)
anchorWithOffsetToAnchor nsLayoutXAxisAnchor otherAnchor =
  sendMessage nsLayoutXAxisAnchor anchorWithOffsetToAnchorSelector (toNSLayoutXAxisAnchor otherAnchor)

-- | @- constraintEqualToSystemSpacingAfterAnchor:multiplier:@
constraintEqualToSystemSpacingAfterAnchor_multiplier :: (IsNSLayoutXAxisAnchor nsLayoutXAxisAnchor, IsNSLayoutXAxisAnchor anchor) => nsLayoutXAxisAnchor -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintEqualToSystemSpacingAfterAnchor_multiplier nsLayoutXAxisAnchor anchor multiplier =
  sendMessage nsLayoutXAxisAnchor constraintEqualToSystemSpacingAfterAnchor_multiplierSelector (toNSLayoutXAxisAnchor anchor) multiplier

-- | @- constraintGreaterThanOrEqualToSystemSpacingAfterAnchor:multiplier:@
constraintGreaterThanOrEqualToSystemSpacingAfterAnchor_multiplier :: (IsNSLayoutXAxisAnchor nsLayoutXAxisAnchor, IsNSLayoutXAxisAnchor anchor) => nsLayoutXAxisAnchor -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintGreaterThanOrEqualToSystemSpacingAfterAnchor_multiplier nsLayoutXAxisAnchor anchor multiplier =
  sendMessage nsLayoutXAxisAnchor constraintGreaterThanOrEqualToSystemSpacingAfterAnchor_multiplierSelector (toNSLayoutXAxisAnchor anchor) multiplier

-- | @- constraintLessThanOrEqualToSystemSpacingAfterAnchor:multiplier:@
constraintLessThanOrEqualToSystemSpacingAfterAnchor_multiplier :: (IsNSLayoutXAxisAnchor nsLayoutXAxisAnchor, IsNSLayoutXAxisAnchor anchor) => nsLayoutXAxisAnchor -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintLessThanOrEqualToSystemSpacingAfterAnchor_multiplier nsLayoutXAxisAnchor anchor multiplier =
  sendMessage nsLayoutXAxisAnchor constraintLessThanOrEqualToSystemSpacingAfterAnchor_multiplierSelector (toNSLayoutXAxisAnchor anchor) multiplier

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @anchorWithOffsetToAnchor:@
anchorWithOffsetToAnchorSelector :: Selector '[Id NSLayoutXAxisAnchor] (Id NSLayoutDimension)
anchorWithOffsetToAnchorSelector = mkSelector "anchorWithOffsetToAnchor:"

-- | @Selector@ for @constraintEqualToSystemSpacingAfterAnchor:multiplier:@
constraintEqualToSystemSpacingAfterAnchor_multiplierSelector :: Selector '[Id NSLayoutXAxisAnchor, CDouble] (Id NSLayoutConstraint)
constraintEqualToSystemSpacingAfterAnchor_multiplierSelector = mkSelector "constraintEqualToSystemSpacingAfterAnchor:multiplier:"

-- | @Selector@ for @constraintGreaterThanOrEqualToSystemSpacingAfterAnchor:multiplier:@
constraintGreaterThanOrEqualToSystemSpacingAfterAnchor_multiplierSelector :: Selector '[Id NSLayoutXAxisAnchor, CDouble] (Id NSLayoutConstraint)
constraintGreaterThanOrEqualToSystemSpacingAfterAnchor_multiplierSelector = mkSelector "constraintGreaterThanOrEqualToSystemSpacingAfterAnchor:multiplier:"

-- | @Selector@ for @constraintLessThanOrEqualToSystemSpacingAfterAnchor:multiplier:@
constraintLessThanOrEqualToSystemSpacingAfterAnchor_multiplierSelector :: Selector '[Id NSLayoutXAxisAnchor, CDouble] (Id NSLayoutConstraint)
constraintLessThanOrEqualToSystemSpacingAfterAnchor_multiplierSelector = mkSelector "constraintLessThanOrEqualToSystemSpacingAfterAnchor:multiplier:"

