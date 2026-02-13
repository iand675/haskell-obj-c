{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSLayoutDimension@.
module ObjC.AppKit.NSLayoutDimension
  ( NSLayoutDimension
  , IsNSLayoutDimension(..)
  , constraintEqualToConstant
  , constraintGreaterThanOrEqualToConstant
  , constraintLessThanOrEqualToConstant
  , constraintEqualToAnchor_multiplier
  , constraintGreaterThanOrEqualToAnchor_multiplier
  , constraintLessThanOrEqualToAnchor_multiplier
  , constraintEqualToAnchor_multiplier_constant
  , constraintGreaterThanOrEqualToAnchor_multiplier_constant
  , constraintLessThanOrEqualToAnchor_multiplier_constant
  , constraintEqualToAnchor_multiplierSelector
  , constraintEqualToAnchor_multiplier_constantSelector
  , constraintEqualToConstantSelector
  , constraintGreaterThanOrEqualToAnchor_multiplierSelector
  , constraintGreaterThanOrEqualToAnchor_multiplier_constantSelector
  , constraintGreaterThanOrEqualToConstantSelector
  , constraintLessThanOrEqualToAnchor_multiplierSelector
  , constraintLessThanOrEqualToAnchor_multiplier_constantSelector
  , constraintLessThanOrEqualToConstantSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- constraintEqualToConstant:@
constraintEqualToConstant :: IsNSLayoutDimension nsLayoutDimension => nsLayoutDimension -> CDouble -> IO (Id NSLayoutConstraint)
constraintEqualToConstant nsLayoutDimension c =
  sendMessage nsLayoutDimension constraintEqualToConstantSelector c

-- | @- constraintGreaterThanOrEqualToConstant:@
constraintGreaterThanOrEqualToConstant :: IsNSLayoutDimension nsLayoutDimension => nsLayoutDimension -> CDouble -> IO (Id NSLayoutConstraint)
constraintGreaterThanOrEqualToConstant nsLayoutDimension c =
  sendMessage nsLayoutDimension constraintGreaterThanOrEqualToConstantSelector c

-- | @- constraintLessThanOrEqualToConstant:@
constraintLessThanOrEqualToConstant :: IsNSLayoutDimension nsLayoutDimension => nsLayoutDimension -> CDouble -> IO (Id NSLayoutConstraint)
constraintLessThanOrEqualToConstant nsLayoutDimension c =
  sendMessage nsLayoutDimension constraintLessThanOrEqualToConstantSelector c

-- | @- constraintEqualToAnchor:multiplier:@
constraintEqualToAnchor_multiplier :: (IsNSLayoutDimension nsLayoutDimension, IsNSLayoutDimension anchor) => nsLayoutDimension -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintEqualToAnchor_multiplier nsLayoutDimension anchor m =
  sendMessage nsLayoutDimension constraintEqualToAnchor_multiplierSelector (toNSLayoutDimension anchor) m

-- | @- constraintGreaterThanOrEqualToAnchor:multiplier:@
constraintGreaterThanOrEqualToAnchor_multiplier :: (IsNSLayoutDimension nsLayoutDimension, IsNSLayoutDimension anchor) => nsLayoutDimension -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintGreaterThanOrEqualToAnchor_multiplier nsLayoutDimension anchor m =
  sendMessage nsLayoutDimension constraintGreaterThanOrEqualToAnchor_multiplierSelector (toNSLayoutDimension anchor) m

-- | @- constraintLessThanOrEqualToAnchor:multiplier:@
constraintLessThanOrEqualToAnchor_multiplier :: (IsNSLayoutDimension nsLayoutDimension, IsNSLayoutDimension anchor) => nsLayoutDimension -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintLessThanOrEqualToAnchor_multiplier nsLayoutDimension anchor m =
  sendMessage nsLayoutDimension constraintLessThanOrEqualToAnchor_multiplierSelector (toNSLayoutDimension anchor) m

-- | @- constraintEqualToAnchor:multiplier:constant:@
constraintEqualToAnchor_multiplier_constant :: (IsNSLayoutDimension nsLayoutDimension, IsNSLayoutDimension anchor) => nsLayoutDimension -> anchor -> CDouble -> CDouble -> IO (Id NSLayoutConstraint)
constraintEqualToAnchor_multiplier_constant nsLayoutDimension anchor m c =
  sendMessage nsLayoutDimension constraintEqualToAnchor_multiplier_constantSelector (toNSLayoutDimension anchor) m c

-- | @- constraintGreaterThanOrEqualToAnchor:multiplier:constant:@
constraintGreaterThanOrEqualToAnchor_multiplier_constant :: (IsNSLayoutDimension nsLayoutDimension, IsNSLayoutDimension anchor) => nsLayoutDimension -> anchor -> CDouble -> CDouble -> IO (Id NSLayoutConstraint)
constraintGreaterThanOrEqualToAnchor_multiplier_constant nsLayoutDimension anchor m c =
  sendMessage nsLayoutDimension constraintGreaterThanOrEqualToAnchor_multiplier_constantSelector (toNSLayoutDimension anchor) m c

-- | @- constraintLessThanOrEqualToAnchor:multiplier:constant:@
constraintLessThanOrEqualToAnchor_multiplier_constant :: (IsNSLayoutDimension nsLayoutDimension, IsNSLayoutDimension anchor) => nsLayoutDimension -> anchor -> CDouble -> CDouble -> IO (Id NSLayoutConstraint)
constraintLessThanOrEqualToAnchor_multiplier_constant nsLayoutDimension anchor m c =
  sendMessage nsLayoutDimension constraintLessThanOrEqualToAnchor_multiplier_constantSelector (toNSLayoutDimension anchor) m c

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @constraintEqualToConstant:@
constraintEqualToConstantSelector :: Selector '[CDouble] (Id NSLayoutConstraint)
constraintEqualToConstantSelector = mkSelector "constraintEqualToConstant:"

-- | @Selector@ for @constraintGreaterThanOrEqualToConstant:@
constraintGreaterThanOrEqualToConstantSelector :: Selector '[CDouble] (Id NSLayoutConstraint)
constraintGreaterThanOrEqualToConstantSelector = mkSelector "constraintGreaterThanOrEqualToConstant:"

-- | @Selector@ for @constraintLessThanOrEqualToConstant:@
constraintLessThanOrEqualToConstantSelector :: Selector '[CDouble] (Id NSLayoutConstraint)
constraintLessThanOrEqualToConstantSelector = mkSelector "constraintLessThanOrEqualToConstant:"

-- | @Selector@ for @constraintEqualToAnchor:multiplier:@
constraintEqualToAnchor_multiplierSelector :: Selector '[Id NSLayoutDimension, CDouble] (Id NSLayoutConstraint)
constraintEqualToAnchor_multiplierSelector = mkSelector "constraintEqualToAnchor:multiplier:"

-- | @Selector@ for @constraintGreaterThanOrEqualToAnchor:multiplier:@
constraintGreaterThanOrEqualToAnchor_multiplierSelector :: Selector '[Id NSLayoutDimension, CDouble] (Id NSLayoutConstraint)
constraintGreaterThanOrEqualToAnchor_multiplierSelector = mkSelector "constraintGreaterThanOrEqualToAnchor:multiplier:"

-- | @Selector@ for @constraintLessThanOrEqualToAnchor:multiplier:@
constraintLessThanOrEqualToAnchor_multiplierSelector :: Selector '[Id NSLayoutDimension, CDouble] (Id NSLayoutConstraint)
constraintLessThanOrEqualToAnchor_multiplierSelector = mkSelector "constraintLessThanOrEqualToAnchor:multiplier:"

-- | @Selector@ for @constraintEqualToAnchor:multiplier:constant:@
constraintEqualToAnchor_multiplier_constantSelector :: Selector '[Id NSLayoutDimension, CDouble, CDouble] (Id NSLayoutConstraint)
constraintEqualToAnchor_multiplier_constantSelector = mkSelector "constraintEqualToAnchor:multiplier:constant:"

-- | @Selector@ for @constraintGreaterThanOrEqualToAnchor:multiplier:constant:@
constraintGreaterThanOrEqualToAnchor_multiplier_constantSelector :: Selector '[Id NSLayoutDimension, CDouble, CDouble] (Id NSLayoutConstraint)
constraintGreaterThanOrEqualToAnchor_multiplier_constantSelector = mkSelector "constraintGreaterThanOrEqualToAnchor:multiplier:constant:"

-- | @Selector@ for @constraintLessThanOrEqualToAnchor:multiplier:constant:@
constraintLessThanOrEqualToAnchor_multiplier_constantSelector :: Selector '[Id NSLayoutDimension, CDouble, CDouble] (Id NSLayoutConstraint)
constraintLessThanOrEqualToAnchor_multiplier_constantSelector = mkSelector "constraintLessThanOrEqualToAnchor:multiplier:constant:"

