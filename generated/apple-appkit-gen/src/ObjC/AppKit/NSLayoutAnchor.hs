{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSLayoutAnchor@.
module ObjC.AppKit.NSLayoutAnchor
  ( NSLayoutAnchor
  , IsNSLayoutAnchor(..)
  , constraintEqualToAnchor
  , constraintGreaterThanOrEqualToAnchor
  , constraintLessThanOrEqualToAnchor
  , constraintEqualToAnchor_constant
  , constraintGreaterThanOrEqualToAnchor_constant
  , constraintLessThanOrEqualToAnchor_constant
  , name
  , item
  , hasAmbiguousLayout
  , constraintsAffectingLayout
  , constraintEqualToAnchorSelector
  , constraintEqualToAnchor_constantSelector
  , constraintGreaterThanOrEqualToAnchorSelector
  , constraintGreaterThanOrEqualToAnchor_constantSelector
  , constraintLessThanOrEqualToAnchorSelector
  , constraintLessThanOrEqualToAnchor_constantSelector
  , constraintsAffectingLayoutSelector
  , hasAmbiguousLayoutSelector
  , itemSelector
  , nameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- constraintEqualToAnchor:@
constraintEqualToAnchor :: (IsNSLayoutAnchor nsLayoutAnchor, IsNSLayoutAnchor anchor) => nsLayoutAnchor -> anchor -> IO (Id NSLayoutConstraint)
constraintEqualToAnchor nsLayoutAnchor anchor =
  sendMessage nsLayoutAnchor constraintEqualToAnchorSelector (toNSLayoutAnchor anchor)

-- | @- constraintGreaterThanOrEqualToAnchor:@
constraintGreaterThanOrEqualToAnchor :: (IsNSLayoutAnchor nsLayoutAnchor, IsNSLayoutAnchor anchor) => nsLayoutAnchor -> anchor -> IO (Id NSLayoutConstraint)
constraintGreaterThanOrEqualToAnchor nsLayoutAnchor anchor =
  sendMessage nsLayoutAnchor constraintGreaterThanOrEqualToAnchorSelector (toNSLayoutAnchor anchor)

-- | @- constraintLessThanOrEqualToAnchor:@
constraintLessThanOrEqualToAnchor :: (IsNSLayoutAnchor nsLayoutAnchor, IsNSLayoutAnchor anchor) => nsLayoutAnchor -> anchor -> IO (Id NSLayoutConstraint)
constraintLessThanOrEqualToAnchor nsLayoutAnchor anchor =
  sendMessage nsLayoutAnchor constraintLessThanOrEqualToAnchorSelector (toNSLayoutAnchor anchor)

-- | @- constraintEqualToAnchor:constant:@
constraintEqualToAnchor_constant :: (IsNSLayoutAnchor nsLayoutAnchor, IsNSLayoutAnchor anchor) => nsLayoutAnchor -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintEqualToAnchor_constant nsLayoutAnchor anchor c =
  sendMessage nsLayoutAnchor constraintEqualToAnchor_constantSelector (toNSLayoutAnchor anchor) c

-- | @- constraintGreaterThanOrEqualToAnchor:constant:@
constraintGreaterThanOrEqualToAnchor_constant :: (IsNSLayoutAnchor nsLayoutAnchor, IsNSLayoutAnchor anchor) => nsLayoutAnchor -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintGreaterThanOrEqualToAnchor_constant nsLayoutAnchor anchor c =
  sendMessage nsLayoutAnchor constraintGreaterThanOrEqualToAnchor_constantSelector (toNSLayoutAnchor anchor) c

-- | @- constraintLessThanOrEqualToAnchor:constant:@
constraintLessThanOrEqualToAnchor_constant :: (IsNSLayoutAnchor nsLayoutAnchor, IsNSLayoutAnchor anchor) => nsLayoutAnchor -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintLessThanOrEqualToAnchor_constant nsLayoutAnchor anchor c =
  sendMessage nsLayoutAnchor constraintLessThanOrEqualToAnchor_constantSelector (toNSLayoutAnchor anchor) c

-- | @- name@
name :: IsNSLayoutAnchor nsLayoutAnchor => nsLayoutAnchor -> IO (Id NSString)
name nsLayoutAnchor =
  sendMessage nsLayoutAnchor nameSelector

-- | @- item@
item :: IsNSLayoutAnchor nsLayoutAnchor => nsLayoutAnchor -> IO RawId
item nsLayoutAnchor =
  sendMessage nsLayoutAnchor itemSelector

-- | @- hasAmbiguousLayout@
hasAmbiguousLayout :: IsNSLayoutAnchor nsLayoutAnchor => nsLayoutAnchor -> IO Bool
hasAmbiguousLayout nsLayoutAnchor =
  sendMessage nsLayoutAnchor hasAmbiguousLayoutSelector

-- | @- constraintsAffectingLayout@
constraintsAffectingLayout :: IsNSLayoutAnchor nsLayoutAnchor => nsLayoutAnchor -> IO (Id NSArray)
constraintsAffectingLayout nsLayoutAnchor =
  sendMessage nsLayoutAnchor constraintsAffectingLayoutSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @constraintEqualToAnchor:@
constraintEqualToAnchorSelector :: Selector '[Id NSLayoutAnchor] (Id NSLayoutConstraint)
constraintEqualToAnchorSelector = mkSelector "constraintEqualToAnchor:"

-- | @Selector@ for @constraintGreaterThanOrEqualToAnchor:@
constraintGreaterThanOrEqualToAnchorSelector :: Selector '[Id NSLayoutAnchor] (Id NSLayoutConstraint)
constraintGreaterThanOrEqualToAnchorSelector = mkSelector "constraintGreaterThanOrEqualToAnchor:"

-- | @Selector@ for @constraintLessThanOrEqualToAnchor:@
constraintLessThanOrEqualToAnchorSelector :: Selector '[Id NSLayoutAnchor] (Id NSLayoutConstraint)
constraintLessThanOrEqualToAnchorSelector = mkSelector "constraintLessThanOrEqualToAnchor:"

-- | @Selector@ for @constraintEqualToAnchor:constant:@
constraintEqualToAnchor_constantSelector :: Selector '[Id NSLayoutAnchor, CDouble] (Id NSLayoutConstraint)
constraintEqualToAnchor_constantSelector = mkSelector "constraintEqualToAnchor:constant:"

-- | @Selector@ for @constraintGreaterThanOrEqualToAnchor:constant:@
constraintGreaterThanOrEqualToAnchor_constantSelector :: Selector '[Id NSLayoutAnchor, CDouble] (Id NSLayoutConstraint)
constraintGreaterThanOrEqualToAnchor_constantSelector = mkSelector "constraintGreaterThanOrEqualToAnchor:constant:"

-- | @Selector@ for @constraintLessThanOrEqualToAnchor:constant:@
constraintLessThanOrEqualToAnchor_constantSelector :: Selector '[Id NSLayoutAnchor, CDouble] (Id NSLayoutConstraint)
constraintLessThanOrEqualToAnchor_constantSelector = mkSelector "constraintLessThanOrEqualToAnchor:constant:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @item@
itemSelector :: Selector '[] RawId
itemSelector = mkSelector "item"

-- | @Selector@ for @hasAmbiguousLayout@
hasAmbiguousLayoutSelector :: Selector '[] Bool
hasAmbiguousLayoutSelector = mkSelector "hasAmbiguousLayout"

-- | @Selector@ for @constraintsAffectingLayout@
constraintsAffectingLayoutSelector :: Selector '[] (Id NSArray)
constraintsAffectingLayoutSelector = mkSelector "constraintsAffectingLayout"

