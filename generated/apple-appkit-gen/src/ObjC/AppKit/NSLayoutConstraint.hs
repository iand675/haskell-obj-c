{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSLayoutConstraint@.
module ObjC.AppKit.NSLayoutConstraint
  ( NSLayoutConstraint
  , IsNSLayoutConstraint(..)
  , constraintsWithVisualFormat_options_metrics_views
  , constraintWithItem_attribute_relatedBy_toItem_attribute_multiplier_constant
  , activateConstraints
  , deactivateConstraints
  , priority
  , setPriority
  , shouldBeArchived
  , setShouldBeArchived
  , firstItem
  , secondItem
  , firstAttribute
  , secondAttribute
  , firstAnchor
  , secondAnchor
  , relation
  , multiplier
  , constant
  , setConstant
  , active
  , setActive
  , identifier
  , setIdentifier
  , activateConstraintsSelector
  , activeSelector
  , constantSelector
  , constraintWithItem_attribute_relatedBy_toItem_attribute_multiplier_constantSelector
  , constraintsWithVisualFormat_options_metrics_viewsSelector
  , deactivateConstraintsSelector
  , firstAnchorSelector
  , firstAttributeSelector
  , firstItemSelector
  , identifierSelector
  , multiplierSelector
  , prioritySelector
  , relationSelector
  , secondAnchorSelector
  , secondAttributeSelector
  , secondItemSelector
  , setActiveSelector
  , setConstantSelector
  , setIdentifierSelector
  , setPrioritySelector
  , setShouldBeArchivedSelector
  , shouldBeArchivedSelector

  -- * Enum types
  , NSLayoutAttribute(NSLayoutAttribute)
  , pattern NSLayoutAttributeLeft
  , pattern NSLayoutAttributeRight
  , pattern NSLayoutAttributeTop
  , pattern NSLayoutAttributeBottom
  , pattern NSLayoutAttributeLeading
  , pattern NSLayoutAttributeTrailing
  , pattern NSLayoutAttributeWidth
  , pattern NSLayoutAttributeHeight
  , pattern NSLayoutAttributeCenterX
  , pattern NSLayoutAttributeCenterY
  , pattern NSLayoutAttributeLastBaseline
  , pattern NSLayoutAttributeBaseline
  , pattern NSLayoutAttributeFirstBaseline
  , pattern NSLayoutAttributeNotAnAttribute
  , NSLayoutFormatOptions(NSLayoutFormatOptions)
  , pattern NSLayoutFormatAlignAllLeft
  , pattern NSLayoutFormatAlignAllRight
  , pattern NSLayoutFormatAlignAllTop
  , pattern NSLayoutFormatAlignAllBottom
  , pattern NSLayoutFormatAlignAllLeading
  , pattern NSLayoutFormatAlignAllTrailing
  , pattern NSLayoutFormatAlignAllCenterX
  , pattern NSLayoutFormatAlignAllCenterY
  , pattern NSLayoutFormatAlignAllLastBaseline
  , pattern NSLayoutFormatAlignAllFirstBaseline
  , pattern NSLayoutFormatAlignAllBaseline
  , pattern NSLayoutFormatAlignmentMask
  , pattern NSLayoutFormatDirectionLeadingToTrailing
  , pattern NSLayoutFormatDirectionLeftToRight
  , pattern NSLayoutFormatDirectionRightToLeft
  , pattern NSLayoutFormatDirectionMask
  , NSLayoutRelation(NSLayoutRelation)
  , pattern NSLayoutRelationLessThanOrEqual
  , pattern NSLayoutRelationEqual
  , pattern NSLayoutRelationGreaterThanOrEqual

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ constraintsWithVisualFormat:options:metrics:views:@
constraintsWithVisualFormat_options_metrics_views :: (IsNSString format, IsNSDictionary metrics, IsNSDictionary views) => format -> NSLayoutFormatOptions -> metrics -> views -> IO (Id NSArray)
constraintsWithVisualFormat_options_metrics_views format opts metrics views =
  do
    cls' <- getRequiredClass "NSLayoutConstraint"
    sendClassMessage cls' constraintsWithVisualFormat_options_metrics_viewsSelector (toNSString format) opts (toNSDictionary metrics) (toNSDictionary views)

-- | @+ constraintWithItem:attribute:relatedBy:toItem:attribute:multiplier:constant:@
constraintWithItem_attribute_relatedBy_toItem_attribute_multiplier_constant :: RawId -> NSLayoutAttribute -> NSLayoutRelation -> RawId -> NSLayoutAttribute -> CDouble -> CDouble -> IO (Id NSLayoutConstraint)
constraintWithItem_attribute_relatedBy_toItem_attribute_multiplier_constant view1 attr1 relation view2 attr2 multiplier c =
  do
    cls' <- getRequiredClass "NSLayoutConstraint"
    sendClassMessage cls' constraintWithItem_attribute_relatedBy_toItem_attribute_multiplier_constantSelector view1 attr1 relation view2 attr2 multiplier c

-- | @+ activateConstraints:@
activateConstraints :: IsNSArray constraints => constraints -> IO ()
activateConstraints constraints =
  do
    cls' <- getRequiredClass "NSLayoutConstraint"
    sendClassMessage cls' activateConstraintsSelector (toNSArray constraints)

-- | @+ deactivateConstraints:@
deactivateConstraints :: IsNSArray constraints => constraints -> IO ()
deactivateConstraints constraints =
  do
    cls' <- getRequiredClass "NSLayoutConstraint"
    sendClassMessage cls' deactivateConstraintsSelector (toNSArray constraints)

-- | @- priority@
priority :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO CFloat
priority nsLayoutConstraint =
  sendMessage nsLayoutConstraint prioritySelector

-- | @- setPriority:@
setPriority :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> CFloat -> IO ()
setPriority nsLayoutConstraint value =
  sendMessage nsLayoutConstraint setPrioritySelector value

-- | @- shouldBeArchived@
shouldBeArchived :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO Bool
shouldBeArchived nsLayoutConstraint =
  sendMessage nsLayoutConstraint shouldBeArchivedSelector

-- | @- setShouldBeArchived:@
setShouldBeArchived :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> Bool -> IO ()
setShouldBeArchived nsLayoutConstraint value =
  sendMessage nsLayoutConstraint setShouldBeArchivedSelector value

-- | @- firstItem@
firstItem :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO RawId
firstItem nsLayoutConstraint =
  sendMessage nsLayoutConstraint firstItemSelector

-- | @- secondItem@
secondItem :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO RawId
secondItem nsLayoutConstraint =
  sendMessage nsLayoutConstraint secondItemSelector

-- | @- firstAttribute@
firstAttribute :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO NSLayoutAttribute
firstAttribute nsLayoutConstraint =
  sendMessage nsLayoutConstraint firstAttributeSelector

-- | @- secondAttribute@
secondAttribute :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO NSLayoutAttribute
secondAttribute nsLayoutConstraint =
  sendMessage nsLayoutConstraint secondAttributeSelector

-- | @- firstAnchor@
firstAnchor :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO (Id NSLayoutAnchor)
firstAnchor nsLayoutConstraint =
  sendMessage nsLayoutConstraint firstAnchorSelector

-- | @- secondAnchor@
secondAnchor :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO (Id NSLayoutAnchor)
secondAnchor nsLayoutConstraint =
  sendMessage nsLayoutConstraint secondAnchorSelector

-- | @- relation@
relation :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO NSLayoutRelation
relation nsLayoutConstraint =
  sendMessage nsLayoutConstraint relationSelector

-- | @- multiplier@
multiplier :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO CDouble
multiplier nsLayoutConstraint =
  sendMessage nsLayoutConstraint multiplierSelector

-- | @- constant@
constant :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO CDouble
constant nsLayoutConstraint =
  sendMessage nsLayoutConstraint constantSelector

-- | @- setConstant:@
setConstant :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> CDouble -> IO ()
setConstant nsLayoutConstraint value =
  sendMessage nsLayoutConstraint setConstantSelector value

-- | @- active@
active :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO Bool
active nsLayoutConstraint =
  sendMessage nsLayoutConstraint activeSelector

-- | @- setActive:@
setActive :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> Bool -> IO ()
setActive nsLayoutConstraint value =
  sendMessage nsLayoutConstraint setActiveSelector value

-- | @- identifier@
identifier :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO (Id NSString)
identifier nsLayoutConstraint =
  sendMessage nsLayoutConstraint identifierSelector

-- | @- setIdentifier:@
setIdentifier :: (IsNSLayoutConstraint nsLayoutConstraint, IsNSString value) => nsLayoutConstraint -> value -> IO ()
setIdentifier nsLayoutConstraint value =
  sendMessage nsLayoutConstraint setIdentifierSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @constraintsWithVisualFormat:options:metrics:views:@
constraintsWithVisualFormat_options_metrics_viewsSelector :: Selector '[Id NSString, NSLayoutFormatOptions, Id NSDictionary, Id NSDictionary] (Id NSArray)
constraintsWithVisualFormat_options_metrics_viewsSelector = mkSelector "constraintsWithVisualFormat:options:metrics:views:"

-- | @Selector@ for @constraintWithItem:attribute:relatedBy:toItem:attribute:multiplier:constant:@
constraintWithItem_attribute_relatedBy_toItem_attribute_multiplier_constantSelector :: Selector '[RawId, NSLayoutAttribute, NSLayoutRelation, RawId, NSLayoutAttribute, CDouble, CDouble] (Id NSLayoutConstraint)
constraintWithItem_attribute_relatedBy_toItem_attribute_multiplier_constantSelector = mkSelector "constraintWithItem:attribute:relatedBy:toItem:attribute:multiplier:constant:"

-- | @Selector@ for @activateConstraints:@
activateConstraintsSelector :: Selector '[Id NSArray] ()
activateConstraintsSelector = mkSelector "activateConstraints:"

-- | @Selector@ for @deactivateConstraints:@
deactivateConstraintsSelector :: Selector '[Id NSArray] ()
deactivateConstraintsSelector = mkSelector "deactivateConstraints:"

-- | @Selector@ for @priority@
prioritySelector :: Selector '[] CFloat
prioritySelector = mkSelector "priority"

-- | @Selector@ for @setPriority:@
setPrioritySelector :: Selector '[CFloat] ()
setPrioritySelector = mkSelector "setPriority:"

-- | @Selector@ for @shouldBeArchived@
shouldBeArchivedSelector :: Selector '[] Bool
shouldBeArchivedSelector = mkSelector "shouldBeArchived"

-- | @Selector@ for @setShouldBeArchived:@
setShouldBeArchivedSelector :: Selector '[Bool] ()
setShouldBeArchivedSelector = mkSelector "setShouldBeArchived:"

-- | @Selector@ for @firstItem@
firstItemSelector :: Selector '[] RawId
firstItemSelector = mkSelector "firstItem"

-- | @Selector@ for @secondItem@
secondItemSelector :: Selector '[] RawId
secondItemSelector = mkSelector "secondItem"

-- | @Selector@ for @firstAttribute@
firstAttributeSelector :: Selector '[] NSLayoutAttribute
firstAttributeSelector = mkSelector "firstAttribute"

-- | @Selector@ for @secondAttribute@
secondAttributeSelector :: Selector '[] NSLayoutAttribute
secondAttributeSelector = mkSelector "secondAttribute"

-- | @Selector@ for @firstAnchor@
firstAnchorSelector :: Selector '[] (Id NSLayoutAnchor)
firstAnchorSelector = mkSelector "firstAnchor"

-- | @Selector@ for @secondAnchor@
secondAnchorSelector :: Selector '[] (Id NSLayoutAnchor)
secondAnchorSelector = mkSelector "secondAnchor"

-- | @Selector@ for @relation@
relationSelector :: Selector '[] NSLayoutRelation
relationSelector = mkSelector "relation"

-- | @Selector@ for @multiplier@
multiplierSelector :: Selector '[] CDouble
multiplierSelector = mkSelector "multiplier"

-- | @Selector@ for @constant@
constantSelector :: Selector '[] CDouble
constantSelector = mkSelector "constant"

-- | @Selector@ for @setConstant:@
setConstantSelector :: Selector '[CDouble] ()
setConstantSelector = mkSelector "setConstant:"

-- | @Selector@ for @active@
activeSelector :: Selector '[] Bool
activeSelector = mkSelector "active"

-- | @Selector@ for @setActive:@
setActiveSelector :: Selector '[Bool] ()
setActiveSelector = mkSelector "setActive:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector '[Id NSString] ()
setIdentifierSelector = mkSelector "setIdentifier:"

