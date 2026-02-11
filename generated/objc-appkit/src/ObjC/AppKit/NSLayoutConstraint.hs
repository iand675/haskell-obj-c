{-# LANGUAGE PatternSynonyms #-}
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
  , constraintsWithVisualFormat_options_metrics_viewsSelector
  , constraintWithItem_attribute_relatedBy_toItem_attribute_multiplier_constantSelector
  , activateConstraintsSelector
  , deactivateConstraintsSelector
  , prioritySelector
  , setPrioritySelector
  , shouldBeArchivedSelector
  , setShouldBeArchivedSelector
  , firstItemSelector
  , secondItemSelector
  , firstAttributeSelector
  , secondAttributeSelector
  , firstAnchorSelector
  , secondAnchorSelector
  , relationSelector
  , multiplierSelector
  , constantSelector
  , setConstantSelector
  , activeSelector
  , setActiveSelector
  , identifierSelector
  , setIdentifierSelector

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

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ constraintsWithVisualFormat:options:metrics:views:@
constraintsWithVisualFormat_options_metrics_views :: (IsNSString format, IsNSDictionary metrics, IsNSDictionary views) => format -> NSLayoutFormatOptions -> metrics -> views -> IO (Id NSArray)
constraintsWithVisualFormat_options_metrics_views format opts metrics views =
  do
    cls' <- getRequiredClass "NSLayoutConstraint"
    withObjCPtr format $ \raw_format ->
      withObjCPtr metrics $ \raw_metrics ->
        withObjCPtr views $ \raw_views ->
          sendClassMsg cls' (mkSelector "constraintsWithVisualFormat:options:metrics:views:") (retPtr retVoid) [argPtr (castPtr raw_format :: Ptr ()), argCULong (coerce opts), argPtr (castPtr raw_metrics :: Ptr ()), argPtr (castPtr raw_views :: Ptr ())] >>= retainedObject . castPtr

-- | @+ constraintWithItem:attribute:relatedBy:toItem:attribute:multiplier:constant:@
constraintWithItem_attribute_relatedBy_toItem_attribute_multiplier_constant :: RawId -> NSLayoutAttribute -> NSLayoutRelation -> RawId -> NSLayoutAttribute -> CDouble -> CDouble -> IO (Id NSLayoutConstraint)
constraintWithItem_attribute_relatedBy_toItem_attribute_multiplier_constant view1 attr1 relation view2 attr2 multiplier c =
  do
    cls' <- getRequiredClass "NSLayoutConstraint"
    sendClassMsg cls' (mkSelector "constraintWithItem:attribute:relatedBy:toItem:attribute:multiplier:constant:") (retPtr retVoid) [argPtr (castPtr (unRawId view1) :: Ptr ()), argCLong (coerce attr1), argCLong (coerce relation), argPtr (castPtr (unRawId view2) :: Ptr ()), argCLong (coerce attr2), argCDouble (fromIntegral multiplier), argCDouble (fromIntegral c)] >>= retainedObject . castPtr

-- | @+ activateConstraints:@
activateConstraints :: IsNSArray constraints => constraints -> IO ()
activateConstraints constraints =
  do
    cls' <- getRequiredClass "NSLayoutConstraint"
    withObjCPtr constraints $ \raw_constraints ->
      sendClassMsg cls' (mkSelector "activateConstraints:") retVoid [argPtr (castPtr raw_constraints :: Ptr ())]

-- | @+ deactivateConstraints:@
deactivateConstraints :: IsNSArray constraints => constraints -> IO ()
deactivateConstraints constraints =
  do
    cls' <- getRequiredClass "NSLayoutConstraint"
    withObjCPtr constraints $ \raw_constraints ->
      sendClassMsg cls' (mkSelector "deactivateConstraints:") retVoid [argPtr (castPtr raw_constraints :: Ptr ())]

-- | @- priority@
priority :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO CFloat
priority nsLayoutConstraint  =
  sendMsg nsLayoutConstraint (mkSelector "priority") retCFloat []

-- | @- setPriority:@
setPriority :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> CFloat -> IO ()
setPriority nsLayoutConstraint  value =
  sendMsg nsLayoutConstraint (mkSelector "setPriority:") retVoid [argCFloat (fromIntegral value)]

-- | @- shouldBeArchived@
shouldBeArchived :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO Bool
shouldBeArchived nsLayoutConstraint  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLayoutConstraint (mkSelector "shouldBeArchived") retCULong []

-- | @- setShouldBeArchived:@
setShouldBeArchived :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> Bool -> IO ()
setShouldBeArchived nsLayoutConstraint  value =
  sendMsg nsLayoutConstraint (mkSelector "setShouldBeArchived:") retVoid [argCULong (if value then 1 else 0)]

-- | @- firstItem@
firstItem :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO RawId
firstItem nsLayoutConstraint  =
  fmap (RawId . castPtr) $ sendMsg nsLayoutConstraint (mkSelector "firstItem") (retPtr retVoid) []

-- | @- secondItem@
secondItem :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO RawId
secondItem nsLayoutConstraint  =
  fmap (RawId . castPtr) $ sendMsg nsLayoutConstraint (mkSelector "secondItem") (retPtr retVoid) []

-- | @- firstAttribute@
firstAttribute :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO NSLayoutAttribute
firstAttribute nsLayoutConstraint  =
  fmap (coerce :: CLong -> NSLayoutAttribute) $ sendMsg nsLayoutConstraint (mkSelector "firstAttribute") retCLong []

-- | @- secondAttribute@
secondAttribute :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO NSLayoutAttribute
secondAttribute nsLayoutConstraint  =
  fmap (coerce :: CLong -> NSLayoutAttribute) $ sendMsg nsLayoutConstraint (mkSelector "secondAttribute") retCLong []

-- | @- firstAnchor@
firstAnchor :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO (Id NSLayoutAnchor)
firstAnchor nsLayoutConstraint  =
  sendMsg nsLayoutConstraint (mkSelector "firstAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- secondAnchor@
secondAnchor :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO (Id NSLayoutAnchor)
secondAnchor nsLayoutConstraint  =
  sendMsg nsLayoutConstraint (mkSelector "secondAnchor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- relation@
relation :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO NSLayoutRelation
relation nsLayoutConstraint  =
  fmap (coerce :: CLong -> NSLayoutRelation) $ sendMsg nsLayoutConstraint (mkSelector "relation") retCLong []

-- | @- multiplier@
multiplier :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO CDouble
multiplier nsLayoutConstraint  =
  sendMsg nsLayoutConstraint (mkSelector "multiplier") retCDouble []

-- | @- constant@
constant :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO CDouble
constant nsLayoutConstraint  =
  sendMsg nsLayoutConstraint (mkSelector "constant") retCDouble []

-- | @- setConstant:@
setConstant :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> CDouble -> IO ()
setConstant nsLayoutConstraint  value =
  sendMsg nsLayoutConstraint (mkSelector "setConstant:") retVoid [argCDouble (fromIntegral value)]

-- | @- active@
active :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO Bool
active nsLayoutConstraint  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLayoutConstraint (mkSelector "active") retCULong []

-- | @- setActive:@
setActive :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> Bool -> IO ()
setActive nsLayoutConstraint  value =
  sendMsg nsLayoutConstraint (mkSelector "setActive:") retVoid [argCULong (if value then 1 else 0)]

-- | @- identifier@
identifier :: IsNSLayoutConstraint nsLayoutConstraint => nsLayoutConstraint -> IO (Id NSString)
identifier nsLayoutConstraint  =
  sendMsg nsLayoutConstraint (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIdentifier:@
setIdentifier :: (IsNSLayoutConstraint nsLayoutConstraint, IsNSString value) => nsLayoutConstraint -> value -> IO ()
setIdentifier nsLayoutConstraint  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsLayoutConstraint (mkSelector "setIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @constraintsWithVisualFormat:options:metrics:views:@
constraintsWithVisualFormat_options_metrics_viewsSelector :: Selector
constraintsWithVisualFormat_options_metrics_viewsSelector = mkSelector "constraintsWithVisualFormat:options:metrics:views:"

-- | @Selector@ for @constraintWithItem:attribute:relatedBy:toItem:attribute:multiplier:constant:@
constraintWithItem_attribute_relatedBy_toItem_attribute_multiplier_constantSelector :: Selector
constraintWithItem_attribute_relatedBy_toItem_attribute_multiplier_constantSelector = mkSelector "constraintWithItem:attribute:relatedBy:toItem:attribute:multiplier:constant:"

-- | @Selector@ for @activateConstraints:@
activateConstraintsSelector :: Selector
activateConstraintsSelector = mkSelector "activateConstraints:"

-- | @Selector@ for @deactivateConstraints:@
deactivateConstraintsSelector :: Selector
deactivateConstraintsSelector = mkSelector "deactivateConstraints:"

-- | @Selector@ for @priority@
prioritySelector :: Selector
prioritySelector = mkSelector "priority"

-- | @Selector@ for @setPriority:@
setPrioritySelector :: Selector
setPrioritySelector = mkSelector "setPriority:"

-- | @Selector@ for @shouldBeArchived@
shouldBeArchivedSelector :: Selector
shouldBeArchivedSelector = mkSelector "shouldBeArchived"

-- | @Selector@ for @setShouldBeArchived:@
setShouldBeArchivedSelector :: Selector
setShouldBeArchivedSelector = mkSelector "setShouldBeArchived:"

-- | @Selector@ for @firstItem@
firstItemSelector :: Selector
firstItemSelector = mkSelector "firstItem"

-- | @Selector@ for @secondItem@
secondItemSelector :: Selector
secondItemSelector = mkSelector "secondItem"

-- | @Selector@ for @firstAttribute@
firstAttributeSelector :: Selector
firstAttributeSelector = mkSelector "firstAttribute"

-- | @Selector@ for @secondAttribute@
secondAttributeSelector :: Selector
secondAttributeSelector = mkSelector "secondAttribute"

-- | @Selector@ for @firstAnchor@
firstAnchorSelector :: Selector
firstAnchorSelector = mkSelector "firstAnchor"

-- | @Selector@ for @secondAnchor@
secondAnchorSelector :: Selector
secondAnchorSelector = mkSelector "secondAnchor"

-- | @Selector@ for @relation@
relationSelector :: Selector
relationSelector = mkSelector "relation"

-- | @Selector@ for @multiplier@
multiplierSelector :: Selector
multiplierSelector = mkSelector "multiplier"

-- | @Selector@ for @constant@
constantSelector :: Selector
constantSelector = mkSelector "constant"

-- | @Selector@ for @setConstant:@
setConstantSelector :: Selector
setConstantSelector = mkSelector "setConstant:"

-- | @Selector@ for @active@
activeSelector :: Selector
activeSelector = mkSelector "active"

-- | @Selector@ for @setActive:@
setActiveSelector :: Selector
setActiveSelector = mkSelector "setActive:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector
setIdentifierSelector = mkSelector "setIdentifier:"

