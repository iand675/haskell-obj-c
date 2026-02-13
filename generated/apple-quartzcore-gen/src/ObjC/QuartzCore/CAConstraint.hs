{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The class representing a single layout constraint. *
--
-- Generated bindings for @CAConstraint@.
module ObjC.QuartzCore.CAConstraint
  ( CAConstraint
  , IsCAConstraint(..)
  , constraintWithAttribute_relativeTo_attribute_scale_offset
  , constraintWithAttribute_relativeTo_attribute_offset
  , constraintWithAttribute_relativeTo_attribute
  , initWithAttribute_relativeTo_attribute_scale_offset
  , attribute
  , sourceName
  , sourceAttribute
  , scale
  , offset
  , attributeSelector
  , constraintWithAttribute_relativeTo_attributeSelector
  , constraintWithAttribute_relativeTo_attribute_offsetSelector
  , constraintWithAttribute_relativeTo_attribute_scale_offsetSelector
  , initWithAttribute_relativeTo_attribute_scale_offsetSelector
  , offsetSelector
  , scaleSelector
  , sourceAttributeSelector
  , sourceNameSelector

  -- * Enum types
  , CAConstraintAttribute(CAConstraintAttribute)
  , pattern KCAConstraintMinX
  , pattern KCAConstraintMidX
  , pattern KCAConstraintMaxX
  , pattern KCAConstraintWidth
  , pattern KCAConstraintMinY
  , pattern KCAConstraintMidY
  , pattern KCAConstraintMaxY
  , pattern KCAConstraintHeight

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuartzCore.Internal.Classes
import ObjC.QuartzCore.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ constraintWithAttribute:relativeTo:attribute:scale:offset:@
constraintWithAttribute_relativeTo_attribute_scale_offset :: IsNSString srcId => CAConstraintAttribute -> srcId -> CAConstraintAttribute -> CDouble -> CDouble -> IO (Id CAConstraint)
constraintWithAttribute_relativeTo_attribute_scale_offset attr srcId srcAttr m c =
  do
    cls' <- getRequiredClass "CAConstraint"
    sendClassMessage cls' constraintWithAttribute_relativeTo_attribute_scale_offsetSelector attr (toNSString srcId) srcAttr m c

-- | @+ constraintWithAttribute:relativeTo:attribute:offset:@
constraintWithAttribute_relativeTo_attribute_offset :: IsNSString srcId => CAConstraintAttribute -> srcId -> CAConstraintAttribute -> CDouble -> IO (Id CAConstraint)
constraintWithAttribute_relativeTo_attribute_offset attr srcId srcAttr c =
  do
    cls' <- getRequiredClass "CAConstraint"
    sendClassMessage cls' constraintWithAttribute_relativeTo_attribute_offsetSelector attr (toNSString srcId) srcAttr c

-- | @+ constraintWithAttribute:relativeTo:attribute:@
constraintWithAttribute_relativeTo_attribute :: IsNSString srcId => CAConstraintAttribute -> srcId -> CAConstraintAttribute -> IO (Id CAConstraint)
constraintWithAttribute_relativeTo_attribute attr srcId srcAttr =
  do
    cls' <- getRequiredClass "CAConstraint"
    sendClassMessage cls' constraintWithAttribute_relativeTo_attributeSelector attr (toNSString srcId) srcAttr

-- | @- initWithAttribute:relativeTo:attribute:scale:offset:@
initWithAttribute_relativeTo_attribute_scale_offset :: (IsCAConstraint caConstraint, IsNSString srcId) => caConstraint -> CAConstraintAttribute -> srcId -> CAConstraintAttribute -> CDouble -> CDouble -> IO (Id CAConstraint)
initWithAttribute_relativeTo_attribute_scale_offset caConstraint attr srcId srcAttr m c =
  sendOwnedMessage caConstraint initWithAttribute_relativeTo_attribute_scale_offsetSelector attr (toNSString srcId) srcAttr m c

-- | @- attribute@
attribute :: IsCAConstraint caConstraint => caConstraint -> IO CAConstraintAttribute
attribute caConstraint =
  sendMessage caConstraint attributeSelector

-- | @- sourceName@
sourceName :: IsCAConstraint caConstraint => caConstraint -> IO (Id NSString)
sourceName caConstraint =
  sendMessage caConstraint sourceNameSelector

-- | @- sourceAttribute@
sourceAttribute :: IsCAConstraint caConstraint => caConstraint -> IO CAConstraintAttribute
sourceAttribute caConstraint =
  sendMessage caConstraint sourceAttributeSelector

-- | @- scale@
scale :: IsCAConstraint caConstraint => caConstraint -> IO CDouble
scale caConstraint =
  sendMessage caConstraint scaleSelector

-- | @- offset@
offset :: IsCAConstraint caConstraint => caConstraint -> IO CDouble
offset caConstraint =
  sendMessage caConstraint offsetSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @constraintWithAttribute:relativeTo:attribute:scale:offset:@
constraintWithAttribute_relativeTo_attribute_scale_offsetSelector :: Selector '[CAConstraintAttribute, Id NSString, CAConstraintAttribute, CDouble, CDouble] (Id CAConstraint)
constraintWithAttribute_relativeTo_attribute_scale_offsetSelector = mkSelector "constraintWithAttribute:relativeTo:attribute:scale:offset:"

-- | @Selector@ for @constraintWithAttribute:relativeTo:attribute:offset:@
constraintWithAttribute_relativeTo_attribute_offsetSelector :: Selector '[CAConstraintAttribute, Id NSString, CAConstraintAttribute, CDouble] (Id CAConstraint)
constraintWithAttribute_relativeTo_attribute_offsetSelector = mkSelector "constraintWithAttribute:relativeTo:attribute:offset:"

-- | @Selector@ for @constraintWithAttribute:relativeTo:attribute:@
constraintWithAttribute_relativeTo_attributeSelector :: Selector '[CAConstraintAttribute, Id NSString, CAConstraintAttribute] (Id CAConstraint)
constraintWithAttribute_relativeTo_attributeSelector = mkSelector "constraintWithAttribute:relativeTo:attribute:"

-- | @Selector@ for @initWithAttribute:relativeTo:attribute:scale:offset:@
initWithAttribute_relativeTo_attribute_scale_offsetSelector :: Selector '[CAConstraintAttribute, Id NSString, CAConstraintAttribute, CDouble, CDouble] (Id CAConstraint)
initWithAttribute_relativeTo_attribute_scale_offsetSelector = mkSelector "initWithAttribute:relativeTo:attribute:scale:offset:"

-- | @Selector@ for @attribute@
attributeSelector :: Selector '[] CAConstraintAttribute
attributeSelector = mkSelector "attribute"

-- | @Selector@ for @sourceName@
sourceNameSelector :: Selector '[] (Id NSString)
sourceNameSelector = mkSelector "sourceName"

-- | @Selector@ for @sourceAttribute@
sourceAttributeSelector :: Selector '[] CAConstraintAttribute
sourceAttributeSelector = mkSelector "sourceAttribute"

-- | @Selector@ for @scale@
scaleSelector :: Selector '[] CDouble
scaleSelector = mkSelector "scale"

-- | @Selector@ for @offset@
offsetSelector :: Selector '[] CDouble
offsetSelector = mkSelector "offset"

