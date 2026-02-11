{-# LANGUAGE PatternSynonyms #-}
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
  , constraintWithAttribute_relativeTo_attribute_scale_offsetSelector
  , constraintWithAttribute_relativeTo_attribute_offsetSelector
  , constraintWithAttribute_relativeTo_attributeSelector
  , initWithAttribute_relativeTo_attribute_scale_offsetSelector
  , attributeSelector
  , sourceNameSelector
  , sourceAttributeSelector
  , scaleSelector
  , offsetSelector

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

import ObjC.QuartzCore.Internal.Classes
import ObjC.QuartzCore.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ constraintWithAttribute:relativeTo:attribute:scale:offset:@
constraintWithAttribute_relativeTo_attribute_scale_offset :: IsNSString srcId => CAConstraintAttribute -> srcId -> CAConstraintAttribute -> CDouble -> CDouble -> IO (Id CAConstraint)
constraintWithAttribute_relativeTo_attribute_scale_offset attr srcId srcAttr m c =
  do
    cls' <- getRequiredClass "CAConstraint"
    withObjCPtr srcId $ \raw_srcId ->
      sendClassMsg cls' (mkSelector "constraintWithAttribute:relativeTo:attribute:scale:offset:") (retPtr retVoid) [argCInt (coerce attr), argPtr (castPtr raw_srcId :: Ptr ()), argCInt (coerce srcAttr), argCDouble (fromIntegral m), argCDouble (fromIntegral c)] >>= retainedObject . castPtr

-- | @+ constraintWithAttribute:relativeTo:attribute:offset:@
constraintWithAttribute_relativeTo_attribute_offset :: IsNSString srcId => CAConstraintAttribute -> srcId -> CAConstraintAttribute -> CDouble -> IO (Id CAConstraint)
constraintWithAttribute_relativeTo_attribute_offset attr srcId srcAttr c =
  do
    cls' <- getRequiredClass "CAConstraint"
    withObjCPtr srcId $ \raw_srcId ->
      sendClassMsg cls' (mkSelector "constraintWithAttribute:relativeTo:attribute:offset:") (retPtr retVoid) [argCInt (coerce attr), argPtr (castPtr raw_srcId :: Ptr ()), argCInt (coerce srcAttr), argCDouble (fromIntegral c)] >>= retainedObject . castPtr

-- | @+ constraintWithAttribute:relativeTo:attribute:@
constraintWithAttribute_relativeTo_attribute :: IsNSString srcId => CAConstraintAttribute -> srcId -> CAConstraintAttribute -> IO (Id CAConstraint)
constraintWithAttribute_relativeTo_attribute attr srcId srcAttr =
  do
    cls' <- getRequiredClass "CAConstraint"
    withObjCPtr srcId $ \raw_srcId ->
      sendClassMsg cls' (mkSelector "constraintWithAttribute:relativeTo:attribute:") (retPtr retVoid) [argCInt (coerce attr), argPtr (castPtr raw_srcId :: Ptr ()), argCInt (coerce srcAttr)] >>= retainedObject . castPtr

-- | @- initWithAttribute:relativeTo:attribute:scale:offset:@
initWithAttribute_relativeTo_attribute_scale_offset :: (IsCAConstraint caConstraint, IsNSString srcId) => caConstraint -> CAConstraintAttribute -> srcId -> CAConstraintAttribute -> CDouble -> CDouble -> IO (Id CAConstraint)
initWithAttribute_relativeTo_attribute_scale_offset caConstraint  attr srcId srcAttr m c =
withObjCPtr srcId $ \raw_srcId ->
    sendMsg caConstraint (mkSelector "initWithAttribute:relativeTo:attribute:scale:offset:") (retPtr retVoid) [argCInt (coerce attr), argPtr (castPtr raw_srcId :: Ptr ()), argCInt (coerce srcAttr), argCDouble (fromIntegral m), argCDouble (fromIntegral c)] >>= ownedObject . castPtr

-- | @- attribute@
attribute :: IsCAConstraint caConstraint => caConstraint -> IO CAConstraintAttribute
attribute caConstraint  =
  fmap (coerce :: CInt -> CAConstraintAttribute) $ sendMsg caConstraint (mkSelector "attribute") retCInt []

-- | @- sourceName@
sourceName :: IsCAConstraint caConstraint => caConstraint -> IO (Id NSString)
sourceName caConstraint  =
  sendMsg caConstraint (mkSelector "sourceName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- sourceAttribute@
sourceAttribute :: IsCAConstraint caConstraint => caConstraint -> IO CAConstraintAttribute
sourceAttribute caConstraint  =
  fmap (coerce :: CInt -> CAConstraintAttribute) $ sendMsg caConstraint (mkSelector "sourceAttribute") retCInt []

-- | @- scale@
scale :: IsCAConstraint caConstraint => caConstraint -> IO CDouble
scale caConstraint  =
  sendMsg caConstraint (mkSelector "scale") retCDouble []

-- | @- offset@
offset :: IsCAConstraint caConstraint => caConstraint -> IO CDouble
offset caConstraint  =
  sendMsg caConstraint (mkSelector "offset") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @constraintWithAttribute:relativeTo:attribute:scale:offset:@
constraintWithAttribute_relativeTo_attribute_scale_offsetSelector :: Selector
constraintWithAttribute_relativeTo_attribute_scale_offsetSelector = mkSelector "constraintWithAttribute:relativeTo:attribute:scale:offset:"

-- | @Selector@ for @constraintWithAttribute:relativeTo:attribute:offset:@
constraintWithAttribute_relativeTo_attribute_offsetSelector :: Selector
constraintWithAttribute_relativeTo_attribute_offsetSelector = mkSelector "constraintWithAttribute:relativeTo:attribute:offset:"

-- | @Selector@ for @constraintWithAttribute:relativeTo:attribute:@
constraintWithAttribute_relativeTo_attributeSelector :: Selector
constraintWithAttribute_relativeTo_attributeSelector = mkSelector "constraintWithAttribute:relativeTo:attribute:"

-- | @Selector@ for @initWithAttribute:relativeTo:attribute:scale:offset:@
initWithAttribute_relativeTo_attribute_scale_offsetSelector :: Selector
initWithAttribute_relativeTo_attribute_scale_offsetSelector = mkSelector "initWithAttribute:relativeTo:attribute:scale:offset:"

-- | @Selector@ for @attribute@
attributeSelector :: Selector
attributeSelector = mkSelector "attribute"

-- | @Selector@ for @sourceName@
sourceNameSelector :: Selector
sourceNameSelector = mkSelector "sourceName"

-- | @Selector@ for @sourceAttribute@
sourceAttributeSelector :: Selector
sourceAttributeSelector = mkSelector "sourceAttribute"

-- | @Selector@ for @scale@
scaleSelector :: Selector
scaleSelector = mkSelector "scale"

-- | @Selector@ for @offset@
offsetSelector :: Selector
offsetSelector = mkSelector "offset"

