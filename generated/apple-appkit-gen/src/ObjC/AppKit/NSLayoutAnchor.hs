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
  , constraintGreaterThanOrEqualToAnchorSelector
  , constraintLessThanOrEqualToAnchorSelector
  , constraintEqualToAnchor_constantSelector
  , constraintGreaterThanOrEqualToAnchor_constantSelector
  , constraintLessThanOrEqualToAnchor_constantSelector
  , nameSelector
  , itemSelector
  , hasAmbiguousLayoutSelector
  , constraintsAffectingLayoutSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- constraintEqualToAnchor:@
constraintEqualToAnchor :: (IsNSLayoutAnchor nsLayoutAnchor, IsNSLayoutAnchor anchor) => nsLayoutAnchor -> anchor -> IO (Id NSLayoutConstraint)
constraintEqualToAnchor nsLayoutAnchor  anchor =
  withObjCPtr anchor $ \raw_anchor ->
      sendMsg nsLayoutAnchor (mkSelector "constraintEqualToAnchor:") (retPtr retVoid) [argPtr (castPtr raw_anchor :: Ptr ())] >>= retainedObject . castPtr

-- | @- constraintGreaterThanOrEqualToAnchor:@
constraintGreaterThanOrEqualToAnchor :: (IsNSLayoutAnchor nsLayoutAnchor, IsNSLayoutAnchor anchor) => nsLayoutAnchor -> anchor -> IO (Id NSLayoutConstraint)
constraintGreaterThanOrEqualToAnchor nsLayoutAnchor  anchor =
  withObjCPtr anchor $ \raw_anchor ->
      sendMsg nsLayoutAnchor (mkSelector "constraintGreaterThanOrEqualToAnchor:") (retPtr retVoid) [argPtr (castPtr raw_anchor :: Ptr ())] >>= retainedObject . castPtr

-- | @- constraintLessThanOrEqualToAnchor:@
constraintLessThanOrEqualToAnchor :: (IsNSLayoutAnchor nsLayoutAnchor, IsNSLayoutAnchor anchor) => nsLayoutAnchor -> anchor -> IO (Id NSLayoutConstraint)
constraintLessThanOrEqualToAnchor nsLayoutAnchor  anchor =
  withObjCPtr anchor $ \raw_anchor ->
      sendMsg nsLayoutAnchor (mkSelector "constraintLessThanOrEqualToAnchor:") (retPtr retVoid) [argPtr (castPtr raw_anchor :: Ptr ())] >>= retainedObject . castPtr

-- | @- constraintEqualToAnchor:constant:@
constraintEqualToAnchor_constant :: (IsNSLayoutAnchor nsLayoutAnchor, IsNSLayoutAnchor anchor) => nsLayoutAnchor -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintEqualToAnchor_constant nsLayoutAnchor  anchor c =
  withObjCPtr anchor $ \raw_anchor ->
      sendMsg nsLayoutAnchor (mkSelector "constraintEqualToAnchor:constant:") (retPtr retVoid) [argPtr (castPtr raw_anchor :: Ptr ()), argCDouble c] >>= retainedObject . castPtr

-- | @- constraintGreaterThanOrEqualToAnchor:constant:@
constraintGreaterThanOrEqualToAnchor_constant :: (IsNSLayoutAnchor nsLayoutAnchor, IsNSLayoutAnchor anchor) => nsLayoutAnchor -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintGreaterThanOrEqualToAnchor_constant nsLayoutAnchor  anchor c =
  withObjCPtr anchor $ \raw_anchor ->
      sendMsg nsLayoutAnchor (mkSelector "constraintGreaterThanOrEqualToAnchor:constant:") (retPtr retVoid) [argPtr (castPtr raw_anchor :: Ptr ()), argCDouble c] >>= retainedObject . castPtr

-- | @- constraintLessThanOrEqualToAnchor:constant:@
constraintLessThanOrEqualToAnchor_constant :: (IsNSLayoutAnchor nsLayoutAnchor, IsNSLayoutAnchor anchor) => nsLayoutAnchor -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintLessThanOrEqualToAnchor_constant nsLayoutAnchor  anchor c =
  withObjCPtr anchor $ \raw_anchor ->
      sendMsg nsLayoutAnchor (mkSelector "constraintLessThanOrEqualToAnchor:constant:") (retPtr retVoid) [argPtr (castPtr raw_anchor :: Ptr ()), argCDouble c] >>= retainedObject . castPtr

-- | @- name@
name :: IsNSLayoutAnchor nsLayoutAnchor => nsLayoutAnchor -> IO (Id NSString)
name nsLayoutAnchor  =
    sendMsg nsLayoutAnchor (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- item@
item :: IsNSLayoutAnchor nsLayoutAnchor => nsLayoutAnchor -> IO RawId
item nsLayoutAnchor  =
    fmap (RawId . castPtr) $ sendMsg nsLayoutAnchor (mkSelector "item") (retPtr retVoid) []

-- | @- hasAmbiguousLayout@
hasAmbiguousLayout :: IsNSLayoutAnchor nsLayoutAnchor => nsLayoutAnchor -> IO Bool
hasAmbiguousLayout nsLayoutAnchor  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsLayoutAnchor (mkSelector "hasAmbiguousLayout") retCULong []

-- | @- constraintsAffectingLayout@
constraintsAffectingLayout :: IsNSLayoutAnchor nsLayoutAnchor => nsLayoutAnchor -> IO (Id NSArray)
constraintsAffectingLayout nsLayoutAnchor  =
    sendMsg nsLayoutAnchor (mkSelector "constraintsAffectingLayout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @constraintEqualToAnchor:@
constraintEqualToAnchorSelector :: Selector
constraintEqualToAnchorSelector = mkSelector "constraintEqualToAnchor:"

-- | @Selector@ for @constraintGreaterThanOrEqualToAnchor:@
constraintGreaterThanOrEqualToAnchorSelector :: Selector
constraintGreaterThanOrEqualToAnchorSelector = mkSelector "constraintGreaterThanOrEqualToAnchor:"

-- | @Selector@ for @constraintLessThanOrEqualToAnchor:@
constraintLessThanOrEqualToAnchorSelector :: Selector
constraintLessThanOrEqualToAnchorSelector = mkSelector "constraintLessThanOrEqualToAnchor:"

-- | @Selector@ for @constraintEqualToAnchor:constant:@
constraintEqualToAnchor_constantSelector :: Selector
constraintEqualToAnchor_constantSelector = mkSelector "constraintEqualToAnchor:constant:"

-- | @Selector@ for @constraintGreaterThanOrEqualToAnchor:constant:@
constraintGreaterThanOrEqualToAnchor_constantSelector :: Selector
constraintGreaterThanOrEqualToAnchor_constantSelector = mkSelector "constraintGreaterThanOrEqualToAnchor:constant:"

-- | @Selector@ for @constraintLessThanOrEqualToAnchor:constant:@
constraintLessThanOrEqualToAnchor_constantSelector :: Selector
constraintLessThanOrEqualToAnchor_constantSelector = mkSelector "constraintLessThanOrEqualToAnchor:constant:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @item@
itemSelector :: Selector
itemSelector = mkSelector "item"

-- | @Selector@ for @hasAmbiguousLayout@
hasAmbiguousLayoutSelector :: Selector
hasAmbiguousLayoutSelector = mkSelector "hasAmbiguousLayout"

-- | @Selector@ for @constraintsAffectingLayout@
constraintsAffectingLayoutSelector :: Selector
constraintsAffectingLayoutSelector = mkSelector "constraintsAffectingLayout"

