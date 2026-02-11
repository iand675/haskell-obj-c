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

-- | @- anchorWithOffsetToAnchor:@
anchorWithOffsetToAnchor :: (IsNSLayoutYAxisAnchor nsLayoutYAxisAnchor, IsNSLayoutYAxisAnchor otherAnchor) => nsLayoutYAxisAnchor -> otherAnchor -> IO (Id NSLayoutDimension)
anchorWithOffsetToAnchor nsLayoutYAxisAnchor  otherAnchor =
withObjCPtr otherAnchor $ \raw_otherAnchor ->
    sendMsg nsLayoutYAxisAnchor (mkSelector "anchorWithOffsetToAnchor:") (retPtr retVoid) [argPtr (castPtr raw_otherAnchor :: Ptr ())] >>= retainedObject . castPtr

-- | @- constraintEqualToSystemSpacingBelowAnchor:multiplier:@
constraintEqualToSystemSpacingBelowAnchor_multiplier :: (IsNSLayoutYAxisAnchor nsLayoutYAxisAnchor, IsNSLayoutYAxisAnchor anchor) => nsLayoutYAxisAnchor -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintEqualToSystemSpacingBelowAnchor_multiplier nsLayoutYAxisAnchor  anchor multiplier =
withObjCPtr anchor $ \raw_anchor ->
    sendMsg nsLayoutYAxisAnchor (mkSelector "constraintEqualToSystemSpacingBelowAnchor:multiplier:") (retPtr retVoid) [argPtr (castPtr raw_anchor :: Ptr ()), argCDouble (fromIntegral multiplier)] >>= retainedObject . castPtr

-- | @- constraintGreaterThanOrEqualToSystemSpacingBelowAnchor:multiplier:@
constraintGreaterThanOrEqualToSystemSpacingBelowAnchor_multiplier :: (IsNSLayoutYAxisAnchor nsLayoutYAxisAnchor, IsNSLayoutYAxisAnchor anchor) => nsLayoutYAxisAnchor -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintGreaterThanOrEqualToSystemSpacingBelowAnchor_multiplier nsLayoutYAxisAnchor  anchor multiplier =
withObjCPtr anchor $ \raw_anchor ->
    sendMsg nsLayoutYAxisAnchor (mkSelector "constraintGreaterThanOrEqualToSystemSpacingBelowAnchor:multiplier:") (retPtr retVoid) [argPtr (castPtr raw_anchor :: Ptr ()), argCDouble (fromIntegral multiplier)] >>= retainedObject . castPtr

-- | @- constraintLessThanOrEqualToSystemSpacingBelowAnchor:multiplier:@
constraintLessThanOrEqualToSystemSpacingBelowAnchor_multiplier :: (IsNSLayoutYAxisAnchor nsLayoutYAxisAnchor, IsNSLayoutYAxisAnchor anchor) => nsLayoutYAxisAnchor -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintLessThanOrEqualToSystemSpacingBelowAnchor_multiplier nsLayoutYAxisAnchor  anchor multiplier =
withObjCPtr anchor $ \raw_anchor ->
    sendMsg nsLayoutYAxisAnchor (mkSelector "constraintLessThanOrEqualToSystemSpacingBelowAnchor:multiplier:") (retPtr retVoid) [argPtr (castPtr raw_anchor :: Ptr ()), argCDouble (fromIntegral multiplier)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @anchorWithOffsetToAnchor:@
anchorWithOffsetToAnchorSelector :: Selector
anchorWithOffsetToAnchorSelector = mkSelector "anchorWithOffsetToAnchor:"

-- | @Selector@ for @constraintEqualToSystemSpacingBelowAnchor:multiplier:@
constraintEqualToSystemSpacingBelowAnchor_multiplierSelector :: Selector
constraintEqualToSystemSpacingBelowAnchor_multiplierSelector = mkSelector "constraintEqualToSystemSpacingBelowAnchor:multiplier:"

-- | @Selector@ for @constraintGreaterThanOrEqualToSystemSpacingBelowAnchor:multiplier:@
constraintGreaterThanOrEqualToSystemSpacingBelowAnchor_multiplierSelector :: Selector
constraintGreaterThanOrEqualToSystemSpacingBelowAnchor_multiplierSelector = mkSelector "constraintGreaterThanOrEqualToSystemSpacingBelowAnchor:multiplier:"

-- | @Selector@ for @constraintLessThanOrEqualToSystemSpacingBelowAnchor:multiplier:@
constraintLessThanOrEqualToSystemSpacingBelowAnchor_multiplierSelector :: Selector
constraintLessThanOrEqualToSystemSpacingBelowAnchor_multiplierSelector = mkSelector "constraintLessThanOrEqualToSystemSpacingBelowAnchor:multiplier:"

