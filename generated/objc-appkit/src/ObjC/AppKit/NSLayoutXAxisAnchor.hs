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
anchorWithOffsetToAnchor :: (IsNSLayoutXAxisAnchor nsLayoutXAxisAnchor, IsNSLayoutXAxisAnchor otherAnchor) => nsLayoutXAxisAnchor -> otherAnchor -> IO (Id NSLayoutDimension)
anchorWithOffsetToAnchor nsLayoutXAxisAnchor  otherAnchor =
withObjCPtr otherAnchor $ \raw_otherAnchor ->
    sendMsg nsLayoutXAxisAnchor (mkSelector "anchorWithOffsetToAnchor:") (retPtr retVoid) [argPtr (castPtr raw_otherAnchor :: Ptr ())] >>= retainedObject . castPtr

-- | @- constraintEqualToSystemSpacingAfterAnchor:multiplier:@
constraintEqualToSystemSpacingAfterAnchor_multiplier :: (IsNSLayoutXAxisAnchor nsLayoutXAxisAnchor, IsNSLayoutXAxisAnchor anchor) => nsLayoutXAxisAnchor -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintEqualToSystemSpacingAfterAnchor_multiplier nsLayoutXAxisAnchor  anchor multiplier =
withObjCPtr anchor $ \raw_anchor ->
    sendMsg nsLayoutXAxisAnchor (mkSelector "constraintEqualToSystemSpacingAfterAnchor:multiplier:") (retPtr retVoid) [argPtr (castPtr raw_anchor :: Ptr ()), argCDouble (fromIntegral multiplier)] >>= retainedObject . castPtr

-- | @- constraintGreaterThanOrEqualToSystemSpacingAfterAnchor:multiplier:@
constraintGreaterThanOrEqualToSystemSpacingAfterAnchor_multiplier :: (IsNSLayoutXAxisAnchor nsLayoutXAxisAnchor, IsNSLayoutXAxisAnchor anchor) => nsLayoutXAxisAnchor -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintGreaterThanOrEqualToSystemSpacingAfterAnchor_multiplier nsLayoutXAxisAnchor  anchor multiplier =
withObjCPtr anchor $ \raw_anchor ->
    sendMsg nsLayoutXAxisAnchor (mkSelector "constraintGreaterThanOrEqualToSystemSpacingAfterAnchor:multiplier:") (retPtr retVoid) [argPtr (castPtr raw_anchor :: Ptr ()), argCDouble (fromIntegral multiplier)] >>= retainedObject . castPtr

-- | @- constraintLessThanOrEqualToSystemSpacingAfterAnchor:multiplier:@
constraintLessThanOrEqualToSystemSpacingAfterAnchor_multiplier :: (IsNSLayoutXAxisAnchor nsLayoutXAxisAnchor, IsNSLayoutXAxisAnchor anchor) => nsLayoutXAxisAnchor -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintLessThanOrEqualToSystemSpacingAfterAnchor_multiplier nsLayoutXAxisAnchor  anchor multiplier =
withObjCPtr anchor $ \raw_anchor ->
    sendMsg nsLayoutXAxisAnchor (mkSelector "constraintLessThanOrEqualToSystemSpacingAfterAnchor:multiplier:") (retPtr retVoid) [argPtr (castPtr raw_anchor :: Ptr ()), argCDouble (fromIntegral multiplier)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @anchorWithOffsetToAnchor:@
anchorWithOffsetToAnchorSelector :: Selector
anchorWithOffsetToAnchorSelector = mkSelector "anchorWithOffsetToAnchor:"

-- | @Selector@ for @constraintEqualToSystemSpacingAfterAnchor:multiplier:@
constraintEqualToSystemSpacingAfterAnchor_multiplierSelector :: Selector
constraintEqualToSystemSpacingAfterAnchor_multiplierSelector = mkSelector "constraintEqualToSystemSpacingAfterAnchor:multiplier:"

-- | @Selector@ for @constraintGreaterThanOrEqualToSystemSpacingAfterAnchor:multiplier:@
constraintGreaterThanOrEqualToSystemSpacingAfterAnchor_multiplierSelector :: Selector
constraintGreaterThanOrEqualToSystemSpacingAfterAnchor_multiplierSelector = mkSelector "constraintGreaterThanOrEqualToSystemSpacingAfterAnchor:multiplier:"

-- | @Selector@ for @constraintLessThanOrEqualToSystemSpacingAfterAnchor:multiplier:@
constraintLessThanOrEqualToSystemSpacingAfterAnchor_multiplierSelector :: Selector
constraintLessThanOrEqualToSystemSpacingAfterAnchor_multiplierSelector = mkSelector "constraintLessThanOrEqualToSystemSpacingAfterAnchor:multiplier:"

