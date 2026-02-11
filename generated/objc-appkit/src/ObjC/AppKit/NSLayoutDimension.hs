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
  , constraintEqualToConstantSelector
  , constraintGreaterThanOrEqualToConstantSelector
  , constraintLessThanOrEqualToConstantSelector
  , constraintEqualToAnchor_multiplierSelector
  , constraintGreaterThanOrEqualToAnchor_multiplierSelector
  , constraintLessThanOrEqualToAnchor_multiplierSelector
  , constraintEqualToAnchor_multiplier_constantSelector
  , constraintGreaterThanOrEqualToAnchor_multiplier_constantSelector
  , constraintLessThanOrEqualToAnchor_multiplier_constantSelector


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

-- | @- constraintEqualToConstant:@
constraintEqualToConstant :: IsNSLayoutDimension nsLayoutDimension => nsLayoutDimension -> CDouble -> IO (Id NSLayoutConstraint)
constraintEqualToConstant nsLayoutDimension  c =
  sendMsg nsLayoutDimension (mkSelector "constraintEqualToConstant:") (retPtr retVoid) [argCDouble (fromIntegral c)] >>= retainedObject . castPtr

-- | @- constraintGreaterThanOrEqualToConstant:@
constraintGreaterThanOrEqualToConstant :: IsNSLayoutDimension nsLayoutDimension => nsLayoutDimension -> CDouble -> IO (Id NSLayoutConstraint)
constraintGreaterThanOrEqualToConstant nsLayoutDimension  c =
  sendMsg nsLayoutDimension (mkSelector "constraintGreaterThanOrEqualToConstant:") (retPtr retVoid) [argCDouble (fromIntegral c)] >>= retainedObject . castPtr

-- | @- constraintLessThanOrEqualToConstant:@
constraintLessThanOrEqualToConstant :: IsNSLayoutDimension nsLayoutDimension => nsLayoutDimension -> CDouble -> IO (Id NSLayoutConstraint)
constraintLessThanOrEqualToConstant nsLayoutDimension  c =
  sendMsg nsLayoutDimension (mkSelector "constraintLessThanOrEqualToConstant:") (retPtr retVoid) [argCDouble (fromIntegral c)] >>= retainedObject . castPtr

-- | @- constraintEqualToAnchor:multiplier:@
constraintEqualToAnchor_multiplier :: (IsNSLayoutDimension nsLayoutDimension, IsNSLayoutDimension anchor) => nsLayoutDimension -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintEqualToAnchor_multiplier nsLayoutDimension  anchor m =
withObjCPtr anchor $ \raw_anchor ->
    sendMsg nsLayoutDimension (mkSelector "constraintEqualToAnchor:multiplier:") (retPtr retVoid) [argPtr (castPtr raw_anchor :: Ptr ()), argCDouble (fromIntegral m)] >>= retainedObject . castPtr

-- | @- constraintGreaterThanOrEqualToAnchor:multiplier:@
constraintGreaterThanOrEqualToAnchor_multiplier :: (IsNSLayoutDimension nsLayoutDimension, IsNSLayoutDimension anchor) => nsLayoutDimension -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintGreaterThanOrEqualToAnchor_multiplier nsLayoutDimension  anchor m =
withObjCPtr anchor $ \raw_anchor ->
    sendMsg nsLayoutDimension (mkSelector "constraintGreaterThanOrEqualToAnchor:multiplier:") (retPtr retVoid) [argPtr (castPtr raw_anchor :: Ptr ()), argCDouble (fromIntegral m)] >>= retainedObject . castPtr

-- | @- constraintLessThanOrEqualToAnchor:multiplier:@
constraintLessThanOrEqualToAnchor_multiplier :: (IsNSLayoutDimension nsLayoutDimension, IsNSLayoutDimension anchor) => nsLayoutDimension -> anchor -> CDouble -> IO (Id NSLayoutConstraint)
constraintLessThanOrEqualToAnchor_multiplier nsLayoutDimension  anchor m =
withObjCPtr anchor $ \raw_anchor ->
    sendMsg nsLayoutDimension (mkSelector "constraintLessThanOrEqualToAnchor:multiplier:") (retPtr retVoid) [argPtr (castPtr raw_anchor :: Ptr ()), argCDouble (fromIntegral m)] >>= retainedObject . castPtr

-- | @- constraintEqualToAnchor:multiplier:constant:@
constraintEqualToAnchor_multiplier_constant :: (IsNSLayoutDimension nsLayoutDimension, IsNSLayoutDimension anchor) => nsLayoutDimension -> anchor -> CDouble -> CDouble -> IO (Id NSLayoutConstraint)
constraintEqualToAnchor_multiplier_constant nsLayoutDimension  anchor m c =
withObjCPtr anchor $ \raw_anchor ->
    sendMsg nsLayoutDimension (mkSelector "constraintEqualToAnchor:multiplier:constant:") (retPtr retVoid) [argPtr (castPtr raw_anchor :: Ptr ()), argCDouble (fromIntegral m), argCDouble (fromIntegral c)] >>= retainedObject . castPtr

-- | @- constraintGreaterThanOrEqualToAnchor:multiplier:constant:@
constraintGreaterThanOrEqualToAnchor_multiplier_constant :: (IsNSLayoutDimension nsLayoutDimension, IsNSLayoutDimension anchor) => nsLayoutDimension -> anchor -> CDouble -> CDouble -> IO (Id NSLayoutConstraint)
constraintGreaterThanOrEqualToAnchor_multiplier_constant nsLayoutDimension  anchor m c =
withObjCPtr anchor $ \raw_anchor ->
    sendMsg nsLayoutDimension (mkSelector "constraintGreaterThanOrEqualToAnchor:multiplier:constant:") (retPtr retVoid) [argPtr (castPtr raw_anchor :: Ptr ()), argCDouble (fromIntegral m), argCDouble (fromIntegral c)] >>= retainedObject . castPtr

-- | @- constraintLessThanOrEqualToAnchor:multiplier:constant:@
constraintLessThanOrEqualToAnchor_multiplier_constant :: (IsNSLayoutDimension nsLayoutDimension, IsNSLayoutDimension anchor) => nsLayoutDimension -> anchor -> CDouble -> CDouble -> IO (Id NSLayoutConstraint)
constraintLessThanOrEqualToAnchor_multiplier_constant nsLayoutDimension  anchor m c =
withObjCPtr anchor $ \raw_anchor ->
    sendMsg nsLayoutDimension (mkSelector "constraintLessThanOrEqualToAnchor:multiplier:constant:") (retPtr retVoid) [argPtr (castPtr raw_anchor :: Ptr ()), argCDouble (fromIntegral m), argCDouble (fromIntegral c)] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @constraintEqualToConstant:@
constraintEqualToConstantSelector :: Selector
constraintEqualToConstantSelector = mkSelector "constraintEqualToConstant:"

-- | @Selector@ for @constraintGreaterThanOrEqualToConstant:@
constraintGreaterThanOrEqualToConstantSelector :: Selector
constraintGreaterThanOrEqualToConstantSelector = mkSelector "constraintGreaterThanOrEqualToConstant:"

-- | @Selector@ for @constraintLessThanOrEqualToConstant:@
constraintLessThanOrEqualToConstantSelector :: Selector
constraintLessThanOrEqualToConstantSelector = mkSelector "constraintLessThanOrEqualToConstant:"

-- | @Selector@ for @constraintEqualToAnchor:multiplier:@
constraintEqualToAnchor_multiplierSelector :: Selector
constraintEqualToAnchor_multiplierSelector = mkSelector "constraintEqualToAnchor:multiplier:"

-- | @Selector@ for @constraintGreaterThanOrEqualToAnchor:multiplier:@
constraintGreaterThanOrEqualToAnchor_multiplierSelector :: Selector
constraintGreaterThanOrEqualToAnchor_multiplierSelector = mkSelector "constraintGreaterThanOrEqualToAnchor:multiplier:"

-- | @Selector@ for @constraintLessThanOrEqualToAnchor:multiplier:@
constraintLessThanOrEqualToAnchor_multiplierSelector :: Selector
constraintLessThanOrEqualToAnchor_multiplierSelector = mkSelector "constraintLessThanOrEqualToAnchor:multiplier:"

-- | @Selector@ for @constraintEqualToAnchor:multiplier:constant:@
constraintEqualToAnchor_multiplier_constantSelector :: Selector
constraintEqualToAnchor_multiplier_constantSelector = mkSelector "constraintEqualToAnchor:multiplier:constant:"

-- | @Selector@ for @constraintGreaterThanOrEqualToAnchor:multiplier:constant:@
constraintGreaterThanOrEqualToAnchor_multiplier_constantSelector :: Selector
constraintGreaterThanOrEqualToAnchor_multiplier_constantSelector = mkSelector "constraintGreaterThanOrEqualToAnchor:multiplier:constant:"

-- | @Selector@ for @constraintLessThanOrEqualToAnchor:multiplier:constant:@
constraintLessThanOrEqualToAnchor_multiplier_constantSelector :: Selector
constraintLessThanOrEqualToAnchor_multiplier_constantSelector = mkSelector "constraintLessThanOrEqualToAnchor:multiplier:constant:"

