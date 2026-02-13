{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSString@.
module ObjC.AppKit.NSString
  ( NSString
  , IsNSString(..)
  , drawWithRect_options_attributes
  , boundingRectWithSize_options_attributes
  , boundingRectWithSize_options_attributesSelector
  , drawWithRect_options_attributesSelector

  -- * Enum types
  , NSStringDrawingOptions(NSStringDrawingOptions)
  , pattern NSStringDrawingUsesLineFragmentOrigin
  , pattern NSStringDrawingUsesFontLeading
  , pattern NSStringDrawingUsesDeviceMetrics
  , pattern NSStringDrawingTruncatesLastVisibleLine
  , pattern NSStringDrawingOptionsResolvesNaturalAlignmentWithBaseWritingDirection
  , pattern NSStringDrawingDisableScreenFontSubstitution
  , pattern NSStringDrawingOneShot

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes
import Data.String (IsString(..))
import ObjC.Runtime.NSString (pureNSString)

-- | @- drawWithRect:options:attributes:@
drawWithRect_options_attributes :: (IsNSString nsString, IsNSDictionary attributes) => nsString -> NSRect -> NSStringDrawingOptions -> attributes -> IO ()
drawWithRect_options_attributes nsString rect options attributes =
  sendMessage nsString drawWithRect_options_attributesSelector rect options (toNSDictionary attributes)

-- | @- boundingRectWithSize:options:attributes:@
boundingRectWithSize_options_attributes :: (IsNSString nsString, IsNSDictionary attributes) => nsString -> NSSize -> NSStringDrawingOptions -> attributes -> IO NSRect
boundingRectWithSize_options_attributes nsString size options attributes =
  sendMessage nsString boundingRectWithSize_options_attributesSelector size options (toNSDictionary attributes)


-- | Allows using @OverloadedStrings@ for @Id NSString@.
--
-- >>> :set -XOverloadedStrings
-- >>> let s = "hello" :: Id NSString
instance IsString (Id NSString) where
  fromString = pureNSString
-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @drawWithRect:options:attributes:@
drawWithRect_options_attributesSelector :: Selector '[NSRect, NSStringDrawingOptions, Id NSDictionary] ()
drawWithRect_options_attributesSelector = mkSelector "drawWithRect:options:attributes:"

-- | @Selector@ for @boundingRectWithSize:options:attributes:@
boundingRectWithSize_options_attributesSelector :: Selector '[NSSize, NSStringDrawingOptions, Id NSDictionary] NSRect
boundingRectWithSize_options_attributesSelector = mkSelector "boundingRectWithSize:options:attributes:"

