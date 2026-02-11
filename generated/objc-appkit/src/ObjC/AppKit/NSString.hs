{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSString@.
module ObjC.AppKit.NSString
  ( NSString
  , IsNSString(..)
  , drawWithRect_options_attributes
  , boundingRectWithSize_options_attributes
  , drawWithRect_options_attributesSelector
  , boundingRectWithSize_options_attributesSelector

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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
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
drawWithRect_options_attributes nsString  rect options attributes =
withObjCPtr attributes $ \raw_attributes ->
    sendMsg nsString (mkSelector "drawWithRect:options:attributes:") retVoid [argNSRect rect, argCLong (coerce options), argPtr (castPtr raw_attributes :: Ptr ())]

-- | @- boundingRectWithSize:options:attributes:@
boundingRectWithSize_options_attributes :: (IsNSString nsString, IsNSDictionary attributes) => nsString -> NSSize -> NSStringDrawingOptions -> attributes -> IO NSRect
boundingRectWithSize_options_attributes nsString  size options attributes =
withObjCPtr attributes $ \raw_attributes ->
    sendMsgStret nsString (mkSelector "boundingRectWithSize:options:attributes:") retNSRect [argNSSize size, argCLong (coerce options), argPtr (castPtr raw_attributes :: Ptr ())]


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
drawWithRect_options_attributesSelector :: Selector
drawWithRect_options_attributesSelector = mkSelector "drawWithRect:options:attributes:"

-- | @Selector@ for @boundingRectWithSize:options:attributes:@
boundingRectWithSize_options_attributesSelector :: Selector
boundingRectWithSize_options_attributesSelector = mkSelector "boundingRectWithSize:options:attributes:"

