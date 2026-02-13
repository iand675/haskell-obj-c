{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextLineFragment@.
module ObjC.AppKit.NSTextLineFragment
  ( NSTextLineFragment
  , IsNSTextLineFragment(..)
  , initWithAttributedString_range
  , initWithCoder
  , initWithString_attributes_range
  , init_
  , attributedString
  , characterRange
  , attributedStringSelector
  , characterRangeSelector
  , initSelector
  , initWithAttributedString_rangeSelector
  , initWithCoderSelector
  , initWithString_attributes_rangeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- initWithAttributedString:range:@
initWithAttributedString_range :: (IsNSTextLineFragment nsTextLineFragment, IsNSAttributedString attributedString) => nsTextLineFragment -> attributedString -> NSRange -> IO (Id NSTextLineFragment)
initWithAttributedString_range nsTextLineFragment attributedString range =
  sendOwnedMessage nsTextLineFragment initWithAttributedString_rangeSelector (toNSAttributedString attributedString) range

-- | @- initWithCoder:@
initWithCoder :: (IsNSTextLineFragment nsTextLineFragment, IsNSCoder aDecoder) => nsTextLineFragment -> aDecoder -> IO (Id NSTextLineFragment)
initWithCoder nsTextLineFragment aDecoder =
  sendOwnedMessage nsTextLineFragment initWithCoderSelector (toNSCoder aDecoder)

-- | @- initWithString:attributes:range:@
initWithString_attributes_range :: (IsNSTextLineFragment nsTextLineFragment, IsNSString string, IsNSDictionary attributes) => nsTextLineFragment -> string -> attributes -> NSRange -> IO (Id NSTextLineFragment)
initWithString_attributes_range nsTextLineFragment string attributes range =
  sendOwnedMessage nsTextLineFragment initWithString_attributes_rangeSelector (toNSString string) (toNSDictionary attributes) range

-- | @- init@
init_ :: IsNSTextLineFragment nsTextLineFragment => nsTextLineFragment -> IO (Id NSTextLineFragment)
init_ nsTextLineFragment =
  sendOwnedMessage nsTextLineFragment initSelector

-- | @- attributedString@
attributedString :: IsNSTextLineFragment nsTextLineFragment => nsTextLineFragment -> IO (Id NSAttributedString)
attributedString nsTextLineFragment =
  sendMessage nsTextLineFragment attributedStringSelector

-- | @- characterRange@
characterRange :: IsNSTextLineFragment nsTextLineFragment => nsTextLineFragment -> IO NSRange
characterRange nsTextLineFragment =
  sendMessage nsTextLineFragment characterRangeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAttributedString:range:@
initWithAttributedString_rangeSelector :: Selector '[Id NSAttributedString, NSRange] (Id NSTextLineFragment)
initWithAttributedString_rangeSelector = mkSelector "initWithAttributedString:range:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSTextLineFragment)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithString:attributes:range:@
initWithString_attributes_rangeSelector :: Selector '[Id NSString, Id NSDictionary, NSRange] (Id NSTextLineFragment)
initWithString_attributes_rangeSelector = mkSelector "initWithString:attributes:range:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSTextLineFragment)
initSelector = mkSelector "init"

-- | @Selector@ for @attributedString@
attributedStringSelector :: Selector '[] (Id NSAttributedString)
attributedStringSelector = mkSelector "attributedString"

-- | @Selector@ for @characterRange@
characterRangeSelector :: Selector '[] NSRange
characterRangeSelector = mkSelector "characterRange"

