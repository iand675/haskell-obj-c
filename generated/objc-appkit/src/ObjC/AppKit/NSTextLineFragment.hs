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
  , initWithAttributedString_rangeSelector
  , initWithCoderSelector
  , initWithString_attributes_rangeSelector
  , initSelector
  , attributedStringSelector
  , characterRangeSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- initWithAttributedString:range:@
initWithAttributedString_range :: (IsNSTextLineFragment nsTextLineFragment, IsNSAttributedString attributedString) => nsTextLineFragment -> attributedString -> NSRange -> IO (Id NSTextLineFragment)
initWithAttributedString_range nsTextLineFragment  attributedString range =
withObjCPtr attributedString $ \raw_attributedString ->
    sendMsg nsTextLineFragment (mkSelector "initWithAttributedString:range:") (retPtr retVoid) [argPtr (castPtr raw_attributedString :: Ptr ()), argNSRange range] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSTextLineFragment nsTextLineFragment, IsNSCoder aDecoder) => nsTextLineFragment -> aDecoder -> IO (Id NSTextLineFragment)
initWithCoder nsTextLineFragment  aDecoder =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg nsTextLineFragment (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithString:attributes:range:@
initWithString_attributes_range :: (IsNSTextLineFragment nsTextLineFragment, IsNSString string, IsNSDictionary attributes) => nsTextLineFragment -> string -> attributes -> NSRange -> IO (Id NSTextLineFragment)
initWithString_attributes_range nsTextLineFragment  string attributes range =
withObjCPtr string $ \raw_string ->
  withObjCPtr attributes $ \raw_attributes ->
      sendMsg nsTextLineFragment (mkSelector "initWithString:attributes:range:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argPtr (castPtr raw_attributes :: Ptr ()), argNSRange range] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSTextLineFragment nsTextLineFragment => nsTextLineFragment -> IO (Id NSTextLineFragment)
init_ nsTextLineFragment  =
  sendMsg nsTextLineFragment (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- attributedString@
attributedString :: IsNSTextLineFragment nsTextLineFragment => nsTextLineFragment -> IO (Id NSAttributedString)
attributedString nsTextLineFragment  =
  sendMsg nsTextLineFragment (mkSelector "attributedString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- characterRange@
characterRange :: IsNSTextLineFragment nsTextLineFragment => nsTextLineFragment -> IO NSRange
characterRange nsTextLineFragment  =
  sendMsgStret nsTextLineFragment (mkSelector "characterRange") retNSRange []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAttributedString:range:@
initWithAttributedString_rangeSelector :: Selector
initWithAttributedString_rangeSelector = mkSelector "initWithAttributedString:range:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithString:attributes:range:@
initWithString_attributes_rangeSelector :: Selector
initWithString_attributes_rangeSelector = mkSelector "initWithString:attributes:range:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @attributedString@
attributedStringSelector :: Selector
attributedStringSelector = mkSelector "attributedString"

-- | @Selector@ for @characterRange@
characterRangeSelector :: Selector
characterRangeSelector = mkSelector "characterRange"

