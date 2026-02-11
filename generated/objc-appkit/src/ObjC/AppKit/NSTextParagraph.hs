{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextParagraph@.
module ObjC.AppKit.NSTextParagraph
  ( NSTextParagraph
  , IsNSTextParagraph(..)
  , initWithAttributedString
  , attributedString
  , paragraphContentRange
  , paragraphSeparatorRange
  , initWithAttributedStringSelector
  , attributedStringSelector
  , paragraphContentRangeSelector
  , paragraphSeparatorRangeSelector


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

-- | @- initWithAttributedString:@
initWithAttributedString :: (IsNSTextParagraph nsTextParagraph, IsNSAttributedString attributedString) => nsTextParagraph -> attributedString -> IO (Id NSTextParagraph)
initWithAttributedString nsTextParagraph  attributedString =
withObjCPtr attributedString $ \raw_attributedString ->
    sendMsg nsTextParagraph (mkSelector "initWithAttributedString:") (retPtr retVoid) [argPtr (castPtr raw_attributedString :: Ptr ())] >>= ownedObject . castPtr

-- | @- attributedString@
attributedString :: IsNSTextParagraph nsTextParagraph => nsTextParagraph -> IO (Id NSAttributedString)
attributedString nsTextParagraph  =
  sendMsg nsTextParagraph (mkSelector "attributedString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- paragraphContentRange@
paragraphContentRange :: IsNSTextParagraph nsTextParagraph => nsTextParagraph -> IO (Id NSTextRange)
paragraphContentRange nsTextParagraph  =
  sendMsg nsTextParagraph (mkSelector "paragraphContentRange") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- paragraphSeparatorRange@
paragraphSeparatorRange :: IsNSTextParagraph nsTextParagraph => nsTextParagraph -> IO (Id NSTextRange)
paragraphSeparatorRange nsTextParagraph  =
  sendMsg nsTextParagraph (mkSelector "paragraphSeparatorRange") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAttributedString:@
initWithAttributedStringSelector :: Selector
initWithAttributedStringSelector = mkSelector "initWithAttributedString:"

-- | @Selector@ for @attributedString@
attributedStringSelector :: Selector
attributedStringSelector = mkSelector "attributedString"

-- | @Selector@ for @paragraphContentRange@
paragraphContentRangeSelector :: Selector
paragraphContentRangeSelector = mkSelector "paragraphContentRange"

-- | @Selector@ for @paragraphSeparatorRange@
paragraphSeparatorRangeSelector :: Selector
paragraphSeparatorRangeSelector = mkSelector "paragraphSeparatorRange"

