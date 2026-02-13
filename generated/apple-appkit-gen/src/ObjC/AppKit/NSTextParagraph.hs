{-# LANGUAGE DataKinds #-}
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
  , attributedStringSelector
  , initWithAttributedStringSelector
  , paragraphContentRangeSelector
  , paragraphSeparatorRangeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithAttributedString:@
initWithAttributedString :: (IsNSTextParagraph nsTextParagraph, IsNSAttributedString attributedString) => nsTextParagraph -> attributedString -> IO (Id NSTextParagraph)
initWithAttributedString nsTextParagraph attributedString =
  sendOwnedMessage nsTextParagraph initWithAttributedStringSelector (toNSAttributedString attributedString)

-- | @- attributedString@
attributedString :: IsNSTextParagraph nsTextParagraph => nsTextParagraph -> IO (Id NSAttributedString)
attributedString nsTextParagraph =
  sendMessage nsTextParagraph attributedStringSelector

-- | @- paragraphContentRange@
paragraphContentRange :: IsNSTextParagraph nsTextParagraph => nsTextParagraph -> IO (Id NSTextRange)
paragraphContentRange nsTextParagraph =
  sendMessage nsTextParagraph paragraphContentRangeSelector

-- | @- paragraphSeparatorRange@
paragraphSeparatorRange :: IsNSTextParagraph nsTextParagraph => nsTextParagraph -> IO (Id NSTextRange)
paragraphSeparatorRange nsTextParagraph =
  sendMessage nsTextParagraph paragraphSeparatorRangeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithAttributedString:@
initWithAttributedStringSelector :: Selector '[Id NSAttributedString] (Id NSTextParagraph)
initWithAttributedStringSelector = mkSelector "initWithAttributedString:"

-- | @Selector@ for @attributedString@
attributedStringSelector :: Selector '[] (Id NSAttributedString)
attributedStringSelector = mkSelector "attributedString"

-- | @Selector@ for @paragraphContentRange@
paragraphContentRangeSelector :: Selector '[] (Id NSTextRange)
paragraphContentRangeSelector = mkSelector "paragraphContentRange"

-- | @Selector@ for @paragraphSeparatorRange@
paragraphSeparatorRangeSelector :: Selector '[] (Id NSTextRange)
paragraphSeparatorRangeSelector = mkSelector "paragraphSeparatorRange"

