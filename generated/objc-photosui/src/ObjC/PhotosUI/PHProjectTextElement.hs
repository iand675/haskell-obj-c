{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A PHProjectTextElement object represents formatted, positioned text that should be considered for inclusion in a project. In this case of a Memory, this will always be the Title and Subtitle show in the Memory header view. For projects created from Apple Book, Card, and Calendar projects, text appearing on any page.
--
-- Generated bindings for @PHProjectTextElement@.
module ObjC.PhotosUI.PHProjectTextElement
  ( PHProjectTextElement
  , IsPHProjectTextElement(..)
  , text
  , attributedText
  , textElementType
  , textSelector
  , attributedTextSelector
  , textElementTypeSelector

  -- * Enum types
  , PHProjectTextElementType(PHProjectTextElementType)
  , pattern PHProjectTextElementTypeBody
  , pattern PHProjectTextElementTypeTitle
  , pattern PHProjectTextElementTypeSubtitle

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

import ObjC.PhotosUI.Internal.Classes
import ObjC.PhotosUI.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Unformatted, raw string for the text element
--
-- ObjC selector: @- text@
text :: IsPHProjectTextElement phProjectTextElement => phProjectTextElement -> IO (Id NSString)
text phProjectTextElement  =
  sendMsg phProjectTextElement (mkSelector "text") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If the text was presented to the user in a stylized manner in Photos, attributedText will provide access to those same attributes.
--
-- ObjC selector: @- attributedText@
attributedText :: IsPHProjectTextElement phProjectTextElement => phProjectTextElement -> IO (Id NSAttributedString)
attributedText phProjectTextElement  =
  sendMsg phProjectTextElement (mkSelector "attributedText") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- textElementType@
textElementType :: IsPHProjectTextElement phProjectTextElement => phProjectTextElement -> IO PHProjectTextElementType
textElementType phProjectTextElement  =
  fmap (coerce :: CLong -> PHProjectTextElementType) $ sendMsg phProjectTextElement (mkSelector "textElementType") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @text@
textSelector :: Selector
textSelector = mkSelector "text"

-- | @Selector@ for @attributedText@
attributedTextSelector :: Selector
attributedTextSelector = mkSelector "attributedText"

-- | @Selector@ for @textElementType@
textElementTypeSelector :: Selector
textElementTypeSelector = mkSelector "textElementType"

