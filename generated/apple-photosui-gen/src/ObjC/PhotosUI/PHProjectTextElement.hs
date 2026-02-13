{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , attributedTextSelector
  , textElementTypeSelector
  , textSelector

  -- * Enum types
  , PHProjectTextElementType(PHProjectTextElementType)
  , pattern PHProjectTextElementTypeBody
  , pattern PHProjectTextElementTypeTitle
  , pattern PHProjectTextElementTypeSubtitle

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PhotosUI.Internal.Classes
import ObjC.PhotosUI.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Unformatted, raw string for the text element
--
-- ObjC selector: @- text@
text :: IsPHProjectTextElement phProjectTextElement => phProjectTextElement -> IO (Id NSString)
text phProjectTextElement =
  sendMessage phProjectTextElement textSelector

-- | If the text was presented to the user in a stylized manner in Photos, attributedText will provide access to those same attributes.
--
-- ObjC selector: @- attributedText@
attributedText :: IsPHProjectTextElement phProjectTextElement => phProjectTextElement -> IO (Id NSAttributedString)
attributedText phProjectTextElement =
  sendMessage phProjectTextElement attributedTextSelector

-- | @- textElementType@
textElementType :: IsPHProjectTextElement phProjectTextElement => phProjectTextElement -> IO PHProjectTextElementType
textElementType phProjectTextElement =
  sendMessage phProjectTextElement textElementTypeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @text@
textSelector :: Selector '[] (Id NSString)
textSelector = mkSelector "text"

-- | @Selector@ for @attributedText@
attributedTextSelector :: Selector '[] (Id NSAttributedString)
attributedTextSelector = mkSelector "attributedText"

-- | @Selector@ for @textElementType@
textElementTypeSelector :: Selector '[] PHProjectTextElementType
textElementTypeSelector = mkSelector "textElementType"

