{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAttributedStringMarkdownSourcePosition@.
module ObjC.Foundation.NSAttributedStringMarkdownSourcePosition
  ( NSAttributedStringMarkdownSourcePosition
  , IsNSAttributedStringMarkdownSourcePosition(..)
  , initWithStartLine_startColumn_endLine_endColumn
  , rangeInString
  , startLine
  , startColumn
  , endLine
  , endColumn
  , endColumnSelector
  , endLineSelector
  , initWithStartLine_startColumn_endLine_endColumnSelector
  , rangeInStringSelector
  , startColumnSelector
  , startLineSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs

-- | @- initWithStartLine:startColumn:endLine:endColumn:@
initWithStartLine_startColumn_endLine_endColumn :: IsNSAttributedStringMarkdownSourcePosition nsAttributedStringMarkdownSourcePosition => nsAttributedStringMarkdownSourcePosition -> CLong -> CLong -> CLong -> CLong -> IO (Id NSAttributedStringMarkdownSourcePosition)
initWithStartLine_startColumn_endLine_endColumn nsAttributedStringMarkdownSourcePosition startLine startColumn endLine endColumn =
  sendOwnedMessage nsAttributedStringMarkdownSourcePosition initWithStartLine_startColumn_endLine_endColumnSelector startLine startColumn endLine endColumn

-- | @- rangeInString:@
rangeInString :: (IsNSAttributedStringMarkdownSourcePosition nsAttributedStringMarkdownSourcePosition, IsNSString string) => nsAttributedStringMarkdownSourcePosition -> string -> IO NSRange
rangeInString nsAttributedStringMarkdownSourcePosition string =
  sendMessage nsAttributedStringMarkdownSourcePosition rangeInStringSelector (toNSString string)

-- | @- startLine@
startLine :: IsNSAttributedStringMarkdownSourcePosition nsAttributedStringMarkdownSourcePosition => nsAttributedStringMarkdownSourcePosition -> IO CLong
startLine nsAttributedStringMarkdownSourcePosition =
  sendMessage nsAttributedStringMarkdownSourcePosition startLineSelector

-- | @- startColumn@
startColumn :: IsNSAttributedStringMarkdownSourcePosition nsAttributedStringMarkdownSourcePosition => nsAttributedStringMarkdownSourcePosition -> IO CLong
startColumn nsAttributedStringMarkdownSourcePosition =
  sendMessage nsAttributedStringMarkdownSourcePosition startColumnSelector

-- | @- endLine@
endLine :: IsNSAttributedStringMarkdownSourcePosition nsAttributedStringMarkdownSourcePosition => nsAttributedStringMarkdownSourcePosition -> IO CLong
endLine nsAttributedStringMarkdownSourcePosition =
  sendMessage nsAttributedStringMarkdownSourcePosition endLineSelector

-- | @- endColumn@
endColumn :: IsNSAttributedStringMarkdownSourcePosition nsAttributedStringMarkdownSourcePosition => nsAttributedStringMarkdownSourcePosition -> IO CLong
endColumn nsAttributedStringMarkdownSourcePosition =
  sendMessage nsAttributedStringMarkdownSourcePosition endColumnSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithStartLine:startColumn:endLine:endColumn:@
initWithStartLine_startColumn_endLine_endColumnSelector :: Selector '[CLong, CLong, CLong, CLong] (Id NSAttributedStringMarkdownSourcePosition)
initWithStartLine_startColumn_endLine_endColumnSelector = mkSelector "initWithStartLine:startColumn:endLine:endColumn:"

-- | @Selector@ for @rangeInString:@
rangeInStringSelector :: Selector '[Id NSString] NSRange
rangeInStringSelector = mkSelector "rangeInString:"

-- | @Selector@ for @startLine@
startLineSelector :: Selector '[] CLong
startLineSelector = mkSelector "startLine"

-- | @Selector@ for @startColumn@
startColumnSelector :: Selector '[] CLong
startColumnSelector = mkSelector "startColumn"

-- | @Selector@ for @endLine@
endLineSelector :: Selector '[] CLong
endLineSelector = mkSelector "endLine"

-- | @Selector@ for @endColumn@
endColumnSelector :: Selector '[] CLong
endColumnSelector = mkSelector "endColumn"

