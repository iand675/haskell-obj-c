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
  , initWithStartLine_startColumn_endLine_endColumnSelector
  , rangeInStringSelector
  , startLineSelector
  , startColumnSelector
  , endLineSelector
  , endColumnSelector


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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Structs

-- | @- initWithStartLine:startColumn:endLine:endColumn:@
initWithStartLine_startColumn_endLine_endColumn :: IsNSAttributedStringMarkdownSourcePosition nsAttributedStringMarkdownSourcePosition => nsAttributedStringMarkdownSourcePosition -> CLong -> CLong -> CLong -> CLong -> IO (Id NSAttributedStringMarkdownSourcePosition)
initWithStartLine_startColumn_endLine_endColumn nsAttributedStringMarkdownSourcePosition  startLine startColumn endLine endColumn =
  sendMsg nsAttributedStringMarkdownSourcePosition (mkSelector "initWithStartLine:startColumn:endLine:endColumn:") (retPtr retVoid) [argCLong (fromIntegral startLine), argCLong (fromIntegral startColumn), argCLong (fromIntegral endLine), argCLong (fromIntegral endColumn)] >>= ownedObject . castPtr

-- | @- rangeInString:@
rangeInString :: (IsNSAttributedStringMarkdownSourcePosition nsAttributedStringMarkdownSourcePosition, IsNSString string) => nsAttributedStringMarkdownSourcePosition -> string -> IO NSRange
rangeInString nsAttributedStringMarkdownSourcePosition  string =
withObjCPtr string $ \raw_string ->
    sendMsgStret nsAttributedStringMarkdownSourcePosition (mkSelector "rangeInString:") retNSRange [argPtr (castPtr raw_string :: Ptr ())]

-- | @- startLine@
startLine :: IsNSAttributedStringMarkdownSourcePosition nsAttributedStringMarkdownSourcePosition => nsAttributedStringMarkdownSourcePosition -> IO CLong
startLine nsAttributedStringMarkdownSourcePosition  =
  sendMsg nsAttributedStringMarkdownSourcePosition (mkSelector "startLine") retCLong []

-- | @- startColumn@
startColumn :: IsNSAttributedStringMarkdownSourcePosition nsAttributedStringMarkdownSourcePosition => nsAttributedStringMarkdownSourcePosition -> IO CLong
startColumn nsAttributedStringMarkdownSourcePosition  =
  sendMsg nsAttributedStringMarkdownSourcePosition (mkSelector "startColumn") retCLong []

-- | @- endLine@
endLine :: IsNSAttributedStringMarkdownSourcePosition nsAttributedStringMarkdownSourcePosition => nsAttributedStringMarkdownSourcePosition -> IO CLong
endLine nsAttributedStringMarkdownSourcePosition  =
  sendMsg nsAttributedStringMarkdownSourcePosition (mkSelector "endLine") retCLong []

-- | @- endColumn@
endColumn :: IsNSAttributedStringMarkdownSourcePosition nsAttributedStringMarkdownSourcePosition => nsAttributedStringMarkdownSourcePosition -> IO CLong
endColumn nsAttributedStringMarkdownSourcePosition  =
  sendMsg nsAttributedStringMarkdownSourcePosition (mkSelector "endColumn") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithStartLine:startColumn:endLine:endColumn:@
initWithStartLine_startColumn_endLine_endColumnSelector :: Selector
initWithStartLine_startColumn_endLine_endColumnSelector = mkSelector "initWithStartLine:startColumn:endLine:endColumn:"

-- | @Selector@ for @rangeInString:@
rangeInStringSelector :: Selector
rangeInStringSelector = mkSelector "rangeInString:"

-- | @Selector@ for @startLine@
startLineSelector :: Selector
startLineSelector = mkSelector "startLine"

-- | @Selector@ for @startColumn@
startColumnSelector :: Selector
startColumnSelector = mkSelector "startColumn"

-- | @Selector@ for @endLine@
endLineSelector :: Selector
endLineSelector = mkSelector "endLine"

-- | @Selector@ for @endColumn@
endColumnSelector :: Selector
endColumnSelector = mkSelector "endColumn"

