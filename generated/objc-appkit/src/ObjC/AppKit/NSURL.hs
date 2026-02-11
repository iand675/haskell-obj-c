{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSURL@.
module ObjC.AppKit.NSURL
  ( NSURL
  , IsNSURL(..)
  , urlFromPasteboard
  , writeToPasteboard
  , urlFromPasteboardSelector
  , writeToPasteboardSelector


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

-- | @+ URLFromPasteboard:@
urlFromPasteboard :: IsNSPasteboard pasteBoard => pasteBoard -> IO (Id NSURL)
urlFromPasteboard pasteBoard =
  do
    cls' <- getRequiredClass "NSURL"
    withObjCPtr pasteBoard $ \raw_pasteBoard ->
      sendClassMsg cls' (mkSelector "URLFromPasteboard:") (retPtr retVoid) [argPtr (castPtr raw_pasteBoard :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeToPasteboard:@
writeToPasteboard :: (IsNSURL nsurl, IsNSPasteboard pasteBoard) => nsurl -> pasteBoard -> IO ()
writeToPasteboard nsurl  pasteBoard =
withObjCPtr pasteBoard $ \raw_pasteBoard ->
    sendMsg nsurl (mkSelector "writeToPasteboard:") retVoid [argPtr (castPtr raw_pasteBoard :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @URLFromPasteboard:@
urlFromPasteboardSelector :: Selector
urlFromPasteboardSelector = mkSelector "URLFromPasteboard:"

-- | @Selector@ for @writeToPasteboard:@
writeToPasteboardSelector :: Selector
writeToPasteboardSelector = mkSelector "writeToPasteboard:"

