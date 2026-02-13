{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ URLFromPasteboard:@
urlFromPasteboard :: IsNSPasteboard pasteBoard => pasteBoard -> IO (Id NSURL)
urlFromPasteboard pasteBoard =
  do
    cls' <- getRequiredClass "NSURL"
    sendClassMessage cls' urlFromPasteboardSelector (toNSPasteboard pasteBoard)

-- | @- writeToPasteboard:@
writeToPasteboard :: (IsNSURL nsurl, IsNSPasteboard pasteBoard) => nsurl -> pasteBoard -> IO ()
writeToPasteboard nsurl pasteBoard =
  sendMessage nsurl writeToPasteboardSelector (toNSPasteboard pasteBoard)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @URLFromPasteboard:@
urlFromPasteboardSelector :: Selector '[Id NSPasteboard] (Id NSURL)
urlFromPasteboardSelector = mkSelector "URLFromPasteboard:"

-- | @Selector@ for @writeToPasteboard:@
writeToPasteboardSelector :: Selector '[Id NSPasteboard] ()
writeToPasteboardSelector = mkSelector "writeToPasteboard:"

