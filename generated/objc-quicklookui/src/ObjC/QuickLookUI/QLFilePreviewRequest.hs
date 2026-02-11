{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This class contains information about the preview that should be provided.
--
-- Generated bindings for @QLFilePreviewRequest@.
module ObjC.QuickLookUI.QLFilePreviewRequest
  ( QLFilePreviewRequest
  , IsQLFilePreviewRequest(..)
  , fileURL
  , fileURLSelector


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

import ObjC.QuickLookUI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The url of the file for which a preview is being requested.
--
-- ObjC selector: @- fileURL@
fileURL :: IsQLFilePreviewRequest qlFilePreviewRequest => qlFilePreviewRequest -> IO (Id NSURL)
fileURL qlFilePreviewRequest  =
  sendMsg qlFilePreviewRequest (mkSelector "fileURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fileURL@
fileURLSelector :: Selector
fileURLSelector = mkSelector "fileURL"

