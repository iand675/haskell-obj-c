{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | WebDownload
--
-- A WebDownload works just like an NSURLDownload, with    one extra feature: if you do not implement the    authentication-related delegate methods, it will automatically    prompt for authentication using the standard WebKit authentication    panel, as either a sheet or window. It provides no extra methods,    but does have one additional delegate method.
--
-- Generated bindings for @WebDownload@.
module ObjC.WebKit.WebDownload
  ( WebDownload
  , IsWebDownload(..)


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

