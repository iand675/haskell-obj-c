{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

