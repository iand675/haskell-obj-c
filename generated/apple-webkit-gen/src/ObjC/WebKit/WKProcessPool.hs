{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A WKProcessPool object represents a pool of web content processes. The process pool associated with a web view is specified by its web view configuration. Each web view is given its own web content process until an implementation-defined process limit is reached; after that, web views with the same process pool end up sharing web content processes.
--
-- Generated bindings for @WKProcessPool@.
module ObjC.WebKit.WKProcessPool
  ( WKProcessPool
  , IsWKProcessPool(..)


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

