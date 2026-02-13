{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An abstract base class for implementing a full-featured file system.
--
-- @FSFileSystem@ is a full-featured file system, which works with one or more ``FSResource`` instances and presents one or more ``FSVolume`` references to callers.
--
-- Implement your app extension by providing a subclass of @FSFileSystem@ as a delegate object. Your delegate also needs to implement the @FSFileSystemOperations@ protocol so that it can probe, load, and unload resources.
--
-- > Note: The current version of FSKit supports only ``FSUnaryFileSystem``, not @FSFileSystem@.
--
-- Generated bindings for @FSFileSystem@.
module ObjC.FSKit.FSFileSystem
  ( FSFileSystem
  , IsFSFileSystem(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FSKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

