{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An abstract base class for implementing a minimal file system.
--
-- @FSUnaryFileSystem@ is a simplified file system, which works with one ``FSResource`` and presents it as one ``FSVolume``.
--
-- The one volume and its container have a shared state and lifetime, a more constrained life cycle than the ``FSFileSystem`` design flow.
--
-- Implement your app extension by providing a subclass of @FSUnaryFileSystem@ as a delegate object. Your delegate also needs to implement the ``FSUnaryFileSystemOperations`` protocol so that it can load resources.
--
-- Generated bindings for @FSUnaryFileSystem@.
module ObjC.FSKit.FSUnaryFileSystem
  ( FSUnaryFileSystem
  , IsFSUnaryFileSystem(..)


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

