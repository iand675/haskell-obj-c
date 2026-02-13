{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A distinct object in a file hierarchy, such as a file, directory, symlink, socket, and more.
--
-- An @FSItem@ is a mostly opaque object, which your file system implementation defines as needed.
--
-- The ``FSItemAttributes`` class defines nonatomic properties to support @FSItem@ instances. An ``FSItemAttributes`` instance contains a snapshot of the attributes of an @FSItem@ at one point in time. The ``FSItemAttributes`` properties have no explicit thread safety provisions, since the operations that either get or set these properties enforce thread safety.
--
-- You test an attribute's validity with the the method ``FSItem/Attributes/isValid(_:)``. If the value is @true@ (Swift) or @YES@ (Objective-C), it's safe to use the attribute.
--
-- Methods that get or set an item's attribute use ``FSItemGetAttributesRequest`` or ``FSItemSetAttributesRequest``, respectively. Both are subclasses of ``FSItemAttributes``. An ``FSItemGetAttributesRequest`` contains a ``FSItemGetAttributesRequest/wantedAttributes`` property to indicate the attributes a file system provides for the request. Similarly, ``FSItemSetAttributesRequest`` uses the property ``FSItemSetAttributesRequest/consumedAttributes`` for a file system to signal back which attributes it successfully used.
--
-- @FSItem@ is the FSKit equivelant of a vnode in the kernel. For every FSKit vnode in the kernel, the @FSModule@ hosting the volume has an instantiated @FSItem@.
--
-- Generated bindings for @FSItem@.
module ObjC.FSKit.FSItem
  ( FSItem
  , IsFSItem(..)


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

