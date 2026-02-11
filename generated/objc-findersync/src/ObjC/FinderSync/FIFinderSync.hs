{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A type to subclass to add badges, custom shortcut menus, and toolbar buttons to the Finder.
--
-- Subclass the FIFinderSync class when you want to customize the appearance of the Finder. Although the FIFinderSync class doesn’t provide any developer accessible API, it does adopt the ``FIFinderSyncProtocol`` protocol. This protocol declares methods you can implement to modify the appearance of the Finder. For more information on these methods, see ``FIFinderSyncProtocol``. To learn more about creating a Finder Sync extension, see [Finder Sync](https://developer.apple.com/library/archive/documentation/General/Conceptual/ExtensibilityPG/Finder.html#//apple_ref/doc/uid/TP40014214-CH15) in [App Extension Programming Guide](https://developer.apple.com/library/archive/documentation/General/Conceptual/ExtensibilityPG/index.html#//apple_ref/doc/uid/TP40014214).
--
-- ## See Also   - ``FinderSync/FIFinderSyncProtocol``
--
-- Generated bindings for @FIFinderSync@.
module ObjC.FinderSync.FIFinderSync
  ( FIFinderSync
  , IsFIFinderSync(..)


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

import ObjC.FinderSync.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

