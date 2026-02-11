{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.InstallerPlugins.Internal.Classes (
    module ObjC.InstallerPlugins.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- InstallerPane ----------

-- | Phantom type for @InstallerPane@.
data InstallerPane

instance IsObjCObject (Id InstallerPane) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "InstallerPane"

class IsNSObject a => IsInstallerPane a where
  toInstallerPane :: a -> Id InstallerPane

instance IsInstallerPane (Id InstallerPane) where
  toInstallerPane = unsafeCastId

instance IsNSObject (Id InstallerPane) where
  toNSObject = unsafeCastId

-- ---------- InstallerSection ----------

-- | Phantom type for @InstallerSection@.
data InstallerSection

instance IsObjCObject (Id InstallerSection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "InstallerSection"

class IsNSObject a => IsInstallerSection a where
  toInstallerSection :: a -> Id InstallerSection

instance IsInstallerSection (Id InstallerSection) where
  toInstallerSection = unsafeCastId

instance IsNSObject (Id InstallerSection) where
  toNSObject = unsafeCastId

-- ---------- InstallerState ----------

-- | InstallerSection
--
-- An object representing a specific Section of the Installer's UI.
--
-- The InstallerSection class declares an interface for a section within the Installer's UI.
--
-- The InstallerSection is the main controller of this section and contains all the panes which				are actually displayed in the Installer's UI.  The InstallerSection itself does not display				anything, but rather provides the InstallerPanes which do.
--
-- Each InstallerSection (or subclass) must be within its own bundle.  The NSPrincipalClass for				this bundle must be specified as an InstallerSection or a subclass.
--
-- Typically an InstallerSection is not subclassed because most of the functionality can be				provided through the Info.plist and the default nib.
--
-- The default nib for a section is specified by the NSMainNibFile key in the Info.plist				for the section's bundle.
--
-- The title for the section is specified by the InstallerSectionTitle key.
-- 
-- Phantom type for @InstallerState@.
data InstallerState

instance IsObjCObject (Id InstallerState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "InstallerState"

class IsNSObject a => IsInstallerState a where
  toInstallerState :: a -> Id InstallerState

instance IsInstallerState (Id InstallerState) where
  toInstallerState = unsafeCastId

instance IsNSObject (Id InstallerState) where
  toNSObject = unsafeCastId
