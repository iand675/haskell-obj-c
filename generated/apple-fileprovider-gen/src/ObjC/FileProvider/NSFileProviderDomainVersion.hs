{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | File provider domain version.
--
-- This object can be used by the @NSFileProviderReplicatedExtension@ to describe the current version of the domain. This object is immutable and can safely be used as a key in a dictionary.
--
-- Generated bindings for @NSFileProviderDomainVersion@.
module ObjC.FileProvider.NSFileProviderDomainVersion
  ( NSFileProviderDomainVersion
  , IsNSFileProviderDomainVersion(..)
  , next
  , compare_
  , compareSelector
  , nextSelector

  -- * Enum types
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FileProvider.Internal.Classes
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Build a version that is strictly greater than the receiver.
--
-- ObjC selector: @- next@
next :: IsNSFileProviderDomainVersion nsFileProviderDomainVersion => nsFileProviderDomainVersion -> IO (Id NSFileProviderDomainVersion)
next nsFileProviderDomainVersion =
  sendMessage nsFileProviderDomainVersion nextSelector

-- | Compare two domain versions.
--
-- This returns the NSComparisonResult of the comparison of the receiver and the other version:  - NSOrderedAscending if the receiver predates the otherVersion  - NSOrderedDescending if the otherVersion predates the receiver  - NSOrderedSame if both versions are equal
--
-- In Swift, NSFileProviderDomainVersion is comparable.
--
-- ObjC selector: @- compare:@
compare_ :: (IsNSFileProviderDomainVersion nsFileProviderDomainVersion, IsNSFileProviderDomainVersion otherVersion) => nsFileProviderDomainVersion -> otherVersion -> IO NSComparisonResult
compare_ nsFileProviderDomainVersion otherVersion =
  sendMessage nsFileProviderDomainVersion compareSelector (toNSFileProviderDomainVersion otherVersion)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @next@
nextSelector :: Selector '[] (Id NSFileProviderDomainVersion)
nextSelector = mkSelector "next"

-- | @Selector@ for @compare:@
compareSelector :: Selector '[Id NSFileProviderDomainVersion] NSComparisonResult
compareSelector = mkSelector "compare:"

