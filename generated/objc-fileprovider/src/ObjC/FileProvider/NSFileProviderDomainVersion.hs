{-# LANGUAGE PatternSynonyms #-}
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
  , nextSelector
  , compareSelector

  -- * Enum types
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending

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

import ObjC.FileProvider.Internal.Classes
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Build a version that is strictly greater than the receiver.
--
-- ObjC selector: @- next@
next :: IsNSFileProviderDomainVersion nsFileProviderDomainVersion => nsFileProviderDomainVersion -> IO (Id NSFileProviderDomainVersion)
next nsFileProviderDomainVersion  =
  sendMsg nsFileProviderDomainVersion (mkSelector "next") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Compare two domain versions.
--
-- This returns the NSComparisonResult of the comparison of the receiver and the other version:  - NSOrderedAscending if the receiver predates the otherVersion  - NSOrderedDescending if the otherVersion predates the receiver  - NSOrderedSame if both versions are equal
--
-- In Swift, NSFileProviderDomainVersion is comparable.
--
-- ObjC selector: @- compare:@
compare_ :: (IsNSFileProviderDomainVersion nsFileProviderDomainVersion, IsNSFileProviderDomainVersion otherVersion) => nsFileProviderDomainVersion -> otherVersion -> IO NSComparisonResult
compare_ nsFileProviderDomainVersion  otherVersion =
withObjCPtr otherVersion $ \raw_otherVersion ->
    fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg nsFileProviderDomainVersion (mkSelector "compare:") retCLong [argPtr (castPtr raw_otherVersion :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @next@
nextSelector :: Selector
nextSelector = mkSelector "next"

-- | @Selector@ for @compare:@
compareSelector :: Selector
compareSelector = mkSelector "compare:"

