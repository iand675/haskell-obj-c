{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSProcessInfo@.
module ObjC.Metal.NSProcessInfo
  ( NSProcessInfo
  , IsNSProcessInfo(..)
  , isDeviceCertifiedFor
  , hasPerformanceProfile
  , hasPerformanceProfileSelector
  , isDeviceCertifiedForSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- isDeviceCertifiedFor:@
isDeviceCertifiedFor :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> CLong -> IO Bool
isDeviceCertifiedFor nsProcessInfo performanceTier =
  sendMessage nsProcessInfo isDeviceCertifiedForSelector performanceTier

-- | @- hasPerformanceProfile:@
hasPerformanceProfile :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> CLong -> IO Bool
hasPerformanceProfile nsProcessInfo performanceProfile =
  sendMessage nsProcessInfo hasPerformanceProfileSelector performanceProfile

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isDeviceCertifiedFor:@
isDeviceCertifiedForSelector :: Selector '[CLong] Bool
isDeviceCertifiedForSelector = mkSelector "isDeviceCertifiedFor:"

-- | @Selector@ for @hasPerformanceProfile:@
hasPerformanceProfileSelector :: Selector '[CLong] Bool
hasPerformanceProfileSelector = mkSelector "hasPerformanceProfile:"

