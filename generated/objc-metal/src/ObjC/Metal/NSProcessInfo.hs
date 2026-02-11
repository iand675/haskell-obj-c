{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSProcessInfo@.
module ObjC.Metal.NSProcessInfo
  ( NSProcessInfo
  , IsNSProcessInfo(..)
  , isDeviceCertifiedFor
  , hasPerformanceProfile
  , isDeviceCertifiedForSelector
  , hasPerformanceProfileSelector


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

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- isDeviceCertifiedFor:@
isDeviceCertifiedFor :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> CLong -> IO Bool
isDeviceCertifiedFor nsProcessInfo  performanceTier =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsProcessInfo (mkSelector "isDeviceCertifiedFor:") retCULong [argCLong (fromIntegral performanceTier)]

-- | @- hasPerformanceProfile:@
hasPerformanceProfile :: IsNSProcessInfo nsProcessInfo => nsProcessInfo -> CLong -> IO Bool
hasPerformanceProfile nsProcessInfo  performanceProfile =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsProcessInfo (mkSelector "hasPerformanceProfile:") retCULong [argCLong (fromIntegral performanceProfile)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isDeviceCertifiedFor:@
isDeviceCertifiedForSelector :: Selector
isDeviceCertifiedForSelector = mkSelector "isDeviceCertifiedFor:"

-- | @Selector@ for @hasPerformanceProfile:@
hasPerformanceProfileSelector :: Selector
hasPerformanceProfileSelector = mkSelector "hasPerformanceProfile:"

