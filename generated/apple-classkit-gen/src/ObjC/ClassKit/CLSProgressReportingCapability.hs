{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This class specifies progress reporting capability of a ClassKit client app
--
-- Generated bindings for @CLSProgressReportingCapability@.
module ObjC.ClassKit.CLSProgressReportingCapability
  ( CLSProgressReportingCapability
  , IsCLSProgressReportingCapability(..)
  , initWithKind_details
  , kind
  , details
  , detailsSelector
  , initWithKind_detailsSelector
  , kindSelector

  -- * Enum types
  , CLSProgressReportingCapabilityKind(CLSProgressReportingCapabilityKind)
  , pattern CLSProgressReportingCapabilityKindDuration
  , pattern CLSProgressReportingCapabilityKindPercent
  , pattern CLSProgressReportingCapabilityKindBinary
  , pattern CLSProgressReportingCapabilityKindQuantity
  , pattern CLSProgressReportingCapabilityKindScore

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ClassKit.Internal.Classes
import ObjC.ClassKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initialize and configure the type of progress reporting capability
--
-- @kind@ — The kind of progress reporting capability
--
-- @details@ — An optional localized string describing the capability. For example: "Reports percentage of progress", "Reports overall score". Schoolwork will use an appropriate default string if one is not provided.
--
-- ObjC selector: @- initWithKind:details:@
initWithKind_details :: (IsCLSProgressReportingCapability clsProgressReportingCapability, IsNSString details) => clsProgressReportingCapability -> CLSProgressReportingCapabilityKind -> details -> IO (Id CLSProgressReportingCapability)
initWithKind_details clsProgressReportingCapability kind details =
  sendOwnedMessage clsProgressReportingCapability initWithKind_detailsSelector kind (toNSString details)

-- | Returns the kind of progress reporting capability
--
-- ObjC selector: @- kind@
kind :: IsCLSProgressReportingCapability clsProgressReportingCapability => clsProgressReportingCapability -> IO CLSProgressReportingCapabilityKind
kind clsProgressReportingCapability =
  sendMessage clsProgressReportingCapability kindSelector

-- | Returns progress reporting details
--
-- ObjC selector: @- details@
details :: IsCLSProgressReportingCapability clsProgressReportingCapability => clsProgressReportingCapability -> IO (Id NSString)
details clsProgressReportingCapability =
  sendMessage clsProgressReportingCapability detailsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithKind:details:@
initWithKind_detailsSelector :: Selector '[CLSProgressReportingCapabilityKind, Id NSString] (Id CLSProgressReportingCapability)
initWithKind_detailsSelector = mkSelector "initWithKind:details:"

-- | @Selector@ for @kind@
kindSelector :: Selector '[] CLSProgressReportingCapabilityKind
kindSelector = mkSelector "kind"

-- | @Selector@ for @details@
detailsSelector :: Selector '[] (Id NSString)
detailsSelector = mkSelector "details"

