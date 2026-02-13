{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXDiskWriteExceptionDiagnostic
--
-- An MXDiagnostic subclass that encapsulates disk write exception reports.
--
-- Disk write exceptions occur when your application writes data excessively to disk.
--
-- Generated bindings for @MXDiskWriteExceptionDiagnostic@.
module ObjC.MetricKit.MXDiskWriteExceptionDiagnostic
  ( MXDiskWriteExceptionDiagnostic
  , IsMXDiskWriteExceptionDiagnostic(..)
  , callStackTree
  , totalWritesCaused
  , callStackTreeSelector
  , totalWritesCausedSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | callStackTree
--
-- The application call stack tree associated with the excessive disk writes.
--
-- ObjC selector: @- callStackTree@
callStackTree :: IsMXDiskWriteExceptionDiagnostic mxDiskWriteExceptionDiagnostic => mxDiskWriteExceptionDiagnostic -> IO (Id MXCallStackTree)
callStackTree mxDiskWriteExceptionDiagnostic =
  sendMessage mxDiskWriteExceptionDiagnostic callStackTreeSelector

-- | totalWritesCaused
--
-- Total disk writes caused in the scope of this disk write exception.
--
-- Dimensioned as NSUnitInformationStorage.
--
-- ObjC selector: @- totalWritesCaused@
totalWritesCaused :: IsMXDiskWriteExceptionDiagnostic mxDiskWriteExceptionDiagnostic => mxDiskWriteExceptionDiagnostic -> IO (Id NSMeasurement)
totalWritesCaused mxDiskWriteExceptionDiagnostic =
  sendMessage mxDiskWriteExceptionDiagnostic totalWritesCausedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @callStackTree@
callStackTreeSelector :: Selector '[] (Id MXCallStackTree)
callStackTreeSelector = mkSelector "callStackTree"

-- | @Selector@ for @totalWritesCaused@
totalWritesCausedSelector :: Selector '[] (Id NSMeasurement)
totalWritesCausedSelector = mkSelector "totalWritesCaused"

